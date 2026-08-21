#' Check the structural integrity of a Parquet file.
#'
#' @description
#' Detect Parquet files whose footer describes a layout the file
#' itself does not have. The motivating failure is `nanoparquet`
#' before 0.5.0, which stored column-chunk file offsets and sizes as
#' 32-bit integers: past 4 GiB (2^32 bytes) they wrapped around, so a
#' multi-gigabyte cube received a footer that still declared every row
#' group and row but pointed at the wrong bytes for most of them. A
#' reader then returns the first ~4 GiB and throws thrift
#' `"Deserializing page header failed"` on everything after it, which a
#' consumer that does not read row group by row group never sees: it
#' silently gets truncated data (whep#531).
#'
#' The default check is metadata-only and therefore costs milliseconds
#' even on a 15 GB file. Column chunks are written contiguously in
#' `(row_group, column)` order by every mainstream writer, so their
#' byte ranges must not overlap and must stay inside the file. An
#' offset that jumps backwards is the signature of the 32-bit
#' wraparound. The upper bound is the start of the footer rather than
#' the end of the file, so a chunk that runs into the metadata is
#' caught too.
#'
#' `deep = TRUE` additionally decodes every row group with `arrow`,
#' which is exact rather than structural but reads the whole file.
#'
#' @param path Path to a Parquet file.
#' @param deep Whether to also read every row group and report the
#'   ones that fail to decode. Exact, but reads the entire file.
#'
#' @return A tibble with one row per anomaly, empty when the file is
#'   sound:
#'   - `row_group`: Zero-based row group index.
#'   - `column`: Zero-based column index, `NA` for whole-row-group
#'     problems found by `deep = TRUE`.
#'   - `issue`: One of `"offset_overlap"` (a chunk starts before the
#'     previous chunk ends, i.e. offsets are not monotonic),
#'     `"offset_past_data"` (a chunk ends beyond the last byte the
#'     data section can hold)
#'     or `"row_group_unreadable"` (`deep = TRUE` only).
#'   - `chunk_start`, `chunk_end`: Byte range the footer claims for
#'     the chunk.
#'   - `detail`: Human-readable context, e.g. the reader's error.
#'
#' @export
#'
#' @examples
#' path <- tempfile(fileext = ".parquet")
#' nanoparquet::write_parquet(data.frame(x = 1:10), path)
#' check_parquet_integrity(path)
#' unlink(path)
check_parquet_integrity <- function(path, deep = FALSE) {
  if (!file.exists(path)) {
    cli::cli_abort("Parquet file not found: {.path {path}}.")
  }
  chunks <- nanoparquet::read_parquet_metadata(path)$column_chunks
  anomalies <- .parquet_offset_anomalies(chunks, .parquet_data_end(path))
  if (isTRUE(deep)) {
    anomalies <- dplyr::bind_rows(anomalies, .parquet_read_anomalies(path))
  }
  anomalies
}

#' Assert that a Parquet file is structurally sound.
#'
#' @description
#' Build-time gate over a Parquet artifact. Aborts when
#' [check_parquet_integrity()] reports any anomaly, so a corrupt
#' multi-gigabyte cube fails loudly at the moment it is written
#' instead of being read back truncated for months (whep#531).
#'
#' @inheritParams check_parquet_integrity
#'
#' @return Invisibly, `path`. Called for its side effect of aborting
#'   on violation.
#'
#' @export
#'
#' @examples
#' path <- tempfile(fileext = ".parquet")
#' nanoparquet::write_parquet(data.frame(x = 1:10), path)
#' assert_parquet_integrity(path)
#' unlink(path)
assert_parquet_integrity <- function(path, deep = FALSE) {
  anomalies <- check_parquet_integrity(path, deep = deep)
  if (nrow(anomalies) == 0) {
    return(invisible(path))
  }
  size_gib <- round(file.size(path) / 1024^3, 2)
  n_bad <- nrow(anomalies)
  kinds <- unique(anomalies$issue)
  first_group <- anomalies$row_group[[1]]
  first_detail <- anomalies$detail[[1]]
  cli::cli_abort(c(
    "Parquet file {.path {path}} ({size_gib} GiB) is corrupt.",
    "x" = "{cli::qty(n_bad)}{n_bad} anomal{?y/ies}: {.val {kinds}}.",
    "i" = "First at row group {first_group}: {first_detail}.",
    "i" = "Offsets that jump backwards past 4 GiB are the 32-bit \\
           overflow fixed in nanoparquet 0.5.0."
  ))
}

#' Write a Parquet file and verify it before returning.
#'
#' @description
#' Wrapper around `nanoparquet::write_parquet()` that reopens the file
#' it just wrote and runs [assert_parquet_integrity()] on it. Use it
#' for pipeline artifacts large enough that a silent write failure
#' would go unnoticed; the verification is metadata-only, so it costs
#' milliseconds regardless of file size.
#'
#' @param data Data frame to write.
#' @param path Destination path.
#' @param deep Whether to verify by decoding every row group as well
#'   as by checking the layout. Reads the whole file back.
#' @param ... Passed to `nanoparquet::write_parquet()`.
#'
#' @return Invisibly, `path`.
#'
#' @export
#'
#' @examples
#' path <- tempfile(fileext = ".parquet")
#' write_parquet_checked(data.frame(x = 1:10), path)
#' unlink(path)
write_parquet_checked <- function(data, path, deep = FALSE, ...) {
  nanoparquet::write_parquet(data, path, ...)
  assert_parquet_integrity(path, deep = deep)
}

.parquet_data_end <- function(path) {
  size <- file.size(path)
  if (size < 12) {
    cli::cli_abort("{.path {path}} is too short to be a Parquet file.")
  }
  # Seek, never read: this must stay O(1) on a 15 GB file.
  con <- file(path, "rb")
  on.exit(close(con), add = TRUE)
  seek(con, size - 8)
  tail <- readBin(con, "raw", n = 8)
  # Compare raws, not strings: rawToChar() can abort on arbitrary bytes.
  if (!identical(tail[5:8], charToRaw("PAR1"))) {
    cli::cli_abort("{.path {path}} does not end with the Parquet magic.")
  }
  footer_len <- readBin(tail[1:4], "integer", size = 4, endian = "little")
  size - 8 - footer_len
}

.parquet_offset_anomalies <- function(chunks, data_end) {
  empty <- tibble::tibble(
    row_group = integer(),
    column = integer(),
    issue = character(),
    chunk_start = numeric(),
    chunk_end = numeric(),
    detail = character()
  )
  needed <- c(
    "row_group",
    "column",
    "data_page_offset",
    "dictionary_page_offset",
    "total_compressed_size"
  )
  missing <- setdiff(needed, names(chunks))
  if (length(missing) > 0) {
    cli::cli_abort(
      "Parquet metadata is missing column{?s} {.field {missing}}."
    )
  }
  if (nrow(chunks) == 0) {
    return(empty)
  }
  chunks |>
    tibble::as_tibble() |>
    dplyr::mutate(
      chunk_start = dplyr::coalesce(
        as.numeric(dictionary_page_offset),
        as.numeric(data_page_offset)
      ),
      chunk_end = chunk_start + as.numeric(total_compressed_size)
    ) |>
    dplyr::arrange(row_group, column) |>
    # The 4-byte "PAR1" magic precedes the first chunk.
    dplyr::mutate(prev_end = dplyr::lag(chunk_end, default = 4)) |>
    dplyr::mutate(
      issue = dplyr::case_when(
        chunk_start < prev_end ~ "offset_overlap",
        chunk_end > data_end ~ "offset_past_data",
        .default = NA_character_
      ),
      detail = dplyr::case_when(
        issue == "offset_overlap" ~ paste0(
          "column ",
          column,
          " starts at ",
          chunk_start,
          " but the previous chunk ends at ",
          prev_end
        ),
        issue == "offset_past_data" ~ paste0(
          "column ",
          column,
          " ends at ",
          chunk_end,
          " past the ",
          data_end,
          "-byte data section"
        ),
        .default = NA_character_
      )
    ) |>
    dplyr::filter(!is.na(issue)) |>
    dplyr::select(
      row_group,
      column,
      issue,
      chunk_start,
      chunk_end,
      detail
    ) |>
    vctrs::vec_cast(empty)
}

.parquet_read_anomalies <- function(path) {
  reader <- arrow::ParquetFileReader$create(path)
  groups <- seq_len(reader$num_row_groups) - 1L
  purrr::map(groups, function(group) {
    tryCatch(
      {
        reader$ReadRowGroup(group)
        NULL
      },
      error = function(cnd) {
        tibble::tibble(
          row_group = group,
          column = NA_integer_,
          issue = "row_group_unreadable",
          chunk_start = NA_real_,
          chunk_end = NA_real_,
          detail = conditionMessage(cnd)
        )
      }
    )
  }) |>
    purrr::list_rbind()
}
