#' Write a table to disk safely and verifiably.
#'
#' @description
#' One writer for WHEP's tabular artifacts, with the four properties an
#' ad-hoc `nanoparquet::write_parquet()` or `readr::write_csv()` call
#' does not have (whep#375):
#'
#' - **The parent directory is created**, recursively. This is not a
#'   convenience: `nanoparquet::write_parquet()` given a path whose
#'   parent does not exist returns `NULL` and writes nothing at all,
#'   without warning, so a mistyped output directory looks like a
#'   successful build until something tries to read the file back.
#' - **The write is atomic.** The table goes to a temporary file beside
#'   the target and is renamed onto it only after it has been read back
#'   and verified. An interrupted, failed or corrupt write therefore
#'   leaves the previous artifact exactly as it was, instead of
#'   replacing hours of pipeline output with a truncated file.
#' - **The artifact is verified before it is published.** For Parquet
#'   that is [assert_parquet_integrity()] plus a row and column-name
#'   check; for CSV the file is re-read and its header and row count
#'   compared. `write_table_checked()` is the atomic, format-agnostic
#'   layer *on top of* [write_parquet_checked()], which stays the
#'   in-place Parquet primitive it already was.
#' - **Overwriting is a decision.** `overwrite = FALSE` refuses an
#'   existing target instead of clobbering it.
#'
#' Column order, column types and text encoding are preserved by round
#' trip, including for zero-row tables: a Parquet round trip returns an
#' identical tibble, and a CSV is always written as UTF-8. Prefer Parquet
#' for anything large: CSV cannot carry types, its verification has to
#' re-read the whole file, and a zero-column table is refused outright
#' because its CSV is a zero-byte file, indistinguishable from a write
#' that never happened.
#'
#' @section Sidecar contract:
#' `sidecars` optionally writes YAML files beside the artifact, named
#' after it: `<path>.schema.yaml` and `<path>.provenance.yaml`. They are
#' written after the data file has landed, so the data file is never
#' waiting on them.
#'
#' The schema sidecar is
#' `{format: "whep-table-schema/1", n_rows: <int>, columns: [{name, type}]}`
#' with `columns` in the table's own column order. `type` is the
#' column's first class, with `"numeric"` reported as `"double"` so that
#' every atomic type name is one `vector()` accepts (`"integer"`,
#' `"double"`, `"character"`, `"logical"`, `"list"`); a classed column
#' reports its class (`"factor"`, `"Date"`, `"POSIXct"`). A consumer can
#' therefore rebuild the prototype with no WHEP-specific lookup. This is
#' deliberately the same shape a declarative schema validator
#' (whep#373) and a typed empty-table constructor (whep#374) would
#' consume; neither exists yet, and this function does not implement
#' either.
#'
#' The provenance sidecar is `{format: "whep-table-provenance/1"}` plus
#' `path`, `table_format`, `n_rows`, `n_cols`, `bytes`, `md5`,
#' `written_at` (UTC, ISO 8601), `whep_version`, `r_version` and
#' `writer`. Every field except `written_at` is a function of the
#' artifact, so two builds of the same table differ only in that field.
#'
#' @param data Table to write. Must be a data frame; a tibble is
#'   returned unchanged by the round trip.
#' @param path Destination path. Its parent is created if needed.
#' @param format Output format: `"parquet"`, `"csv"`, or `"auto"` to
#'   take it from the file extension.
#' @param overwrite Whether an existing target may be replaced.
#' @param sidecars Which sidecars to write: any of `"schema"` and
#'   `"provenance"`. Defaults to none.
#' @param ... Passed to the underlying writer,
#'   `nanoparquet::write_parquet()` or `readr::write_csv()`.
#'
#' @return Invisibly, a one-row tibble describing what was written:
#'   `path`, `format`, `n_rows`, `n_cols`, `bytes`, `md5`,
#'   `schema_path` and `provenance_path` (the last two `NA` when the
#'   sidecar was not requested).
#'
#' @export
#'
#' @examples
#' path <- tempfile(fileext = ".parquet")
#' table <- tibble::tibble(area_code = 724L, year = 2020L, value = 1.5)
#' manifest <- write_table_checked(table, path, sidecars = "schema")
#' manifest$n_rows
#' nanoparquet::read_parquet(path)
#' unlink(c(path, manifest$schema_path))
write_table_checked <- function(
  data,
  path,
  format = c("auto", "parquet", "csv"),
  overwrite = TRUE,
  sidecars = character(),
  ...
) {
  format <- .resolve_table_format(path, rlang::arg_match(format))
  .check_table_to_write(data, format)
  sidecars <- rlang::arg_match(
    sidecars,
    c("schema", "provenance"),
    multiple = TRUE
  )
  .check_table_target(path, overwrite)
  .ensure_table_parent(path)
  tmp <- .table_tmp_path(path)
  withr::defer(unlink(tmp, force = TRUE))
  .write_table_body(data, tmp, format, ...)
  .verify_table_written(data, tmp, format)
  .replace_table_atomically(tmp, path)
  .table_write_manifest(data, path, format, sidecars)
}

.check_table_to_write <- function(data, format) {
  if (!is.data.frame(data)) {
    cli::cli_abort(
      c(
        "{.arg data} must be a data frame, not {.obj_type_friendly {data}}.",
        "i" = "Wrap it with {.fn tibble::as_tibble} first."
      ),
      class = "whep_table_write_input_error"
    )
  }
  dup <- unique(names(data)[duplicated(names(data))])
  if (length(dup) > 0) {
    cli::cli_abort(
      "{.arg data} has duplicated column name{?s}: {.field {dup}}.",
      class = "whep_table_write_input_error"
    )
  }
  if (identical(format, "csv") && ncol(data) == 0) {
    cli::cli_abort(
      c(
        "A zero-column table has no CSV representation.",
        "i" = "A CSV of it is a zero-byte file, indistinguishable from a
               write that never happened. Use {.val parquet} instead."
      ),
      class = "whep_table_write_input_error"
    )
  }
  invisible(data)
}

.resolve_table_format <- function(path, format) {
  if (format != "auto") {
    return(format)
  }
  known <- c("parquet", "csv")
  ext <- stringr::str_to_lower(tools::file_ext(path))
  if (!ext %in% known) {
    cli::cli_abort(
      c(
        "Cannot infer an output format from {.path {path}}.",
        "i" = "Use a {.val .parquet} or {.val .csv} extension, or set
               {.arg format} explicitly."
      ),
      class = "whep_table_write_format_error"
    )
  }
  ext
}

.check_table_target <- function(path, overwrite) {
  if (!isTRUE(overwrite) && file.exists(path)) {
    cli::cli_abort(
      c(
        "{.path {path}} already exists and {.arg overwrite} is {.code FALSE}.",
        "i" = "Delete it, choose another path, or pass
               {.code overwrite = TRUE}."
      ),
      class = "whep_table_write_exists_error"
    )
  }
  invisible(path)
}

.ensure_table_parent <- function(path) {
  parent <- dirname(path)
  if (!dir.exists(parent)) {
    dir.create(parent, recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(parent)) {
    cli::cli_abort(
      c(
        "Cannot create the parent directory {.path {parent}}.",
        "i" = "A file of that name may already exist, or the path may not
               be writable."
      ),
      class = "whep_table_write_parent_error"
    )
  }
  if (file.access(parent, mode = 2) != 0) {
    cli::cli_abort(
      "The parent directory {.path {parent}} is not writable.",
      class = "whep_table_write_parent_error"
    )
  }
  invisible(parent)
}

# Beside the target, so the final rename stays on one filesystem and is
# therefore atomic. Not dot-prefixed: a leftover must be visible.
.table_tmp_path <- function(path) {
  token <- basename(tempfile(""))
  paste0(path, ".whep-tmp-", token)
}

# Mocked in tests to simulate an interrupted or corrupt write.
.write_table_body <- function(data, path, format, ...) {
  switch(
    format,
    parquet = nanoparquet::write_parquet(data, path, ...),
    csv = readr::write_csv(data, path, ...)
  )
  invisible(path)
}

.verify_table_written <- function(data, path, format) {
  if (!file.exists(path) || file.size(path) == 0) {
    cli::cli_abort(
      c(
        "The writer produced no file at {.path {path}}.",
        "i" = "{.fn nanoparquet::write_parquet} returns silently without
               writing when its parent directory is missing."
      ),
      class = "whep_table_write_verify_error"
    )
  }
  switch(
    format,
    parquet = .verify_parquet_written(data, path),
    csv = .verify_csv_written(data, path)
  )
  invisible(path)
}

.verify_parquet_written <- function(data, path) {
  assert_parquet_integrity(path)
  found <- rlang::try_fetch(
    {
      schema <- nanoparquet::read_parquet_schema(path)
      # The first schema element is the root group, not a column.
      list(
        names = schema$name[-1],
        n_rows = nanoparquet::read_parquet_info(path)$num_rows
      )
    },
    error = function(cnd) {
      cli::cli_abort(
        "Cannot read back the Parquet file just written to {.path {path}}.",
        class = "whep_table_write_verify_error",
        parent = cnd
      )
    }
  )
  .compare_written_shape(data, found, path)
}

.verify_csv_written <- function(data, path) {
  # Read everything as text: this checks the shape, not the parser's
  # type guesses, and skips a second full type conversion.
  back <- readr::read_csv(
    path,
    col_types = readr::cols(.default = readr::col_character()),
    progress = FALSE
  )
  .compare_written_shape(
    data,
    list(names = names(back), n_rows = nrow(back)),
    path
  )
}

.compare_written_shape <- function(data, found, path) {
  if (!identical(found$names, names(data))) {
    cli::cli_abort(
      c(
        "{.path {path}} came back with different columns.",
        "x" = "Wrote {.field {names(data)}}.",
        "x" = "Read {.field {found$names}}."
      ),
      class = "whep_table_write_verify_error"
    )
  }
  found_rows <- as.numeric(found$n_rows)
  wanted_rows <- as.numeric(nrow(data))
  if (!identical(found_rows, wanted_rows)) {
    cli::cli_abort(
      c(
        "{.path {path}} came back with {cli::qty(found_rows)}{found_rows}
         row{?s} instead of {wanted_rows}.",
        "i" = "The write was truncated; the previous artifact was kept."
      ),
      class = "whep_table_write_verify_error"
    )
  }
  invisible(path)
}

.replace_table_atomically <- function(tmp, path) {
  if (file.rename(tmp, path)) {
    return(invisible(path))
  }
  # Windows refuses to rename onto an existing file. Removing it first
  # opens a window where neither file is at `path`, which is why the
  # rename is tried on its own first.
  unlink(path, force = TRUE)
  if (!file.rename(tmp, path)) {
    cli::cli_abort(
      c(
        "Cannot move the verified table into place at {.path {path}}.",
        "i" = "The verified copy is still at {.path {tmp}}."
      ),
      class = "whep_table_write_replace_error"
    )
  }
  invisible(path)
}

.table_write_manifest <- function(data, path, format, sidecars) {
  written <- .write_table_sidecars(data, path, format, sidecars)
  invisible(tibble::tibble(
    path = path,
    format = format,
    n_rows = nrow(data),
    n_cols = ncol(data),
    bytes = as.numeric(file.size(path)),
    md5 = unname(tools::md5sum(path)),
    schema_path = written[["schema"]],
    provenance_path = written[["provenance"]]
  ))
}

.write_table_sidecars <- function(data, path, format, sidecars) {
  written <- c(schema = NA_character_, provenance = NA_character_)
  if ("schema" %in% sidecars) {
    written[["schema"]] <- .write_sidecar(
      .table_schema_record(data),
      paste0(path, ".schema.yaml")
    )
  }
  if ("provenance" %in% sidecars) {
    written[["provenance"]] <- .write_sidecar(
      .table_provenance_record(data, path, format),
      paste0(path, ".provenance.yaml")
    )
  }
  written
}

.write_sidecar <- function(record, path) {
  tmp <- .table_tmp_path(path)
  withr::defer(unlink(tmp, force = TRUE))
  yaml::write_yaml(record, tmp)
  .replace_table_atomically(tmp, path)
  path
}

.table_schema_record <- function(data) {
  columns <- purrr::map2(
    names(data),
    unname(purrr::map_chr(data, .column_type_label)),
    \(name, type) list(name = name, type = type)
  )
  list(
    format = "whep-table-schema/1",
    n_rows = nrow(data),
    columns = columns
  )
}

# The column's own first class, except that "numeric" is reported as
# "double" so an atomic type name is always one `vector()` accepts and a
# consumer can build the prototype without a special case.
.column_type_label <- function(column) {
  label <- class(column)[[1]]
  if (identical(label, "numeric")) "double" else label
}

.table_provenance_record <- function(data, path, format) {
  list(
    format = "whep-table-provenance/1",
    path = basename(path),
    table_format = format,
    n_rows = nrow(data),
    n_cols = ncol(data),
    bytes = as.numeric(file.size(path)),
    md5 = unname(tools::md5sum(path)),
    written_at = format(
      as.POSIXlt(Sys.time(), tz = "UTC"),
      "%Y-%m-%dT%H:%M:%SZ"
    ),
    whep_version = as.character(utils::packageVersion("whep")),
    r_version = as.character(getRversion()),
    writer = "whep::write_table_checked"
  )
}
