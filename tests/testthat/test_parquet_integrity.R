# Helper fixtures --------------------------------------------------------------

# A frame big enough to be split into several row groups.
integrity_fixture <- function(n = 40L) {
  tibble::tibble(
    lon = seq_len(n) / 10,
    lat = rev(seq_len(n)) / 10,
    year = rep(2000:2009, length.out = n),
    value = as.numeric(seq_len(n)) * 1.5
  )
}

# Write `data` with several row groups so the layout has something to check.
write_multi_group <- function(data, path, rows_per_group = 5L) {
  nanoparquet::write_parquet(
    data,
    path,
    options = nanoparquet::parquet_options(
      num_rows_per_row_group = rows_per_group
    )
  )
  path
}

# Build a genuinely corrupt Parquet file that reproduces the observed failure
# mode of whep#531: the footer is intact and still declares every row group
# and row, but the bytes those row groups point at are not there. Here that is
# done by keeping the footer of a sound file and dropping the tail of its body,
# which is cheap; in the real 15.6 GB cube the same discrepancy arose because
# nanoparquet < 0.5.0 wrapped its 32-bit offsets past 4 GiB. Either way a
# reader gets a readable prefix and thrift page-header errors after it.
truncate_body_keep_footer <- function(source_path, out_path, keep_frac = 0.5) {
  raw_bytes <- readBin(
    source_path,
    "raw",
    n = file.size(source_path)
  )
  n <- length(raw_bytes)
  footer_len <- readBin(
    raw_bytes[(n - 7):(n - 4)],
    "integer",
    size = 4,
    endian = "little"
  )
  footer_start <- n - 8 - footer_len
  body <- raw_bytes[5:footer_start]
  keep <- max(1L, floor(length(body) * keep_frac))
  writeBin(
    c(raw_bytes[1:4], body[1:keep], raw_bytes[(footer_start + 1):n]),
    out_path
  )
  out_path
}

# Metadata of a sound two-row-group, two-column file, laid out contiguously.
sound_chunk_metadata <- function() {
  tibble::tribble(
    ~row_group, ~column, ~data_page_offset, ~total_compressed_size,
    0L, 0L, 4, 100,
    0L, 1L, 104, 100,
    1L, 0L, 204, 100,
    1L, 1L, 304, 100
  ) |>
    dplyr::mutate(dictionary_page_offset = NA_real_)
}

# The same layout, but with the last row group's offsets reduced by 2^32, which
# is exactly what a 32-bit offset field does once the file passes 4 GiB.
wrapped_chunk_metadata <- function(base = 2^32) {
  sound_chunk_metadata() |>
    dplyr::mutate(
      data_page_offset = data_page_offset + base,
      data_page_offset = dplyr::if_else(
        row_group == 1L,
        data_page_offset - base,
        data_page_offset
      )
    )
}

# check_parquet_integrity ------------------------------------------------------

test_that("a sound nanoparquet file reports no anomalies", {
  path <- withr::local_tempfile(fileext = ".parquet")
  write_multi_group(integrity_fixture(), path)

  expect_equal(nrow(whep::check_parquet_integrity(path)), 0L)
  expect_equal(nrow(whep::check_parquet_integrity(path, deep = TRUE)), 0L)
})

test_that("a sound arrow-written file reports no anomalies", {
  path <- withr::local_tempfile(fileext = ".parquet")
  arrow::write_parquet(integrity_fixture(), path, chunk_size = 5)

  expect_equal(nrow(whep::check_parquet_integrity(path, deep = TRUE)), 0L)
})

test_that("the report has a stable schema even when empty", {
  path <- withr::local_tempfile(fileext = ".parquet")
  write_multi_group(integrity_fixture(), path)

  expect_named(
    whep::check_parquet_integrity(path),
    c("row_group", "column", "issue", "chunk_start", "chunk_end", "detail")
  )
})

test_that("a footer describing bytes that are not there is flagged", {
  sound <- withr::local_tempfile(fileext = ".parquet")
  broken <- withr::local_tempfile(fileext = ".parquet")
  write_multi_group(integrity_fixture(), sound)
  truncate_body_keep_footer(sound, broken)

  # The footer still declares the full file, so a naive reader sees health.
  expect_equal(
    nanoparquet::read_parquet_metadata(broken)$file_meta_data$num_rows,
    nanoparquet::read_parquet_metadata(sound)$file_meta_data$num_rows
  )

  anomalies <- whep::check_parquet_integrity(broken)
  expect_gt(nrow(anomalies), 0L)
  expect_true(all(anomalies$issue == "offset_past_data"))
})

test_that("deep = TRUE names the row groups that fail to decode", {
  sound <- withr::local_tempfile(fileext = ".parquet")
  broken <- withr::local_tempfile(fileext = ".parquet")
  write_multi_group(integrity_fixture(), sound)
  truncate_body_keep_footer(sound, broken)

  anomalies <- whep::check_parquet_integrity(broken, deep = TRUE)
  unreadable <- dplyr::filter(anomalies, issue == "row_group_unreadable")
  expect_gt(nrow(unreadable), 0L)
  # The early row groups still decode: the failure is a silent truncation,
  # not a file that refuses to open.
  expect_lt(
    nrow(unreadable),
    nanoparquet::read_parquet_metadata(broken)$file_meta_data$num_rows
  )
})

test_that("check_parquet_integrity aborts on a missing file", {
  expect_error(
    whep::check_parquet_integrity(tempfile(fileext = ".parquet")),
    "not found"
  )
})

# .parquet_offset_anomalies ----------------------------------------------------

test_that("a 32-bit offset wraparound past 4 GiB is flagged", {
  anomalies <- whep:::.parquet_offset_anomalies(
    wrapped_chunk_metadata(),
    data_end = 2^32 + 1e6
  )

  expect_equal(nrow(anomalies), 1L)
  expect_equal(anomalies$issue, "offset_overlap")
  expect_equal(anomalies$row_group, 1L)
})

test_that("a contiguous layout is accepted and gaps are tolerated", {
  expect_equal(
    nrow(whep:::.parquet_offset_anomalies(
      sound_chunk_metadata(),
      data_end = 1e6
    )),
    0L
  )
  # Some writers leave padding between chunks; a gap is not corruption.
  padded <- sound_chunk_metadata() |>
    dplyr::mutate(data_page_offset = data_page_offset * 2)
  expect_equal(
    nrow(whep:::.parquet_offset_anomalies(padded, data_end = 1e6)),
    0L
  )
})

test_that("a dictionary page offset is used as the chunk start", {
  # The chunk starts at the dictionary page, so ignoring it makes the
  # preceding chunk look like it overlaps this one.
  meta <- sound_chunk_metadata() |>
    dplyr::mutate(
      dictionary_page_offset = data_page_offset,
      data_page_offset = data_page_offset + 40
    )
  expect_equal(
    nrow(whep:::.parquet_offset_anomalies(meta, data_end = 1e6)),
    0L
  )
})

test_that("zero column chunks is not an anomaly", {
  expect_equal(
    nrow(whep:::.parquet_offset_anomalies(
      sound_chunk_metadata()[0, ],
      data_end = 100
    )),
    0L
  )
})

test_that("missing metadata columns abort rather than pass silently", {
  expect_error(
    whep:::.parquet_offset_anomalies(
      dplyr::select(sound_chunk_metadata(), -total_compressed_size),
      data_end = 1e6
    ),
    "total_compressed_size"
  )
})

# assert_parquet_integrity -----------------------------------------------------

test_that("assert_parquet_integrity passes a sound file and returns it", {
  path <- withr::local_tempfile(fileext = ".parquet")
  write_multi_group(integrity_fixture(), path)

  expect_equal(whep::assert_parquet_integrity(path), path)
})

test_that("assert_parquet_integrity aborts loudly on a corrupt file", {
  sound <- withr::local_tempfile(fileext = ".parquet")
  broken <- withr::local_tempfile(fileext = ".parquet")
  write_multi_group(integrity_fixture(), sound)
  truncate_body_keep_footer(sound, broken)

  expect_error(whep::assert_parquet_integrity(broken), "is corrupt")
})

# write_parquet_checked --------------------------------------------------------

test_that("write_parquet_checked round-trips and verifies", {
  path <- withr::local_tempfile(fileext = ".parquet")
  data <- integrity_fixture()

  expect_equal(whep::write_parquet_checked(data, path), path)
  expect_equal(
    nanoparquet::read_parquet(path) |> tibble::as_tibble(),
    data
  )
})

test_that("write_parquet_checked is byte-identical to write_parquet", {
  # Verification must not change the artifact.
  plain <- withr::local_tempfile(fileext = ".parquet")
  checked <- withr::local_tempfile(fileext = ".parquet")
  data <- integrity_fixture()
  nanoparquet::write_parquet(data, plain)
  whep::write_parquet_checked(data, checked)

  expect_equal(
    readBin(plain, "raw", n = file.size(plain)),
    readBin(checked, "raw", n = file.size(checked))
  )
})

# .parquet_data_end ------------------------------------------------------------

test_that("the data section ends where the footer begins", {
  path <- withr::local_tempfile(fileext = ".parquet")
  write_multi_group(integrity_fixture(), path)

  data_end <- whep:::.parquet_data_end(path)
  expect_gt(data_end, 4)
  expect_lt(data_end, file.size(path))
})

test_that("a file that is not Parquet is rejected, not misread", {
  short <- withr::local_tempfile(fileext = ".parquet")
  writeBin(as.raw(1:4), short)
  expect_error(whep:::.parquet_data_end(short), "too short")

  wrong <- withr::local_tempfile(fileext = ".parquet")
  writeBin(as.raw(rep(0L, 40)), wrong)
  expect_error(whep:::.parquet_data_end(wrong), "Parquet magic")
})
