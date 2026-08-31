# Escaped rather than literal so the file stays ASCII: R CMD check NOTEs
# non-ASCII bytes in package sources. The strings are still non-ASCII.
sample_table <- function() {
  tibble::tribble(
    ~area_code, ~area_name,           ~year, ~value, ~flag,
    724L,       "Espa\u00f1a",        2020L, 1.5,    TRUE,
    384L,       "C\u00f4te d'Ivoire", 2020L, 2.5,    FALSE
  )
}

test_that("a parquet round trip preserves names, order, types and values", {
  path <- withr::local_tempfile(fileext = ".parquet")
  manifest <- whep::write_table_checked(sample_table(), path)
  back <- tibble::as_tibble(nanoparquet::read_parquet(path))

  expect_identical(names(back), names(sample_table()))
  expect_identical(back, sample_table())
  expect_identical(manifest$n_rows, 2L)
  expect_identical(manifest$format, "parquet")
})

test_that("non-ASCII text survives a csv round trip as UTF-8", {
  path <- withr::local_tempfile(fileext = ".csv")
  whep::write_table_checked(sample_table(), path)
  back <- readr::read_csv(path, show_col_types = FALSE, progress = FALSE)

  expect_identical(back$area_name, sample_table()$area_name)
  expect_true(all(validUTF8(readLines(path, warn = FALSE))))
})

test_that("a zero-row table keeps its schema in both formats", {
  empty <- sample_table()[0, ]
  parquet_path <- withr::local_tempfile(fileext = ".parquet")
  csv_path <- withr::local_tempfile(fileext = ".csv")

  whep::write_table_checked(empty, parquet_path)
  whep::write_table_checked(empty, csv_path)

  back <- tibble::as_tibble(nanoparquet::read_parquet(parquet_path))
  expect_identical(back, empty)
  expect_identical(
    names(readr::read_csv(csv_path, show_col_types = FALSE, progress = FALSE)),
    names(empty)
  )
})

test_that("a missing parent directory is created instead of silently dropped", {
  root <- withr::local_tempdir()
  path <- file.path(root, "deep", "nested", "out.parquet")

  whep::write_table_checked(sample_table(), path)

  expect_true(file.exists(path))
  expect_identical(nrow(nanoparquet::read_parquet(path)), 2L)
})

test_that("an unwritable parent aborts actionably", {
  root <- withr::local_tempdir()
  blocker <- file.path(root, "blocker")
  writeLines("not a directory", blocker)

  expect_error(
    whep::write_table_checked(sample_table(), file.path(blocker, "x.parquet")),
    class = "whep_table_write_parent_error"
  )
})

test_that("overwrite = FALSE refuses an existing target and keeps it", {
  path <- withr::local_tempfile(fileext = ".parquet")
  whep::write_table_checked(sample_table(), path)
  before <- tools::md5sum(path)

  expect_error(
    whep::write_table_checked(sample_table()[1, ], path, overwrite = FALSE),
    class = "whep_table_write_exists_error"
  )
  expect_identical(tools::md5sum(path), before)
})

test_that("an unknown extension aborts rather than guessing a format", {
  path <- withr::local_tempfile(fileext = ".dat")
  expect_error(
    whep::write_table_checked(sample_table(), path),
    class = "whep_table_write_format_error"
  )
  expect_false(file.exists(path))
})

test_that("a failed write leaves the previous artifact intact", {
  path <- withr::local_tempfile(fileext = ".parquet")
  whep::write_table_checked(sample_table(), path)
  before <- tools::md5sum(path)

  # A writer that dies part-way through, as an interrupted write would.
  testthat::local_mocked_bindings(
    .write_table_body = function(data, path, format, ...) {
      writeBin(charToRaw("PAR1truncated"), path)
      cli::cli_abort("simulated interruption")
    }
  )

  expect_error(whep::write_table_checked(sample_table()[1, ], path))
  expect_identical(tools::md5sum(path), before)
  expect_identical(nrow(nanoparquet::read_parquet(path)), 2L)
})

test_that("a corrupt write is detected and does not replace the target", {
  path <- withr::local_tempfile(fileext = ".parquet")
  whep::write_table_checked(sample_table(), path)
  before <- tools::md5sum(path)

  # Writes a file that exists but is not readable Parquet.
  testthat::local_mocked_bindings(
    .write_table_body = function(data, path, format, ...) {
      writeBin(charToRaw("PAR1nonsense"), path)
      invisible(path)
    }
  )

  expect_error(whep::write_table_checked(sample_table(), path))
  expect_identical(tools::md5sum(path), before)
})

test_that("no temporary files survive a successful or a failed write", {
  root <- withr::local_tempdir()
  path <- file.path(root, "out.parquet")
  whep::write_table_checked(sample_table(), path)
  expect_identical(list.files(root), "out.parquet")

  testthat::local_mocked_bindings(
    .write_table_body = function(data, path, format, ...) {
      writeBin(charToRaw("PAR1nonsense"), path)
      invisible(path)
    }
  )
  expect_error(whep::write_table_checked(sample_table(), path))
  expect_identical(list.files(root), "out.parquet")
})

test_that("sidecars are opt-in and describe the schema and the artifact", {
  path <- withr::local_tempfile(fileext = ".parquet")

  manifest <- whep::write_table_checked(sample_table(), path)
  expect_true(all(is.na(c(manifest$schema_path, manifest$provenance_path))))

  manifest <- whep::write_table_checked(
    sample_table(),
    path,
    sidecars = c("schema", "provenance")
  )
  schema <- yaml::read_yaml(manifest$schema_path)
  provenance <- yaml::read_yaml(manifest$provenance_path)

  expect_identical(
    purrr::map_chr(schema$columns, "name"),
    names(sample_table())
  )
  expect_identical(
    purrr::map_chr(schema$columns, "type"),
    c("integer", "character", "integer", "double", "logical")
  )
  expect_identical(provenance$n_rows, 2L)
  expect_identical(provenance$md5, unname(tools::md5sum(path)))
  expect_identical(provenance$writer, "whep::write_table_checked")
})

test_that("the schema sidecar names atomic types `vector()` accepts", {
  path <- withr::local_tempfile(fileext = ".parquet")
  manifest <- whep::write_table_checked(
    sample_table(),
    path,
    sidecars = "schema"
  )
  schema <- yaml::read_yaml(manifest$schema_path)
  types <- purrr::map_chr(schema$columns, "type")

  # The point of the vocabulary: a consumer rebuilds the prototype with
  # no WHEP-specific lookup, and it matches the table it came from.
  rebuilt <- purrr::map(types, \(type) vector(type, 0L)) |>
    rlang::set_names(purrr::map_chr(schema$columns, "name")) |>
    tibble::as_tibble()
  expect_identical(rebuilt, sample_table()[0, ])
})

test_that("classed columns report their class in the schema sidecar", {
  path <- withr::local_tempfile(fileext = ".parquet")
  rich <- tibble::tibble(
    day = as.Date("2020-01-01"),
    stamp = as.POSIXct("2020-01-01 12:00:00", tz = "UTC"),
    label = factor("a", levels = c("a", "b"))
  )
  manifest <- whep::write_table_checked(rich, path, sidecars = "schema")

  expect_identical(
    purrr::map_chr(yaml::read_yaml(manifest$schema_path)$columns, "type"),
    c("Date", "POSIXct", "factor")
  )
})

test_that("a zero-column table is written and described", {
  path <- withr::local_tempfile(fileext = ".parquet")
  manifest <- whep::write_table_checked(
    tibble::tibble(),
    path,
    sidecars = "schema"
  )

  expect_identical(manifest$n_cols, 0L)
  expect_identical(yaml::read_yaml(manifest$schema_path)$columns, list())
})

test_that("an unsupported input is rejected before anything is written", {
  path <- withr::local_tempfile(fileext = ".parquet")
  expect_error(
    whep::write_table_checked(1:10, path),
    class = "whep_table_write_input_error"
  )
  expect_false(file.exists(path))
})

test_that("a zero-column csv is refused rather than written empty", {
  path <- withr::local_tempfile(fileext = ".csv")
  expect_error(
    whep::write_table_checked(tibble::tibble(), path),
    class = "whep_table_write_input_error"
  )
  expect_false(file.exists(path))
})

test_that("duplicated column names are refused", {
  path <- withr::local_tempfile(fileext = ".parquet")
  clashing <- as.data.frame(matrix(1:4, nrow = 2))
  names(clashing) <- c("x", "x")
  expect_error(
    whep::write_table_checked(clashing, path),
    class = "whep_table_write_input_error"
  )
  expect_false(file.exists(path))
})

test_that("an unknown sidecar name aborts", {
  path <- withr::local_tempfile(fileext = ".parquet")
  expect_error(whep::write_table_checked(sample_table(), path, sidecars = "x"))
  expect_false(file.exists(path))
})
