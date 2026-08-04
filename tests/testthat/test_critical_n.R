# Writes a tiny 3x2-cell ESRI ASCII grid (6-line header + matrix) at the
# nested archive path read_critical_n() expects for the default
# threshold "mi" and land_use "all", so the real parser is exercised
# without the off-repo Zenodo archive.
.critical_n_write_asc <- function(dir) {
  target <- file.path(
    dir,
    "extracted",
    "Global_critical_N_surpluses_and_N_inputs_and_their_exceedances",
    "Output_files",
    "Critical N surpluses"
  )
  dir.create(target, recursive = TRUE, showWarnings = FALSE)
  writeLines(
    c(
      "ncols 3",
      "nrows 2",
      "xllcorner 0",
      "yllcorner 0",
      "cellsize 1",
      "NODATA_value -9999",
      "10 20 -9999",
      "40 50 60"
    ),
    file.path(target, "nsur_crit_mi_all_ph.asc")
  )
  invisible(dir)
}

testthat::test_that("read_critical_n parses an ESRI grid at cell centres", {
  tmp <- withr::local_tempdir()
  .critical_n_write_asc(tmp)
  out <- whep::read_critical_n("critical_n_surplus", dir = tmp)
  testthat::expect_named(
    out,
    c(
      "lon",
      "lat",
      "value",
      "critical_var",
      "critical_threshold",
      "critical_land_use",
      "critical_year",
      "critical_source"
    )
  )
  testthat::expect_s3_class(out, "tbl_df")
  # 6 cells minus the one NODATA cell.
  testthat::expect_equal(nrow(out), 5L)
  # Row 1 (north) has lat 1.5 with the two non-NODATA values.
  north <- out[out$lat == 1.5, ]
  testthat::expect_equal(sort(north$value), c(10, 20))
  # Cell centre of the north-west cell: lon 0.5, lat 1.5.
  first <- out[out$lon == 0.5 & out$lat == 1.5, ]
  testthat::expect_equal(first$value, 10)
  # South row lat 0.5 keeps all three values.
  testthat::expect_equal(sort(out$value[out$lat == 0.5]), c(40, 50, 60))
  # NODATA (-9999) cells are dropped.
  testthat::expect_false(any(out$value == -9999))
})

testthat::test_that("read_critical_n(example = TRUE) returns lon/lat/value", {
  out <- whep::read_critical_n(example = TRUE)
  pointblank::expect_col_exists(
    out,
    c("lon", "lat", "value", "critical_var", "critical_land_use")
  )
  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_gt(nrow(out), 0L)
})

testthat::test_that("read_critical_n(data=) bypasses the file read", {
  grid <- tibble::tribble(
    ~lon, ~lat, ~value,
    -0.25, 51.75, 84,
    0.25, 51.75, 12
  )
  out <- whep::read_critical_n(var = "exceedance", data = grid)
  pointblank::expect_col_exists(
    out,
    c("lon", "lat", "value", "critical_var", "critical_land_use")
  )
  testthat::expect_equal(out$value, c(84, 12))
  testthat::expect_true(all(out$critical_var == "exceedance"))
})

testthat::test_that("read_critical_n rejects an unknown var", {
  testthat::expect_error(
    whep::read_critical_n(var = "not_a_layer", data = tibble::tibble()),
    "arg_match|must be one of|not_a_layer"
  )
})

testthat::test_that("read_critical_n(data=) errors on missing columns", {
  testthat::expect_error(
    whep::read_critical_n(var = "exceedance", data = tibble::tibble(x = 1)),
    "value|lon|lat"
  )
})

testthat::test_that("no configured directory falls through to the cache", {
  withr::local_envvar(WHEP_CRITICAL_N_DIR = "")
  # The cache is consulted instead of aborting, and nothing downloads unless
  # the cache is empty. Mocked so the assertion holds whether or not the
  # developer running the suite happens to have a populated cache.
  reached <- FALSE
  testthat::local_mocked_bindings(
    .critn_cached_dir = function(...) {
      reached <<- TRUE
      "/nonexistent-cache"
    }
  )
  testthat::expect_equal(
    whep:::.resolve_critical_n_dir(),
    "/nonexistent-cache"
  )
  testthat::expect_true(reached)
})

testthat::test_that("a set WHEP_CRITICAL_N_DIR wins over the cache", {
  dir <- withr::local_tempdir()
  withr::local_envvar(WHEP_CRITICAL_N_DIR = dir)
  testthat::local_mocked_bindings(
    .critn_cached_dir = function(...) testthat::fail("cache must not be used")
  )
  testthat::expect_equal(whep:::.resolve_critical_n_dir(), dir)
})

# ---- Archive resolution and on-demand Zenodo fetch --------------------------

testthat::test_that("the archive constants match the published Zenodo record", {
  testthat::expect_match(
    whep:::.critn_archive_url(),
    "zenodo\\.org/api/records/6395016"
  )
  testthat::expect_match(whep:::.critn_archive_md5(), "^[0-9a-f]{32}$")
  testthat::expect_equal(
    whep:::.critn_archive_md5(),
    "d6b4bf88e9b140bd25a147396e371733"
  )
  # The cache-hit marker must be the directory the reader then reads through.
  testthat::expect_match(whep:::.critn_archive_root(), "^Global_critical_N_")
})

testthat::test_that("an explicit dir short-circuits the cache and the fetch", {
  dir <- withr::local_tempdir()
  testthat::expect_equal(whep:::.resolve_critical_n_dir(dir), dir)
})

testthat::test_that("a populated cache is reused without downloading", {
  dir <- withr::local_tempdir()
  dir.create(
    file.path(dir, "extracted", whep:::.critn_archive_root()),
    recursive = TRUE
  )
  boom <- function(...) testthat::fail("must not touch the network")
  testthat::expect_equal(
    whep:::.critn_cached_dir(dir, download = boom, extract = boom),
    dir
  )
})

testthat::test_that("an empty cache downloads then extracts, in that order", {
  dir <- withr::local_tempdir()
  calls <- character()
  fake_download <- function(d) {
    calls <<- c(calls, "download")
    file.path(d, "critical_n_archive.7z")
  }
  fake_extract <- function(archive, exdir) {
    calls <<- c(calls, "extract")
    dir.create(
      file.path(exdir, whep:::.critn_archive_root()),
      recursive = TRUE
    )
  }
  out <- whep:::.critn_cached_dir(
    dir,
    download = fake_download,
    extract = fake_extract
  )
  testthat::expect_equal(calls, c("download", "extract"))
  testthat::expect_equal(out, dir)
})

testthat::test_that("an extraction that yields the wrong layout aborts", {
  dir <- withr::local_tempdir()
  testthat::expect_error(
    whep:::.critn_cached_dir(
      dir,
      download = function(d) "archive.7z",
      extract = function(archive, exdir) dir.create(exdir, recursive = TRUE)
    ),
    "did not unpack as expected"
  )
})

testthat::test_that("a cached archive with the right MD5 is not re-downloaded", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "critical_n_archive.7z")
  writeLines("x", path)
  testthat::expect_false(whep:::.critn_md5_ok(path))
  testthat::local_mocked_bindings(
    .critn_archive_md5 = function() unname(tools::md5sum(path))
  )
  testthat::expect_true(whep:::.critn_md5_ok(path))
  # .critn_download() returns the cached path without hitting the network:
  # download.file() would error on the unreachable URL if it were called.
  testthat::expect_equal(whep:::.critn_download(dir), path)
})

testthat::test_that("7z discovery returns a real path or nothing", {
  bin <- whep:::.critn_7z_binary()
  if (is.null(bin)) {
    testthat::succeed()
  } else {
    testthat::expect_true(file.exists(bin))
  }
})

testthat::test_that("no extractor aborts, naming the verified download", {
  dir <- withr::local_tempdir()
  archive <- file.path(dir, "critical_n_archive.7z")
  writeLines("x", archive)
  testthat::local_mocked_bindings(.critn_7z_binary = function() NULL)
  testthat::local_mocked_bindings(
    is_installed = function(...) FALSE,
    .package = "rlang"
  )
  testthat::expect_error(
    whep:::.critn_extract(archive, file.path(dir, "extracted")),
    "No 7-Zip extractor available"
  )
  testthat::expect_error(
    whep:::.critn_extract(archive, file.path(dir, "extracted")),
    "MD5-verified"
  )
})
