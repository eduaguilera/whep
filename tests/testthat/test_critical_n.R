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
  testthat::expect_named(out, c("lon", "lat", "value"))
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
  pointblank::expect_col_exists(out, c("lon", "lat", "value"))
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
  testthat::expect_named(out, c("lon", "lat", "value"))
  testthat::expect_equal(out$value, c(84, 12))
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

testthat::test_that("read_critical_n aborts when no directory is set", {
  withr::local_envvar(WHEP_CRITICAL_N_DIR = "")
  testthat::expect_error(
    whep::read_critical_n("critical_n_surplus"),
    "WHEP_CRITICAL_N_DIR|directory"
  )
})
