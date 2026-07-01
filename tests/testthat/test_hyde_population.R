# Write a tiny HYDE-style ESRI ASCII grid ZIP covering exactly
# `n_lon_blocks` x `n_lat_blocks` WHEP 0.5-degree cells (each block is 6x6
# fine cells, since 0.5 / 0.0833333 = 6), zipped the same way as the real
# "{year}AD_pop.zip" archive (containing "urbc_{year}AD.asc"), so the test
# exercises the real zip-read-parse path, not a shortcut.
.hyde_fixture_zip <- function(
  year = 1900L,
  n_lon_blocks = 1L,
  n_lat_blocks = 1L,
  fill_value = 100,
  nodata_cells = integer(0)
) {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  cellsize <- 0.0833333
  ncols <- 6L * n_lon_blocks
  nrows <- 6L * n_lat_blocks
  values <- rep(fill_value, ncols * nrows)
  values[nodata_cells] <- -9999
  header <- c(
    paste("ncols", ncols),
    paste("nrows", nrows),
    "xllcorner -180.0",
    "yllcorner -90.0",
    paste("cellsize", cellsize),
    "NODATA_value -9999.0"
  )
  # Only the northwest-most block is needed: place the fine grid starting at
  # the grid origin so the resulting block center matches the real-grid
  # formula (lon_block[1] = -180 + 0.25, lat_block[1] = 90 - 0.25).
  mat <- matrix(values, nrow = nrows, ncol = ncols, byrow = TRUE)
  rows <- apply(mat, 1, paste, collapse = " ")
  asc_path <- file.path(dir, paste0("urbc_", year, "AD.asc"))
  writeLines(c(header, rows), asc_path)

  zip_path <- file.path(dir, paste0(year, "AD_pop.zip"))
  withr::with_dir(dir, zip::zip(basename(zip_path), basename(asc_path)))

  list(
    dir = dir,
    n_cells = n_lon_blocks * n_lat_blocks,
    fine_cells_per_block = 36L,
    fill_value = fill_value
  )
}

testthat::test_that("read_hyde_population sums fine cells into one 0.5-deg cell", {
  fixture <- .hyde_fixture_zip(
    year = 1900L,
    n_lon_blocks = 1L,
    n_lat_blocks = 1L
  )
  result <- whep::read_hyde_population(
    hyde_dir = fixture$dir,
    years = 1900L
  )

  pointblank::expect_col_exists(result, c("lon", "lat", "year", "urban_pop"))
  testthat::expect_equal(nrow(result), 1L)
  testthat::expect_equal(
    result$urban_pop,
    fixture$fine_cells_per_block * fixture$fill_value
  )
})

testthat::test_that("read_hyde_population maps block centers correctly", {
  fixture <- .hyde_fixture_zip(
    year = 1900L,
    n_lon_blocks = 1L,
    n_lat_blocks = 1L
  )
  result <- whep::read_hyde_population(
    hyde_dir = fixture$dir,
    years = 1900L
  )

  # xllcorner = -180 fixes the WEST edge of the grid (column 1), so the
  # single fine block's 0.5-degree block center has lon = -180 + 0.25. Row 1
  # of the data matrix is the NORTHERNMOST row (standard ESRI convention:
  # data starts at yllcorner + cellsize*nrows and decreases), so the single
  # fine block sits at the NORTH edge, giving lat = 90 - 0.25.
  testthat::expect_equal(result$lon, -179.75)
  testthat::expect_equal(result$lat, 89.75)
})

testthat::test_that("read_hyde_population keeps adjacent 0.5-deg cells separate", {
  fixture <- .hyde_fixture_zip(
    year = 1900L,
    n_lon_blocks = 2L,
    n_lat_blocks = 1L
  )
  result <- whep::read_hyde_population(
    hyde_dir = fixture$dir,
    years = 1900L
  )

  testthat::expect_equal(nrow(result), 2L)
  testthat::expect_setequal(result$lon, c(-179.75, -179.25))
  testthat::expect_true(all(
    result$urban_pop == fixture$fine_cells_per_block * fixture$fill_value
  ))
})

testthat::test_that("read_hyde_population drops NODATA cells", {
  fixture <- .hyde_fixture_zip(
    year = 1900L,
    n_lon_blocks = 1L,
    n_lat_blocks = 1L,
    nodata_cells = c(1L, 2L, 3L)
  )
  result <- whep::read_hyde_population(
    hyde_dir = fixture$dir,
    years = 1900L
  )

  testthat::expect_equal(
    result$urban_pop,
    (fixture$fine_cells_per_block - 3L) * fixture$fill_value
  )
})

testthat::test_that("read_hyde_population reads multiple years", {
  fixture_1900 <- .hyde_fixture_zip(year = 1900L, fill_value = 100)
  fixture_1950 <- .hyde_fixture_zip(year = 1950L, fill_value = 200)
  file.copy(
    file.path(fixture_1950$dir, "1950AD_pop.zip"),
    file.path(fixture_1900$dir, "1950AD_pop.zip")
  )
  result <- whep::read_hyde_population(
    hyde_dir = fixture_1900$dir,
    years = c(1900L, 1950L)
  )

  testthat::expect_setequal(result$year, c(1900L, 1950L))
  testthat::expect_equal(
    result$urban_pop[result$year == 1900L],
    36L * 100
  )
  testthat::expect_equal(
    result$urban_pop[result$year == 1950L],
    36L * 200
  )
})

testthat::test_that("read_hyde_population requires years to be specified", {
  testthat::expect_error(
    whep::read_hyde_population(hyde_dir = "some/dir"),
    "years"
  )
})

testthat::test_that("read_hyde_population aborts on BC years", {
  testthat::expect_error(
    whep::read_hyde_population(hyde_dir = "some/dir", years = -100L),
    "AD"
  )
})

testthat::test_that("read_hyde_population example fixture is schema-complete", {
  out <- whep::read_hyde_population(example = TRUE)
  pointblank::expect_col_exists(out, c("lon", "lat", "year", "urban_pop"))
  pointblank::expect_col_vals_gte(out, "urban_pop", 0)
})
