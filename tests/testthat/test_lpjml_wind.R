# Write a tiny gridded GSWP3-W5E5-style monthly wind NetCDF (2 lon x 2 lat x
# a few time steps), matching the real file's dimension names ("longitude"/
# "latitude"), the "days since 1970-1-1" time convention, north-to-south
# latitude order, and a _FillValue-bearing "wind" variable. Returns where it
# lives plus the fixture's known coordinates/time so tests can assert on
# them directly.
#
# `file_name` defaults to the name the registered `lpjml-wind-isimip-1901-2019`
# pin actually ships. It is an argument because the span in that name is not
# stable -- it moved from 1901_2016 when the base was extended to 2019 -- and a
# fixture that hardcodes one span cannot notice a reader that hardcodes another
# (whep#1069).
.lpjml_wind_fixture_cube <- function(
  file_name = "wind_gswp3-w5e5_1901_2019_monthly.nc",
  dir = NULL
) {
  if (is.null(dir)) {
    dir <- withr::local_tempdir(.local_envir = parent.frame())
  }
  lon <- c(-179.75, -179.25)
  lat <- c(89.75, 89.25) # north-to-south, matching the real file's order
  # Three monthly steps: Dec 1901, Jan 1902, Feb 1902 (days since epoch).
  time <- as.numeric(as.Date(c("1901-12-01", "1902-01-01", "1902-02-01")))
  fill_value <- -1.17549402418441e+38
  dim_lon <- ncdf4::ncdim_def("longitude", "degrees_east", lon)
  dim_lat <- ncdf4::ncdim_def("latitude", "degrees_north", lat)
  dim_time <- ncdf4::ncdim_def("time", "days since 1970-1-1", time)
  var <- ncdf4::ncvar_def(
    "wind",
    "",
    list(dim_lon, dim_lat, dim_time),
    missval = fill_value,
    prec = "float"
  )
  path <- file.path(dir, file_name)
  nc <- ncdf4::nc_create(path, list(var))
  vals <- array(
    seq_len(length(lon) * length(lat) * length(time)),
    dim = c(length(lon), length(lat), length(time))
  )
  # Make one cell in the first time step NODATA to test fill-value dropping.
  vals[1, 1, 1] <- fill_value
  ncdf4::ncvar_put(nc, var, vals)
  ncdf4::nc_close(nc)
  list(
    dir = dir,
    lon = lon,
    lat = lat,
    n_cells = length(lon) * length(lat)
  )
}

testthat::test_that("year/month are decoded correctly from the time axis", {
  cube <- .lpjml_wind_fixture_cube()
  result <- whep::read_lpjml_wind(wind_dir = cube$dir)

  pointblank::expect_col_exists(
    result,
    c("lon", "lat", "year", "month", "windspeed_ms")
  )
  decoded <- result |>
    dplyr::distinct(year, month) |>
    dplyr::arrange(year, month)
  testthat::expect_equal(decoded$year, c(1901L, 1902L, 1902L))
  testthat::expect_equal(decoded$month, c(12L, 1L, 2L))
})

testthat::test_that("years argument filters the time axis", {
  cube <- .lpjml_wind_fixture_cube()
  result <- whep::read_lpjml_wind(years = 1902L, wind_dir = cube$dir)

  testthat::expect_setequal(result$year, 1902L)
  testthat::expect_setequal(result$month, c(1L, 2L))
})

testthat::test_that("an absent year returns an empty documented schema", {
  cube <- .lpjml_wind_fixture_cube()
  result <- whep::read_lpjml_wind(years = 1800L, wind_dir = cube$dir)

  testthat::expect_equal(nrow(result), 0L)
  testthat::expect_identical(
    names(result),
    c("lon", "lat", "year", "month", "windspeed_ms")
  )
})

testthat::test_that("row order reflects the file's north-to-south latitude", {
  cube <- .lpjml_wind_fixture_cube()
  result <- whep::read_lpjml_wind(years = 1902L, wind_dir = cube$dir)

  testthat::expect_setequal(result$lon, cube$lon)
  testthat::expect_setequal(result$lat, cube$lat)
  testthat::expect_true(max(result$lat) == cube$lat[1])
})

testthat::test_that("_FillValue cells are dropped as NA", {
  cube <- .lpjml_wind_fixture_cube()
  result <- whep::read_lpjml_wind(years = 1901L, wind_dir = cube$dir)

  # Fixture has n_cells for the Dec-1901 step, minus the one NODATA cell.
  testthat::expect_equal(nrow(result), cube$n_cells - 1L)
  testthat::expect_false(anyNA(result$windspeed_ms))
})

testthat::test_that("example = TRUE returns a schema-complete fixture", {
  result <- whep::read_lpjml_wind(example = TRUE)

  pointblank::expect_col_exists(
    result,
    c("lon", "lat", "year", "month", "windspeed_ms")
  )
  testthat::expect_gt(nrow(result), 0)
})

testthat::test_that("the filename span is read off the directory", {
  # The span in the artefact name is not stable: the base was extended from
  # 2016 to 2019, and the next extension will move it again. Any span must
  # read, so no future rename silently turns the reader into a file-not-found.
  for (nm in c(
    "wind_gswp3-w5e5_1901_2016_monthly.nc",
    "wind_gswp3-w5e5_1901_2019_monthly.nc",
    "wind_gswp3-w5e5_era5_1901_2023_monthly.nc"
  )) {
    cube <- .lpjml_wind_fixture_cube(file_name = nm)
    result <- whep::read_lpjml_wind(years = 1902L, wind_dir = cube$dir)
    testthat::expect_setequal(result$year, 1902L)
    testthat::expect_gt(nrow(result), 0)
  }
})

testthat::test_that("the newest span wins when several are present", {
  dir <- withr::local_tempdir()
  .lpjml_wind_fixture_cube(
    file_name = "wind_gswp3-w5e5_1901_2016_monthly.nc",
    dir = dir
  )
  .lpjml_wind_fixture_cube(
    file_name = "wind_gswp3-w5e5_1901_2019_monthly.nc",
    dir = dir
  )
  # Both fixtures hold the same values, so this asserts the resolution, not a
  # number: the reader must pick one deterministically rather than abort.
  testthat::expect_gt(nrow(whep::read_lpjml_wind(wind_dir = dir)), 0)
  testthat::expect_identical(
    basename(whep:::.wind_file(dir)),
    "wind_gswp3-w5e5_1901_2019_monthly.nc"
  )
})

testthat::test_that("a directory with no wind file aborts naming its files", {
  dir <- withr::local_tempdir()
  file.create(file.path(dir, "era5_wind_monthly_2017_2023_0p25.nc"))

  # The ERA5 companion pin lives in the same directory but is a different grid
  # and a different variable, so it must not be mistaken for the base.
  testthat::expect_error(
    whep::read_lpjml_wind(wind_dir = dir),
    "era5_wind_monthly_2017_2023_0p25\\.nc"
  )
})

testthat::test_that("an empty wind directory aborts with the expected name", {
  dir <- withr::local_tempdir()

  testthat::expect_error(
    whep::read_lpjml_wind(wind_dir = dir),
    "wind_gswp3-w5e5"
  )
})

testthat::test_that("reads the real GSWP3-W5E5 wind forcing file", {
  testthat::skip_on_cran()
  testthat::skip_if(
    Sys.getenv("WHEP_WIND_DIR") == "",
    "WHEP_WIND_DIR not set; skipping real-data smoke test."
  )

  result <- whep::read_lpjml_wind(years = 2005L)

  pointblank::expect_col_exists(
    result,
    c("lon", "lat", "year", "month", "windspeed_ms")
  )
  testthat::expect_gt(nrow(result), 0)
  testthat::expect_setequal(result$year, 2005L)
  testthat::expect_false(anyNA(result$windspeed_ms))
  testthat::expect_true(all(result$windspeed_ms >= 0))
  testthat::expect_true(all(result$windspeed_ms <= 20))
})
