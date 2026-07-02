# Write a tiny gridded GSWP3-W5E5-style monthly wind NetCDF (2 lon x 2 lat x
# a few time steps), matching the real file's dimension names ("longitude"/
# "latitude"), the "days since 1970-1-1" time convention, north-to-south
# latitude order, and a _FillValue-bearing "wind" variable. Returns where it
# lives plus the fixture's known coordinates/time so tests can assert on
# them directly.
.lpjml_wind_fixture_cube <- function() {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
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
  path <- file.path(dir, "wind_gswp3-w5e5_1901_2016_monthly.nc")
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
