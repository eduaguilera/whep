# Write a tiny gridded LPJmL-style monthly NetCDF (2 lon x 2 lat x 12 month)
# holding one positive flux variable, and return where it lives plus the
# expected per-month cell count.
.lpjml_hydro_fixture_cube <- function(
  var_name = "seepage",
  file = "mseepage.nc"
) {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  lon <- c(-179.75, -179.25)
  lat <- c(0.25, 0.75)
  time <- 1:12
  dim_lon <- ncdf4::ncdim_def("lon", "degrees_east", lon)
  dim_lat <- ncdf4::ncdim_def("lat", "degrees_north", lat)
  dim_time <- ncdf4::ncdim_def("time", "months", time)
  var <- ncdf4::ncvar_def(
    var_name,
    "mm",
    list(dim_lon, dim_lat, dim_time),
    missval = -9999
  )
  path <- file.path(dir, file)
  nc <- ncdf4::nc_create(path, list(var))
  vals <- array(
    seq_len(length(lon) * length(lat) * length(time)),
    dim = c(length(lon), length(lat), length(time))
  )
  ncdf4::ncvar_put(nc, var, vals)
  ncdf4::nc_close(nc)
  list(dir = dir, file = file, n_cells = length(lon) * length(lat))
}

testthat::test_that("monthly read returns one tidy row per cell-month", {
  cube <- .lpjml_hydro_fixture_cube()
  result <- whep::read_lpjml_hydrology(
    "drainage",
    run_dir = cube$dir,
    years = 1901L,
    first_year = 1901L,
    monthly = TRUE
  )

  pointblank::expect_col_exists(
    result,
    c("lon", "lat", "year", "month", "value")
  )
  testthat::expect_equal(nrow(result), cube$n_cells * 12)
  pointblank::expect_col_vals_gte(result, "value", 0)
  testthat::expect_setequal(result$month, 1:12)
})

testthat::test_that("annual read sums the 12 months per cell", {
  cube <- .lpjml_hydro_fixture_cube()
  result <- whep::read_lpjml_hydrology(
    "drainage",
    run_dir = cube$dir,
    years = 1901L,
    first_year = 1901L,
    monthly = FALSE
  )

  pointblank::expect_col_exists(result, c("lon", "lat", "year", "value"))
  testthat::expect_false("month" %in% names(result))
  testthat::expect_equal(nrow(result), cube$n_cells)
  pointblank::expect_col_vals_gt(result, "value", 0)

  monthly <- whep::read_lpjml_hydrology(
    "drainage",
    run_dir = cube$dir,
    years = 1901L,
    first_year = 1901L,
    monthly = TRUE
  )
  annual_from_monthly <- monthly |>
    dplyr::summarise(value = sum(value), .by = c(lon, lat, year)) |>
    dplyr::arrange(lon, lat)
  result <- dplyr::arrange(result, lon, lat)
  testthat::expect_equal(result$value, annual_from_monthly$value)
})
