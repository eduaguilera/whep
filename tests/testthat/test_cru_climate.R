test_that("read_cru_climate example returns the tidy schema", {
  out <- whep::read_cru_climate(example = TRUE)
  pointblank::expect_col_exists(
    out,
    columns = c("lon", "lat", "year", "month", "value", "var")
  )
  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_gt(nrow(out), 0)
  testthat::expect_true(all(out$month %in% 1:12))
})

test_that("read_cru_climate uses injected data and returns tidy schema", {
  injected <- tibble::tribble(
    ~lon, ~lat, ~year, ~month, ~value,
    -0.25, 51.75, 2000L, 1L, 4.2,
    -0.25, 51.75, 2000L, 7L, 17.8
  )
  out <- whep::read_cru_climate(var = "tmp", data = injected)
  pointblank::expect_col_exists(
    out,
    columns = c("lon", "lat", "year", "month", "value", "var")
  )
  testthat::expect_equal(nrow(out), 2)
  testthat::expect_true(all(out$var == "tmp"))
  testthat::expect_equal(sort(out$value), c(4.2, 17.8))
})

test_that("read_cru_climate filters injected data by year", {
  injected <- tibble::tribble(
    ~lon, ~lat, ~year, ~month, ~value,
    -0.25, 51.75, 1999L, 1L, 3.1,
    -0.25, 51.75, 2000L, 1L, 4.2
  )
  out <- whep::read_cru_climate(var = "pre", years = 2000, data = injected)
  testthat::expect_equal(nrow(out), 1)
  testthat::expect_equal(out$year, 2000L)
})

# Write a tiny synthetic CRU-style monthly NetCDF (3 lon x 2 lat x 3 month)
# mirroring the real file's conventions the reader parses: a "days since
# 1900-1-1" time axis holding mid-month day stamps, dimension names lon/lat/
# time, one named variable, and an ncdf4 _FillValue marking ocean cells. Each
# land cell's value encodes its own (lon index, lat index, month) as
# li*100 + lj*10 + month so the reshape/pairing can be asserted exactly, and
# the (lon[1], lat[1]) cell of every month is set to the fill value (ocean).
# Returns the file directory plus the coordinate vectors and encoded months.
#
# NOTE: real CRU stamps sit near the 15th/16th of each month; the exact
# mid-month day is irrelevant to the year/month decode (any day within a month
# decodes to that month), so representative mid-month dates are used.
.cru_fixture_cube <- function(var_name = "tmp", file = NULL) {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  lon <- c(-179.75, -179.25, -178.75)
  lat <- c(-89.75, -89.25)
  encoded <- tibble::tribble(
    ~date, ~year, ~month,
    "2000-01-16", 2000L, 1L,
    "2000-02-15", 2000L, 2L,
    "2001-07-16", 2001L, 7L
  )
  day_stamps <- as.integer(as.Date(encoded$date) - as.Date("1900-01-01"))
  fill <- 9.96921e36
  dim_lon <- ncdf4::ncdim_def("lon", "degrees_east", lon)
  dim_lat <- ncdf4::ncdim_def("lat", "degrees_north", lat)
  dim_time <- ncdf4::ncdim_def("time", "days since 1900-1-1", day_stamps)
  var <- ncdf4::ncvar_def(
    var_name,
    "degrees Celsius",
    list(dim_lon, dim_lat, dim_time),
    missval = fill
  )
  file <- file %||% paste0("cru_ts4.09.1901.2024.", var_name, ".dat.nc")
  path <- file.path(dir, file)
  nc <- ncdf4::nc_create(path, list(var))
  nlon <- length(lon)
  nlat <- length(lat)
  # Column-major array: lon varies fastest, matching as.vector(slab) in the
  # reader. Value = li*100 + lj*10 + month, with (li=1, lj=1) forced to fill.
  vals <- array(dim = c(nlon, nlat, nrow(encoded)))
  for (ti in seq_len(nrow(encoded))) {
    for (li in seq_len(nlon)) {
      for (lj in seq_len(nlat)) {
        vals[li, lj, ti] <- li * 100L + lj * 10L + encoded$month[ti]
      }
    }
    vals[1L, 1L, ti] <- fill
  }
  ncdf4::ncvar_put(nc, var, vals)
  ncdf4::nc_close(nc)
  list(
    dir = dir,
    lon = lon,
    lat = lat,
    year = encoded$year,
    month = encoded$month,
    n_land_per_month = nlon * nlat - 1L
  )
}

test_that("read_cru_climate parses a synthetic CRU NetCDF end to end", {
  cube <- .cru_fixture_cube()
  out <- whep::read_cru_climate(var = "tmp", cru_dir = cube$dir)

  pointblank::expect_col_exists(
    out,
    columns = c("lon", "lat", "year", "month", "value", "var")
  )
  testthat::expect_true(all(out$var == "tmp"))

  # (a) days-since-1900 decode: encoded (year, month) pairs recovered exactly,
  # including the year that rolls over into 2001.
  testthat::expect_setequal(
    paste(out$year, out$month),
    paste(cube$year, cube$month)
  )
  testthat::expect_setequal(out$year, c(2000L, 2001L))
  testthat::expect_setequal(out$month, c(1L, 2L, 7L))

  # (b) lon/lat reshape row count: one row per land cell-month (ocean dropped).
  testthat::expect_equal(nrow(out), cube$n_land_per_month * length(cube$month))

  # (c) ocean fill cell is dropped, not returned as a bogus row.
  testthat::expect_false(any(out$value > 1e30))
  testthat::expect_true(all(is.finite(out$value)))
})

test_that("read_cru_climate reshape pairs lon/lat without transposing", {
  cube <- .cru_fixture_cube()
  out <- whep::read_cru_climate(var = "tmp", cru_dir = cube$dir)

  # Each value encodes li*100 + lj*10 + month; recovering the lon/lat indices
  # from the value and comparing to the reported lon/lat proves the rep()
  # reshape pairs coordinates correctly (a transpose would swap li and lj).
  decoded_li <- out$value %/% 100
  decoded_lj <- (out$value %% 100) %/% 10
  decoded_month <- out$value %% 10
  testthat::expect_equal(out$lon, cube$lon[decoded_li])
  testthat::expect_equal(out$lat, cube$lat[decoded_lj])
  testthat::expect_equal(out$month, decoded_month)

  # The dropped ocean cell is exactly (lon[1], lat[1]); it must never appear.
  ocean <- out$lon == cube$lon[1] & out$lat == cube$lat[1]
  testthat::expect_false(any(ocean))
})

test_that("read_cru_climate slices the requested year from the NetCDF", {
  cube <- .cru_fixture_cube()
  out <- whep::read_cru_climate(var = "tmp", years = 2001, cru_dir = cube$dir)

  testthat::expect_setequal(out$year, 2001L)
  testthat::expect_setequal(out$month, 7L)
  testthat::expect_equal(nrow(out), cube$n_land_per_month)
  # A real synthetic value survives the slice unchanged: (lon[3], lat[2]) in
  # July encodes 3*100 + 2*10 + 7 = 327.
  target <- out[out$lon == cube$lon[3] & out$lat == cube$lat[2], ]
  testthat::expect_equal(nrow(target), 1L)
  testthat::expect_equal(target$value, 327)
})

test_that("read_cru_climate keeps its schema when no requested year exists", {
  cube <- .cru_fixture_cube()
  out <- whep::read_cru_climate(var = "tmp", years = 1999L, cru_dir = cube$dir)

  testthat::expect_equal(nrow(out), 0L)
  testthat::expect_identical(
    names(out),
    c("lon", "lat", "year", "month", "value", "var")
  )
})

test_that("read_cru_climate reads a real CRU file (smoke)", {
  cru_dir <- Sys.getenv("WHEP_CRU_DIR", "")
  testthat::skip_if(!dir.exists(cru_dir))
  out <- whep::read_cru_climate(var = "tmp", years = 2000, cru_dir = cru_dir)
  pointblank::expect_col_exists(
    out,
    columns = c("lon", "lat", "year", "month", "value", "var")
  )
  testthat::expect_true(all(out$lon >= -180 & out$lon <= 180))
  testthat::expect_true(all(out$lat >= -90 & out$lat <= 90))
  testthat::expect_true(all(out$month %in% 1:12))
  testthat::expect_true(all(out$year == 2000L))
  testthat::expect_true(all(is.finite(out$value)))
  mean_temp <- mean(out$value)
  testthat::expect_gt(mean_temp, -40)
  testthat::expect_lt(mean_temp, 40)
})
