# helper_cru_annual.R ---------------------------------------------------------

# A tiny synthetic CRU-style monthly NetCDF holding COMPLETE calendar years, so
# the annual reader in R/livestock_climate.R can be exercised end to end
# without the real archive. Same conventions as the real files: dimensions
# lon/lat/time, a "days since 1900-1-1" time axis with mid-month stamps, one
# named variable, and an ncdf4 `_FillValue` marking ocean cells.
#
# The grid is 2 lon x 2 lat. The (lon[1], lat[1]) cell is ocean in every month,
# leaving three land cells numbered by their position in the column-major
# grid: cell 2 = (lon[2], lat[1]), cell 3 = (lon[1], lat[2]), cell 4 =
# (lon[2], lat[2]).
#
# `value_fn(cell, year, month)` supplies each land cell-month's value and must
# be vectorised over `cell`. `holes` is an optional tibble (`cell`, `year`,
# `month`) of land cell-months to blank with the fill value, which is how a
# partial year is built. Returns the directory and the coordinate vectors.
.cru_annual_cube <- function(
  var_name = "tmp",
  value_fn,
  years = 1901:1904,
  holes = NULL,
  dir = NULL
) {
  testthat::skip_if_not_installed("ncdf4")
  dir <- dir %||% withr::local_tempdir(.local_envir = parent.frame())
  lon <- c(-179.75, -179.25)
  lat <- c(-89.75, -89.25)
  steps <- tidyr::expand_grid(year = as.integer(years), month = 1:12)
  fill <- 9.96921e36
  dims <- list(
    ncdf4::ncdim_def("lon", "degrees_east", lon),
    ncdf4::ncdim_def("lat", "degrees_north", lat),
    ncdf4::ncdim_def(
      "time",
      "days since 1900-1-1",
      as.integer(
        as.Date(sprintf("%d-%02d-16", steps$year, steps$month)) -
          as.Date("1900-01-01")
      )
    )
  )
  var <- ncdf4::ncvar_def(var_name, "synthetic", dims, missval = fill)
  nc <- ncdf4::nc_create(
    file.path(dir, paste0("cru_ts4.09.1901.2024.", var_name, ".dat.nc")),
    list(var)
  )
  vals <- .cru_cube_values(
    steps,
    value_fn,
    holes,
    fill,
    length(lon),
    length(lat)
  )
  ncdf4::ncvar_put(nc, var, vals)
  ncdf4::nc_close(nc)
  list(dir = dir, lon = lon, lat = lat, years = as.integer(years))
}

# The lon x lat x time array: one slab per time step, lon varying fastest, the
# ocean cell and any requested holes set to the fill value.
.cru_cube_values <- function(steps, value_fn, holes, fill, nlon, nlat) {
  cell_of <- \(li, lj) (lj - 1L) * nlon + li
  slabs <- purrr::map2(steps$year, steps$month, \(yr, mo) {
    outer(
      seq_len(nlon),
      seq_len(nlat),
      \(li, lj) value_fn(cell_of(li, lj), yr, mo)
    )
  })
  vals <- array(unlist(slabs), dim = c(nlon, nlat, nrow(steps)))
  vals[1L, 1L, ] <- fill
  if (!is.null(holes)) {
    vals[cbind(
      (holes$cell - 1L) %% nlon + 1L,
      (holes$cell - 1L) %/% nlon + 1L,
      match(paste(holes$year, holes$month), paste(steps$year, steps$month))
    )] <- fill
  }
  vals
}
