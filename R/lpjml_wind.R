# Gridded monthly windspeed from the GSWP3-W5E5 forcing used to drive LPJmL.
#
# CONFIRMED FACTS (local file inspected; do not re-guess):
# - Single consolidated file "wind_gswp3-w5e5_1901_2016_monthly.nc" holding
#   one variable wind[longitude,latitude,time], float,
#   _FillValue = -1.17549402418441e+38.
# - longitude size 720 (degrees_east), latitude size 360 (degrees_north):
#   already WHEP's native 0.5-degree grid (geotransform
#   "-180 0.5 0 90 0 -0.5"), no fine-to-coarse aggregation needed.
# - The geotransform's row step is -0.5, i.e. the latitude coordinate
#   variable itself is north-to-south; read it directly with
#   ncdf4::ncvar_get() rather than assuming the order.
# - time size 1392, units "days since 1970-1-1", standard Gregorian
#   calendar, 1901-2016 monthly (116*12 = 1392); convert with
#   as.Date(time_vals, origin = "1970-01-01") then extract year/month.
# - The wind variable carries no "units" attribute in this file, because the
#   monthly means were written by terra, which dropped it. The unit is m/s,
#   and that is now VERIFIED rather than assumed: the ISIMIP2a source the
#   file is derived from declares wind:units = "m s-1" and
#   wind:standard_name = "wind_speed" (checked against every 1901-2016 chunk
#   at https://files.isimip.org/ISIMIP2a/InputData/climate_co2/climate/HistObs/GSWP3-W5E5/,
#   issue #371). inst/scripts/fetch_isimip_wind.sh rebuilds the series from
#   those files and validation/lpjml_wind_provenance.R audits the pin against
#   them; run_lpjml.R declares the same unit as input.wind.unit = "m/s".
# - Local dev data dir is read from Sys.getenv("WHEP_WIND_DIR"); never
#   hardcode an absolute path in committed code.

#' Read gridded LPJmL-forcing windspeed onto WHEP's grid.
#'
#' @description
#' Reads the GSWP3-W5E5 monthly windspeed forcing used to drive LPJmL
#' (single consolidated NetCDF, already on WHEP's native 0.5-degree grid) and
#' returns it in tidy long form. The file's `wind` variable carries no
#' `units` attribute, because the monthly aggregation dropped it; the unit is
#' metres per second, taken from the ISIMIP2a GSWP3-W5E5 daily source the file
#' is derived from, which declares `units = "m s-1"` and `standard_name =
#' "wind_speed"`. That is why the output column is named `windspeed_ms`.
#'
#' @param years Optional integer vector of calendar years to keep. `NULL`
#'   reads every year present in the file (1901-2016).
#' @param wind_dir Path to the directory holding
#'   `wind_gswp3-w5e5_1901_2016_monthly.nc`. Defaults to
#'   `Sys.getenv("WHEP_WIND_DIR")`.
#' @param example If `TRUE`, return a small fixture instead of reading data.
#'   Defaults to `FALSE`.
#' @return A tibble with `lon`, `lat`, `year`, `month`, `windspeed_ms`.
#' @export
#' @examples
#' read_lpjml_wind(example = TRUE)
read_lpjml_wind <- function(years = NULL, wind_dir = NULL, example = FALSE) {
  if (isTRUE(example)) {
    return(.example_lpjml_wind())
  }
  rlang::check_installed("ncdf4")
  path <- file.path(
    .resolve_wind_dir(wind_dir),
    "wind_gswp3-w5e5_1901_2016_monthly.nc"
  )
  .read_wind_nc(path, years)
}

# ---- Private helpers --------------------------------------------------

# Resolve the wind forcing data directory from the argument, else the env
# var.
.resolve_wind_dir <- function(wind_dir) {
  resolved <- wind_dir %||% Sys.getenv("WHEP_WIND_DIR")
  if (!.has_path(resolved)) {
    cli::cli_abort(c(
      "No LPJmL wind forcing directory available.",
      i = "Pass {.arg wind_dir} or set {.envvar WHEP_WIND_DIR}."
    ))
  }
  resolved
}

# Read the consolidated windspeed NetCDF, slicing the time dimension to the
# requested years, and drop _FillValue cells.
.read_wind_nc <- function(path, years) {
  if (!file.exists(path)) {
    cli::cli_abort("LPJmL wind forcing file not found: {.file {path}}.")
  }
  nc <- ncdf4::nc_open(path)
  on.exit(ncdf4::nc_close(nc))
  lon <- ncdf4::ncvar_get(nc, "longitude")
  lat <- ncdf4::ncvar_get(nc, "latitude")
  time_dates <- as.Date(
    ncdf4::ncvar_get(nc, "time"),
    origin = "1970-01-01"
  )
  time_idx <- .wind_time_index(time_dates, years)
  if (length(time_idx) == 0L) {
    return(tibble::tibble(
      lon = double(),
      lat = double(),
      year = integer(),
      month = integer(),
      windspeed_ms = double()
    ))
  }
  fill_value <- ncdf4::ncatt_get(nc, "wind", "_FillValue")$value
  layers <- lapply(time_idx, function(ti) {
    .read_wind_month(nc, lon, lat, time_dates[ti], ti, fill_value)
  })
  data.table::rbindlist(layers) |> tibble::as_tibble()
}

# Map requested calendar years to time-axis indices; NULL keeps every step.
.wind_time_index <- function(time_dates, years) {
  if (is.null(years)) {
    seq_along(time_dates)
  } else {
    which(as.integer(format(time_dates, "%Y")) %in% years)
  }
}

# Read one native-grid monthly slab, dropping _FillValue cells as NA.
.read_wind_month <- function(nc, lon, lat, date, time_idx, fill_value) {
  slab <- ncdf4::ncvar_get(
    nc,
    "wind",
    start = c(1, 1, time_idx),
    count = c(-1, -1, 1)
  )
  slab[slab == fill_value] <- NA_real_
  data.table::data.table(
    lon = rep(lon, times = length(lat)),
    lat = rep(lat, each = length(lon)),
    year = as.integer(format(date, "%Y")),
    month = as.integer(format(date, "%m")),
    windspeed_ms = as.vector(slab)
  ) |>
    (\(dt) dt[!is.na(windspeed_ms)])()
}

# Toy fixture for a runnable example (one cell, one month).
.example_lpjml_wind <- function() {
  tibble::tribble(
    ~lon, ~lat, ~year, ~month, ~windspeed_ms,
    -179.75, 89.75, 1901L, 1L, 3.2
  )
}
