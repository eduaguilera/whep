# CRU TS 4.09 monthly climate NetCDF reader.
#
# CONFIRMED CRU FACTS (file inspected; do not re-guess):
# - Dir from Sys.getenv("WHEP_CRU_DIR", "C:/XL_files/CRU/CRU_TS_4"). Files are
#   cru_ts4.09.1901.2024.<var>.dat.nc for var in tmp, tmn, tmx, pre, pet, vap,
#   cld, wet, dtr, frs (the 4.09 set, coverage to 2024).
# - Grid: lon[720] -179.75..179.75, lat[360] -89.75..89.75 (global 0.5 deg).
#   time: "days since 1900-1-1", length 1488 = 124 years x 12 months
#   (1901-2024), mid-month stamps. Decode with as.Date(., "1900-01-01").
# - Units DIFFER per variable and are reported, not converted: tmp = degrees
#   Celsius, pre = mm/month, pet = mm/day. The tmp/pre files carry extra vars
#   (stn, mae, maea) so the requested named var must be selected explicitly.
# - _FillValue (~9.97e36) marks ocean cells; only finite land cells are kept.

#' Read a CRU TS 4.09 monthly climate variable into a tidy tibble.
#'
#' @description
#' Reads one monthly variable of the CRU TS 4.09 high-resolution gridded
#' climate dataset (1901-2024, global 0.5 degree) from its NetCDF file and
#' returns it in tidy long form, one row per land cell-month. Requested years
#' are sliced at read time so the full grid is never materialised, and ocean
#' fill cells are dropped. Values are returned in the file's native units (not
#' converted): `tmp`/`tmn`/`tmx`/`dtr` in degrees Celsius, `pre` in mm/month,
#' `pet` in mm/day, `vap` in hPa, `cld` in percent, `wet` and `frs` in days.
#'
#' @param var Variable name, one of `"tmp"`, `"pet"`, `"pre"`, `"tmn"`,
#'   `"tmx"`, `"vap"`, `"cld"`, `"wet"`, `"dtr"` or `"frs"`.
#' @param years Optional integer vector of calendar years to keep. `NULL`
#'   keeps every year present in the file.
#' @param cru_dir Path to the CRU TS NetCDF directory. Defaults to
#'   `Sys.getenv("WHEP_CRU_DIR", "C:/XL_files/CRU/CRU_TS_4")`.
#' @param data Optional pre-read tibble (`lon`, `lat`, `year`, `month`,
#'   `value`) used in place of reading NetCDF, for testing.
#' @param example If `TRUE`, return a small fixture instead of reading data.
#'   Defaults to `FALSE`.
#' @return A tibble with columns `lon`, `lat`, `year`, `month`, `value` (native
#'   units) and `var` (the requested variable name).
#' @source CRU TS 4.09 (Climatic Research Unit, University of East Anglia;
#'   Harris, Osborn & Jones 2020, Scientific Data,
#'   \doi{10.1038/s41597-020-0453-3}).
#' @export
#' @examples
#' read_cru_climate(example = TRUE)
read_cru_climate <- function(
  var = c(
    "tmp",
    "pet",
    "pre",
    "tmn",
    "tmx",
    "vap",
    "cld",
    "wet",
    "dtr",
    "frs"
  ),
  years = NULL,
  cru_dir = NULL,
  data = NULL,
  example = FALSE
) {
  if (example) {
    return(.example_cru_climate())
  }
  var <- rlang::arg_match(var)
  long <- data %||% .cru_read_var(var, .resolve_cru_dir(cru_dir), years)
  long <- .filter_years_if_present(tibble::as_tibble(long), years)
  dplyr::mutate(long, var = var)
}

# Resolve the CRU directory from the argument, else the environment variable
# with the documented local fallback.
.resolve_cru_dir <- function(cru_dir) {
  cru_dir %||% Sys.getenv("WHEP_CRU_DIR", "C:/XL_files/CRU/CRU_TS_4")
}

# On-disk file name for one CRU variable in the 4.09 (1901-2024) set.
.cru_file <- function(var, cru_dir) {
  file.path(cru_dir, paste0("cru_ts4.09.1901.2024.", var, ".dat.nc"))
}

# Read one CRU variable for the requested years, slicing the time dimension at
# read time and dropping ocean fill cells. Returns a long tibble.
.cru_read_var <- function(var, cru_dir, years) {
  rlang::check_installed("ncdf4")
  path <- .cru_file(var, cru_dir)
  if (!file.exists(path)) {
    cli::cli_abort("CRU climate file not found: {.file {path}}.")
  }
  nc <- ncdf4::nc_open(path)
  on.exit(ncdf4::nc_close(nc))
  lon <- ncdf4::ncvar_get(nc, "lon")
  lat <- ncdf4::ncvar_get(nc, "lat")
  stamps <- .cru_time_stamps(nc)
  keep <- .cru_keep_indices(stamps$year, years)
  parts <- purrr::map(keep, function(ti) {
    .cru_slice_to_long(nc, var, lon, lat, ti, stamps$year[ti], stamps$month[ti])
  })
  tibble::as_tibble(data.table::rbindlist(parts))
}

# Decode the monthly time axis ("days since 1900-1-1") into calendar year and
# month vectors, one per time step.
.cru_time_stamps <- function(nc) {
  dates <- as.Date(nc$dim[["time"]]$vals, origin = "1900-01-01")
  list(
    year = as.integer(format(dates, "%Y")),
    month = as.integer(format(dates, "%m"))
  )
}

# Time-step indices to read: all of them, or only those in the requested years.
.cru_keep_indices <- function(stamp_years, years) {
  if (is.null(years)) {
    seq_along(stamp_years)
  } else {
    which(stamp_years %in% as.integer(years))
  }
}

# Read a single month slab and reshape it into a long data.table of finite
# (land) cells: lon, lat, year, month, value.
.cru_slice_to_long <- function(nc, var, lon, lat, time_index, year, month) {
  slab <- ncdf4::ncvar_get(
    nc,
    var,
    start = c(1L, 1L, time_index),
    count = c(length(lon), length(lat), 1L)
  )
  dt <- data.table::data.table(
    lon = rep(lon, times = length(lat)),
    lat = rep(lat, each = length(lon)),
    year = year,
    month = month,
    value = as.vector(slab)
  )
  dt[is.finite(value)]
}
