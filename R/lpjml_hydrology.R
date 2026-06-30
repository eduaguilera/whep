# Generalized LPJmL hydrology NetCDF reader.
#
# CONFIRMED LPJmL FACTS (run inspected; do not re-guess):
# - Gridded NetCDF lon[720] x lat[277] x time, 0.5 deg, monthly, firstyear
#   1901, lastyear 2009 (monthly time len 1308 = 109*12).
# - The in-file variable name differs from the filename:
#     mseepage.nc holds var "seepage" (drainage, mm/month, 1 band)
#     mtransp.nc -> "transp", mevap.nc -> "evap", minterc.nc -> "interc"
#       (AET = transp + evap + interc; there is NO aet, NO pet, NO temperature
#       output)
#     mprec.nc -> "prec", mrain.nc -> "rain", mirrig.nc -> "irrig",
#       mrunoff.nc -> "runoff", mdischarge.nc -> "discharge"
#     mswc.nc -> "SWC" (4-D lon x lat x layer[6] x time, fractional saturation
#       0-1; layers at 200/500/1000/2000/3000/13000 mm).
#     mcft_nir.nc -> "cft_nir" (net irrigation requirement, mm/month, per-CFT
#       band dimension; the blue-water net-demand BW_net analogue).
# - Monthly time index for year y, month m = (y - first_year) * 12 + m.
# - Local dev run dir is read from Sys.getenv("WHEP_LPJML_RUN_DIR"); never
#   hardcode an absolute path in committed code.

#' Read an LPJmL hydrology variable into a tidy tibble.
#'
#' @description
#' Reads one monthly LPJmL hydrology output (drainage, evapotranspiration
#' components, precipitation, irrigation, runoff, discharge or soil water
#' content) from a finished run's NetCDF files and returns it in tidy long
#' form. The logical `var` name is mapped to the on-disk file and in-file
#' variable name, so callers need not know the LPJmL naming quirks. The
#' synthetic `"aet"` variable sums the three actual-evapotranspiration
#' components (transpiration, evaporation, interception).
#'
#' @param var Logical variable name, one of `"drainage"`, `"transp"`,
#'   `"evap"`, `"interc"`, `"aet"`, `"prec"`, `"rain"`, `"irrig"`, `"runoff"`,
#'   `"discharge"`, `"swc"` or `"cft_nir"` (per-CFT net irrigation requirement).
#' @param run_dir Path to the LPJmL run output directory. Defaults to
#'   `Sys.getenv("WHEP_LPJML_RUN_DIR")`.
#' @param years Optional integer vector of calendar years to keep. `NULL`
#'   keeps every year present in the file.
#' @param first_year First calendar year of the run's monthly time axis.
#' @param monthly If `TRUE`, return one row per cell-month; if `FALSE`,
#'   aggregate the 12 months of each year per cell (flux variables summed,
#'   soil water content averaged).
#' @param agg Annual aggregation for `monthly = FALSE`, `"sum"` (flux default)
#'   or `"mean"` (soil-water default).
#' @param data Optional pre-read tibble (`lon`, `lat`, `year`, `month`,
#'   `value`, and `layer` for `"swc"`) used in place of reading NetCDF, for
#'   testing.
#' @param example If `TRUE`, return a small fixture instead of reading remote
#'   data. Defaults to `FALSE`.
#' @return A tibble with columns `lon`, `lat`, `year`, `value` (plus `month`
#'   when `monthly = TRUE`, and `layer` for `"swc"`).
#' @export
#' @examples
#' read_lpjml_hydrology(example = TRUE)
read_lpjml_hydrology <- function(
  var = c(
    "drainage",
    "transp",
    "evap",
    "interc",
    "aet",
    "prec",
    "rain",
    "irrig",
    "runoff",
    "discharge",
    "swc",
    "cft_nir"
  ),
  run_dir = NULL,
  years = NULL,
  first_year = 1901L,
  monthly = TRUE,
  agg = c("sum", "mean"),
  data = NULL,
  example = FALSE
) {
  if (example) {
    return(.example_lpjml_hydrology())
  }
  var <- rlang::arg_match(var)
  agg <- if (var == "swc" && missing(agg)) "mean" else rlang::arg_match(agg)
  long <- data %||% .read_hydro_cube(var, .resolve_run_dir(run_dir), first_year)
  long <- .filter_years_if_present(long, years)
  if (monthly) long else .aggregate_hydro_annual(long, var, agg)
}

# Logical name -> (file, in-file variable) for each LPJmL hydrology output.
.hydro_var_map <- function() {
  tibble::tribble(
    ~var, ~file, ~netcdf_var,
    "drainage", "mseepage.nc", "seepage",
    "transp", "mtransp.nc", "transp",
    "evap", "mevap.nc", "evap",
    "interc", "minterc.nc", "interc",
    "prec", "mprec.nc", "prec",
    "rain", "mrain.nc", "rain",
    "irrig", "mirrig.nc", "irrig",
    "runoff", "mrunoff.nc", "runoff",
    "discharge", "mdischarge.nc", "discharge",
    "swc", "mswc.nc", "SWC",
    "cft_nir", "mcft_nir.nc", "cft_nir"
  )
}

# Resolve the run directory from the argument, else the environment variable.
.resolve_run_dir <- function(run_dir) {
  resolved <- run_dir %||% Sys.getenv("WHEP_LPJML_RUN_DIR")
  if (!.has_path(resolved)) {
    # TODO: register a pinned LPJmL-hydrology alias and read it via
    # whep_read_file() here once one exists; do not invent a pin name.
    cli::cli_abort(c(
      "No LPJmL run directory available.",
      i = "Pass {.arg run_dir} or set {.envvar WHEP_LPJML_RUN_DIR}."
    ))
  }
  resolved
}

# Read one logical hydrology variable into a long tibble. The synthetic "aet"
# sums its three actual-evapotranspiration components per cell-month.
.read_hydro_cube <- function(var, run_dir, first_year) {
  rlang::check_installed("ncdf4")
  if (var == "aet") {
    return(.read_aet_cube(run_dir, first_year))
  }
  spec <- .hydro_var_map()[.hydro_var_map()$var == var, ]
  .read_hydro_one(file.path(run_dir, spec$file), spec$netcdf_var, first_year)
}

# Sum transpiration + evaporation + interception into actual evapotranspiration.
.read_aet_cube <- function(run_dir, first_year) {
  components <- c("transp", "evap", "interc")
  spec <- .hydro_var_map()
  parts <- purrr::map(components, function(component) {
    row <- spec[spec$var == component, ]
    .read_hydro_one(file.path(run_dir, row$file), row$netcdf_var, first_year)
  })
  data.table::rbindlist(parts) |>
    data.table::as.data.table() |>
    (\(dt) dt[, .(value = sum(value)), by = .(lon, lat, year, month)])() |>
    tibble::as_tibble()
}

# Read a single LPJmL monthly NetCDF (3-D, or 4-D for soil water content) into
# a long tibble: lon, lat, year, month, value (plus layer when 4-D).
.read_hydro_one <- function(path, netcdf_var, first_year) {
  if (!file.exists(path)) {
    cli::cli_abort("LPJmL hydrology file not found: {.file {path}}.")
  }
  nc <- ncdf4::nc_open(path)
  on.exit(ncdf4::nc_close(nc))
  lon <- ncdf4::ncvar_get(nc, "lon")
  lat <- ncdf4::ncvar_get(nc, "lat")
  slab <- ncdf4::ncvar_get(nc, netcdf_var)
  n_time <- nc$dim[["time"]]$len
  dt <- .hydro_slab_to_long(slab, lon, lat, n_time, first_year)
  tibble::as_tibble(dt)
}

# Reshape a (lon, lat, [layer,] time) array into a long data.table with the
# monthly time axis decomposed into calendar year and month.
.hydro_slab_to_long <- function(slab, lon, lat, n_time, first_year) {
  has_layer <- length(dim(slab)) == 4L
  n_layer <- if (has_layer) dim(slab)[3] else 1L
  dt <- data.table::data.table(
    lon = rep(lon, times = length(lat) * n_layer * n_time),
    lat = rep(rep(lat, each = length(lon)), times = n_layer * n_time),
    layer = rep(seq_len(n_layer), each = length(lon) * length(lat)),
    time_index = rep(
      seq_len(n_time),
      each = length(lon) * length(lat) * n_layer
    ),
    value = as.vector(slab)
  )
  dt[, year := first_year + (time_index - 1L) %/% 12L]
  dt[, month := ((time_index - 1L) %% 12L) + 1L]
  if (has_layer) {
    dt[, .(lon, lat, year, month, layer, value)]
  } else {
    dt[, .(lon, lat, year, month, value)]
  }
}

# Aggregate the 12 monthly values of each year per cell (and layer for swc):
# flux variables sum, soil water content means.
.aggregate_hydro_annual <- function(long, var, agg) {
  group_cols <- if (rlang::has_name(long, "layer")) {
    c("lon", "lat", "year", "layer")
  } else {
    c("lon", "lat", "year")
  }
  reducer <- if (agg == "mean") base::mean else base::sum
  dplyr::summarise(
    long,
    value = reducer(value),
    .by = dplyr::all_of(group_cols)
  )
}

# Toy fixture for the runnable example (one cell, two months of drainage).
.example_lpjml_hydrology <- function() {
  tibble::tribble(
    ~lon, ~lat, ~year, ~month, ~value,
    -179.75, 0.25, 1901L, 1L, 1.2,
    -179.75, 0.25, 1901L, 2L, 0.8
  )
}
