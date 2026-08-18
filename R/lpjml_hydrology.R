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
#     cft_nir.nc -> "nir" (net irrigation requirement, mm/yr, per-CFT band
#       dimension; the blue-water net-demand BW_net analogue). NOT "mcft_nir.nc"
#       holding "cft_nir" monthly, as this map claimed until 2026-08: no WHEP
#       run has ever written that file. Checked every run under
#       LPJmL_runs/ (nine runs, 5.9.7 and 6.1.1 alike): all write cft_nir.nc,
#       var "nir", nstep 1, 32 bands. The wrong entry never surfaced because
#       nothing calls this variable yet (see the TODO in R/water_balance.R).
#     cft_consump_water_b.nc holds var "consump_water_b", and
#       cft_consump_water_g.nc holds "consump_water_g" (consumptive blue and
#       green water, mm/yr, per-CFT band). Inspected in a completed 6.1.1
#       run: these are ANNUAL outputs (nstep 1, timestep 1, units mm/yr), not
#       monthly, so their time axis is one step per year, not twelve.
# - The per-CFT cubes carry 32 bands, named in the file's NamePFT variable:
#   bands 1-16 rainfed, 17-32 irrigated, in the same crop order; "rainfed
#   grassland" is band 14 and "irrigated grassland" band 30. Select bands by
#   name (band_name) rather than by index, so a run configured with a different
#   band set fails loudly instead of silently charging the wrong crop.
# - Monthly time index for year y, month m = (y - first_year) * 12 + m; annual
#   variables index simply as y - first_year + 1.
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
#'   `"discharge"`, `"swc"`, `"cft_nir"` (per-CFT net irrigation requirement)
#'   or the per-CFT consumptive-water cubes `"cft_consump_water_b"` (blue) and
#'   `"cft_consump_water_g"` (green). The per-CFT variables keep their `band`
#'   dimension, and carry `band_name` when the file names its bands.
#' @param run_dir Path to the LPJmL run output directory. Defaults to
#'   `Sys.getenv("WHEP_LPJML_RUN_DIR")`.
#' @param years Optional integer vector of calendar years to keep. `NULL`
#'   keeps every year present in the file.
#' @param first_year First calendar year of the run's monthly time axis.
#' @param monthly If `TRUE`, return one row per cell-month; if `FALSE`,
#'   aggregate the 12 months of each year per cell (flux variables summed,
#'   soil water content averaged). Immaterial for the annual per-CFT
#'   consumptive-water variables, which LPJmL writes one step per year: they
#'   carry no `month` column either way, and aggregating them groups rows that
#'   are already one per cell-year-band.
#' @param agg Annual aggregation for `monthly = FALSE`, `"sum"` (flux default)
#'   or `"mean"` (soil-water default).
#' @param data Optional pre-read tibble (`lon`, `lat`, `year`, `month`,
#'   `value`, plus `layer` for `"swc"` or `band` for `"cft_nir"`) used in
#'   place of reading NetCDF, for testing.
#' @param example If `TRUE`, return a small fixture instead of reading remote
#'   data. Defaults to `FALSE`.
#' @return A tibble with columns `lon`, `lat`, `year`, `value` (plus `month`
#'   for the monthly variables when `monthly = TRUE`, `layer` for `"swc"`, and
#'   `band` plus `band_name` for the per-CFT variables). The annual per-CFT
#'   consumptive-water variables never carry `month`.
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
    "cft_nir",
    "cft_consump_water_b",
    "cft_consump_water_g"
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
  long <- data %||%
    .read_hydro_cube(var, .resolve_run_dir(run_dir), first_year, years)
  long <- .hydro_name_band(long, var)
  long <- .filter_years_if_present(long, years)
  if (monthly) long else .aggregate_hydro_annual(long, var, agg)
}

# Logical name -> (file, in-file variable, time steps per year) for each LPJmL
# hydrology output. `steps_per_year` is 12 for the monthly outputs and 1 for the
# annual per-CFT consumptive-water cubes (see the header facts).
.hydro_var_map <- function() {
  tibble::tribble(
    ~var, ~file, ~netcdf_var, ~steps_per_year,
    "drainage", "mseepage.nc", "seepage", 12L,
    "transp", "mtransp.nc", "transp", 12L,
    "evap", "mevap.nc", "evap", 12L,
    "interc", "minterc.nc", "interc", 12L,
    "prec", "mprec.nc", "prec", 12L,
    "rain", "mrain.nc", "rain", 12L,
    "irrig", "mirrig.nc", "irrig", 12L,
    "runoff", "mrunoff.nc", "runoff", 12L,
    "discharge", "mdischarge.nc", "discharge", 12L,
    "swc", "mswc.nc", "SWC", 12L,
    "cft_nir", "cft_nir.nc", "nir", 1L,
    "cft_consump_water_b", "cft_consump_water_b.nc", "consump_water_b", 1L,
    "cft_consump_water_g", "cft_consump_water_g.nc", "consump_water_g", 1L
  )
}

# The logical variables whose third dimension is a per-CFT band rather than a
# soil layer.
.hydro_band_vars <- function() {
  c("cft_nir", "cft_consump_water_b", "cft_consump_water_g")
}

# Time steps per year for a logical variable; 12 (monthly) unless mapped
# otherwise. The synthetic "aet" is built from monthly components.
.hydro_steps_per_year <- function(var) {
  if (var == "aet") {
    return(12L)
  }
  spec <- .hydro_var_map()
  spec$steps_per_year[[match(var, spec$var)]]
}

# LPJmL 6.x renames some output variables to their CF short names, so the name
# a file actually carries depends on the model version that wrote it. Known
# renames, keyed by the 5.x name this package's map records: in `mprec.nc`, the
# variable 5.x calls `prec` is named `pr` in 6.x.
#
# Only `prec` is affected among the variables read here; the other nine are
# byte-identical in name between 5.9.7 and 6.1.1, verified against completed
# runs of both.
.hydro_var_aliases <- function() {
  list(
    prec = "pr",
    nir = "cft_nir",
    consump_water_b = "cft_consump_water_b",
    consump_water_g = "cft_consump_water_g"
  )
}

# The in-file name for a mapped variable, tolerating the 6.x renames.
#
# Resolved per file rather than per version because there is no version stamp to
# branch on: a run directory is just NetCDF files, and both versions' output can
# sit side by side on one machine. Checking what the file contains works for
# either without asking the caller which model wrote it.
#
# An unresolvable name lists what the file does contain, so the next rename
# costs one run to diagnose rather than a hunt -- the failure it replaces was
# `argument is of length zero` from `nc$var[[netcdf_var]]$ndims`, which names
# neither the variable nor the file.
.hydro_resolve_var <- function(nc, netcdf_var, path) {
  present <- names(nc$var)
  if (netcdf_var %in% present) {
    return(netcdf_var)
  }
  alias <- intersect(.hydro_var_aliases()[[netcdf_var]], present)
  if (length(alias) > 0L) {
    return(alias[[1L]])
  }
  cli::cli_abort(c(
    "Variable {.field {netcdf_var}} not found in {.file {path}}.",
    i = "The file contains: {.field {present}}.",
    i = "If LPJmL renamed it, add the new name to {.fun .hydro_var_aliases}."
  ))
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
# sums its three actual-evapotranspiration components per cell-month. `years`
# (when supplied) is forwarded so only the covering NetCDF time slice is read
# (see .read_hydro_one()).
.read_hydro_cube <- function(var, run_dir, first_year, years = NULL) {
  rlang::check_installed("ncdf4")
  if (var == "aet") {
    return(.read_aet_cube(run_dir, first_year, years))
  }
  spec <- .hydro_var_map()[.hydro_var_map()$var == var, ]
  .read_hydro_one(
    file.path(run_dir, spec$file),
    spec$netcdf_var,
    first_year,
    years,
    spec$steps_per_year
  )
}

# Sum transpiration + evaporation + interception into actual evapotranspiration.
.read_aet_cube <- function(run_dir, first_year, years = NULL) {
  components <- c("transp", "evap", "interc")
  spec <- .hydro_var_map()
  parts <- purrr::map(components, function(component) {
    row <- spec[spec$var == component, ]
    .read_hydro_one(
      file.path(run_dir, row$file),
      row$netcdf_var,
      first_year,
      years,
      row$steps_per_year
    )
  })
  data.table::rbindlist(parts) |>
    data.table::as.data.table() |>
    (\(dt) dt[, .(value = sum(value)), by = .(lon, lat, year, month)])() |>
    tibble::as_tibble()
}

# Read a single LPJmL monthly NetCDF (3-D, or 4-D for soil water content) into
# a long tibble: lon, lat, year, month, value (plus layer when 4-D).
#
# When `years` is supplied, only the contiguous NetCDF time slice covering
# min(years):max(years) is fetched via ncvar_get(start=, count=), instead of
# reading the entire (multi-decade, potentially 4-D) cube into memory. This is
# a partial optimization for the common single-year/contiguous-range case: a
# non-contiguous `years` vector (e.g. c(1920, 1950)) still over-fetches the
# full 1920:1950 range at the NetCDF level, and .filter_years_if_present()
# (called by the caller after this returns) narrows it down to the exact
# requested years as a post-hoc safety net -- a deliberate, acceptable scope
# boundary, not a full general solution.
.read_hydro_one <- function(
  path,
  netcdf_var,
  first_year,
  years = NULL,
  steps_per_year = 12L
) {
  if (!file.exists(path)) {
    cli::cli_abort("LPJmL hydrology file not found: {.file {path}}.")
  }
  nc <- ncdf4::nc_open(path)
  on.exit(ncdf4::nc_close(nc))
  netcdf_var <- .hydro_resolve_var(nc, netcdf_var, path)
  lon <- ncdf4::ncvar_get(nc, "lon")
  lat <- ncdf4::ncvar_get(nc, "lat")
  slice <- .hydro_time_slice(
    nc,
    netcdf_var,
    first_year,
    years,
    steps_per_year
  )
  # collapse_degen = FALSE keeps length-1 dimensions. Without it, slicing a
  # single year out of an annual per-CFT cube drops the time axis, leaving a
  # 3-D slab whose band dimension the decoder would read as time -- silently
  # scrambling crop bands into years. Monthly cubes never hit this (a one-year
  # slice is still 12 steps), which is why it only surfaced with annual data.
  slab <- if (is.null(slice)) {
    ncdf4::ncvar_get(nc, netcdf_var, collapse_degen = FALSE)
  } else {
    ncdf4::ncvar_get(
      nc,
      netcdf_var,
      start = slice$start,
      count = slice$count,
      collapse_degen = FALSE
    )
  }
  n_time <- if (is.null(slice)) nc$dim[["time"]]$len else slice$n_steps
  slab_first_year <- if (is.null(slice)) first_year else slice$slab_first_year
  dt <- .hydro_slab_to_long(
    slab,
    lon,
    lat,
    n_time,
    slab_first_year,
    steps_per_year
  )
  .hydro_attach_band_names(tibble::as_tibble(dt), nc)
}

# Attach the file's own band names to a per-CFT cube, so callers select a band
# by name ("rainfed grassland") instead of by a positional index that a
# differently configured run would silently redefine. Runs here at the raw
# decoder's `layer` name, before .hydro_name_band() renames it to `band`.
# Files without the NamePFT variable (every non-CFT output, soil water content
# included) pass through unchanged.
.hydro_attach_band_names <- function(long, nc) {
  if (!rlang::has_name(long, "layer") || !("NamePFT" %in% names(nc$var))) {
    return(long)
  }
  names_pft <- as.character(ncdf4::ncvar_get(nc, "NamePFT"))
  dplyr::mutate(long, band_name = names_pft[.data$layer])
}

# Compute the ncvar_get() start=/count= slice covering min(years):max(years),
# plus the adjusted "first year" of the resulting slab (needed so
# .hydro_slab_to_long() decodes year/month correctly for a slab that no
# longer starts at the file's own first_year). Returns NULL when years is
# NULL (caller then falls back to the full, unsliced read).
.hydro_time_slice <- function(
  nc,
  netcdf_var,
  first_year,
  years,
  steps_per_year = 12L
) {
  if (is.null(years)) {
    return(NULL)
  }
  slab_first_year <- min(years)
  last_year <- max(years)
  time_start <- (slab_first_year - first_year) * steps_per_year + 1L
  n_steps <- (last_year - slab_first_year + 1L) * steps_per_year
  ndims <- nc$var[[netcdf_var]]$ndims
  if (ndims == 4L) {
    start <- c(1, 1, 1, time_start)
    count <- c(-1, -1, -1, n_steps)
  } else {
    start <- c(1, 1, time_start)
    count <- c(-1, -1, n_steps)
  }
  list(
    start = start,
    count = count,
    n_steps = n_steps,
    slab_first_year = slab_first_year
  )
}

# Reshape a (lon, lat, [layer,] time) array into a long data.table with the
# time axis decomposed into calendar year and, for monthly cubes, month.
# Annual cubes (steps_per_year = 1) carry one step per year and so get no
# month column: there is no month to report, and inventing one would let a
# caller sum twelve copies of an annual mm/yr value.
.hydro_slab_to_long <- function(
  slab,
  lon,
  lat,
  n_time,
  first_year,
  steps_per_year = 12L
) {
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
  dt[, year := first_year + (time_index - 1L) %/% steps_per_year]
  if (steps_per_year == 1L) {
    return(.hydro_select_long(dt, has_layer, monthly = FALSE))
  }
  dt[, month := ((time_index - 1L) %% steps_per_year) + 1L]
  .hydro_select_long(dt, has_layer, monthly = TRUE)
}

# Select the long-form output columns, keeping `layer` only for cubes that have
# one and `month` only for monthly cubes.
.hydro_select_long <- function(dt, has_layer, monthly) {
  keep <- c(
    "lon",
    "lat",
    "year",
    if (monthly) "month",
    if (has_layer) "layer",
    "value"
  )
  dt[, .SD, .SDcols = keep]
}

# The generic 4-D decoder calls the third dimension `layer`, which is correct
# for SWC but not for the per-CFT cft_nir cube. Give that dimension its public
# `band` name before returning or aggregating; injected data already using
# `band` passes through unchanged.
.hydro_name_band <- function(long, var) {
  if (
    var %in%
      .hydro_band_vars() &&
      rlang::has_name(long, "layer") &&
      !rlang::has_name(long, "band")
  ) {
    return(dplyr::rename(long, band = "layer"))
  }
  long
}

# Aggregate the 12 monthly values of each year per cell and third-dimension
# member (`layer` for SWC, `band` for cft_nir): flux variables sum, soil water
# content means.
.aggregate_hydro_annual <- function(long, var, agg) {
  group_cols <- c(
    "lon",
    "lat",
    "year",
    intersect(c("layer", "band", "band_name"), names(long))
  )
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
