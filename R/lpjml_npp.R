# Per-PFT annual carbon reader for the LPJmL run (net primary production and
# harvested carbon), used to build the grassland and natural-land soil carbon
# inputs (Module B, Task B2c-2).
#
# CONFIRMED LPJmL FACTS (run inspected; do not re-guess):
# - pft_npp.nc holds var "NPP" (gC/m2/yr) with dims [lon(720), lat(277),
#   npft(43), time(109)] and a char var "NamePFT" (43 names). time is ANNUAL,
#   "years since 1901-1-1" (index 1 = 1901); NOT the monthly hydrology axis.
# - pft_harvestc.nc holds var "harvestc" (gC/m2/yr) with dims [lon, lat,
#   npft(32), time(109)] and a DIFFERENT NamePFT (32 names, no natural PFTs).
#   Positional band indices differ between the two files, so callers must join
#   on the PFT NAME, never the band position.
# - The per-PFT values are PER STAND (gC per m2 of the PFT's own stand), not per
#   grid cell: verified empirically at year 2000 by reconstructing mnpp.nc's
#   annual total as natfrac*sum(natural pft NPP) + sum(cftfrac*cft NPP) (ratio
#   0.996-1.001 across sampled land cells; the naive 43-band sum overshoots
#   2-20x). The per-stand value IS the per-hectare-of-that-land-use density used
#   by build_grass_natural_carbon_inputs().
# - Run dir from Sys.getenv("WHEP_LPJML_RUN_DIR"); never hardcode a path.

#' Read a per-PFT annual LPJmL carbon variable into a tidy tibble.
#'
#' @description
#' Reads one annual per-plant-functional-type (PFT) carbon output of a finished
#' LPJmL run, either net primary production (`"npp"`, from `pft_npp.nc`) or
#' harvested carbon (`"harvestc"`, from `pft_harvestc.nc`), and returns it in
#' tidy long form with the PFT name attached. Values are per-PFT-stand carbon
#' densities in grams of carbon per square metre per year. Requested years are
#' sliced at read time so the full cube is never materialised. The two files
#' index their PFT bands differently, so downstream code should join on
#' `name_pft`, never on the band position.
#'
#' @param var Logical variable name, `"npp"` (per-PFT net primary production)
#'   or `"harvestc"` (per-PFT harvested carbon).
#' @param years Optional integer vector of calendar years to keep. `NULL` keeps
#'   every year present in the file.
#' @param run_dir Path to the LPJmL run output directory. Defaults to
#'   `Sys.getenv("WHEP_LPJML_RUN_DIR")`.
#' @param first_year First calendar year of the run's annual time axis.
#' @param data Optional pre-read tibble (`lon`, `lat`, `year`, `npft`,
#'   `name_pft`, `value`) used in place of reading NetCDF, for testing.
#' @param example If `TRUE`, return a small fixture instead of reading remote
#'   data. Defaults to `FALSE`.
#' @return A tibble with columns `lon`, `lat`, `year`, `npft` (band index),
#'   `name_pft` (PFT name) and `value` (gC/m2/yr).
#' @export
#' @examples
#' read_lpjml_npp(example = TRUE)
read_lpjml_npp <- function(
  var = c("npp", "harvestc"),
  years = NULL,
  run_dir = NULL,
  first_year = 1901L,
  data = NULL,
  example = FALSE
) {
  if (isTRUE(example)) {
    return(.example_lpjml_npp())
  }
  var <- rlang::arg_match(var)
  long <- data %||%
    .lpjml_npp_read_cube(var, .resolve_run_dir(run_dir), first_year, years)
  .filter_years_if_present(tibble::as_tibble(long), years)
}

# -- Private helpers ----------------------------------------------------------

# Logical variable name -> (file, in-file variable) for the per-PFT carbon
# outputs. NPP carries 43 bands, harvestc 32 (no natural PFTs).
.lpjml_npp_var_map <- function() {
  tibble::tribble(
    ~var, ~file, ~netcdf_var,
    "npp", "pft_npp.nc", "NPP",
    "harvestc", "pft_harvestc.nc", "harvestc"
  )
}

# Read one per-PFT annual carbon variable for the requested years, slicing the
# annual time axis at read time and attaching the PFT names.
.lpjml_npp_read_cube <- function(var, run_dir, first_year, years) {
  rlang::check_installed("ncdf4")
  spec <- .lpjml_npp_var_map()[.lpjml_npp_var_map()$var == var, ]
  path <- file.path(run_dir, spec$file)
  if (!file.exists(path)) {
    cli::cli_abort("LPJmL per-PFT carbon file not found: {.file {path}}.")
  }
  nc <- ncdf4::nc_open(path)
  on.exit(ncdf4::nc_close(nc))
  names_pft <- .lpjml_npp_names(nc)
  keep <- .lpjml_npp_keep_years(nc, first_year, years)
  parts <- purrr::map(keep, function(ti) {
    .lpjml_npp_slice(nc, spec$netcdf_var, names_pft, first_year, ti)
  })
  tibble::as_tibble(data.table::rbindlist(parts))
}

# The PFT names for each band (a [len, npft] char array collapses to a vector).
.lpjml_npp_names <- function(nc) {
  as.character(ncdf4::ncvar_get(nc, "NamePFT"))
}

# Annual time-step indices to read: all, or only the requested calendar years.
.lpjml_npp_keep_years <- function(nc, first_year, years) {
  n_time <- nc$dim[["time"]]$len
  stamp_years <- first_year + seq_len(n_time) - 1L
  if (is.null(years)) {
    seq_len(n_time)
  } else {
    which(stamp_years %in% as.integer(years))
  }
}

# Read one year's [lon, lat, npft] slab and reshape into a long data.table of
# lon, lat, year, npft, name_pft, value.
.lpjml_npp_slice <- function(
  nc,
  netcdf_var,
  names_pft,
  first_year,
  time_index
) {
  lon <- ncdf4::ncvar_get(nc, "lon")
  lat <- ncdf4::ncvar_get(nc, "lat")
  n_pft <- length(names_pft)
  slab <- ncdf4::ncvar_get(
    nc,
    netcdf_var,
    start = c(1L, 1L, 1L, time_index),
    count = c(length(lon), length(lat), n_pft, 1L)
  )
  dt <- data.table::data.table(
    lon = rep(lon, times = length(lat) * n_pft),
    lat = rep(rep(lat, each = length(lon)), times = n_pft),
    npft = rep(seq_len(n_pft), each = length(lon) * length(lat)),
    value = as.vector(slab)
  )
  dt[, year := first_year + time_index - 1L]
  dt[, name_pft := names_pft[npft]]
  dt[is.finite(value), .(lon, lat, year, npft, name_pft, value)]
}
