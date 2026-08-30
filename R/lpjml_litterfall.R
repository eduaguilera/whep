# Litterfall by land-use class from LPJmL. Read separately from the hydrology
# cube because these are annual whole-cell densities, not monthly water
# variables, and separately from pft_npp because they carry no PFT band.

#' Read litterfall by land-use class from an LPJmL run.
#'
#' @description
#' Per-cell, per-year carbon returned to the soil as litter, split by the
#' stand that shed it. Litterfall is what physically enters the soil;
#' net primary production is not, because it also contains the increment
#' that stays in living biomass. On this run natural litterfall is 0.84 times
#' natural NPP at the median cell and 0.68 in aggregate, the shortfall being
#' biomass accumulation, fire, and the conversion pulse.
#'
#' @section Whole-cell densities:
#' Every class is a density per square metre of GRIDCELL, not of its own
#' stand: each accumulation site in LPJmL multiplies by `stand->frac`. To get
#' the density a stand actually experiences, divide by that stand's area
#' fraction -- for `"nv"`, `natural_stand_frac` from
#' [read_lpjml_natural_cover()]. Mixing the two conventions understates a
#' partly-natural cell's input by exactly its natural fraction.
#'
#' @section The four classes sum to the total:
#' `nv + luc + agr + mgrass` recovers `total` to float32 rounding, the
#' remainder being set-aside grass. `"luc"` is the pulse released when land is
#' converted, and it is large -- 9.9% of all litterfall at 2010 against 1.3%
#' for crop stands. It is booked to no destination: at `landusechange.c` the
#' receiving stand is still natural when the pulse is credited, so
#' apportioning it to the land that gained area is a decision for the caller,
#' not a quantity LPJmL reports.
#'
#' @param class Which stand's litterfall to read: `"nv"` (standing natural
#'   vegetation), `"agr"` (crop stands), `"mgrass"` (managed grassland),
#'   `"luc"` (the land-use conversion pulse) or `"total"`.
#' @param run_dir Path to the LPJmL run output directory. `NULL` (default)
#'   uses `WHEP_LPJML_RUN_DIR`.
#' @param years Optional integer vector of calendar years to keep. `NULL`
#'   (default) keeps every year the file covers.
#' @param first_year Calendar year of the file's first time step.
#' @param example If `TRUE`, return a small fixture instead of reading a run.
#'   Defaults to `FALSE`.
#' @return A tibble with `lon`, `lat`, `year`, `class` and
#'   `litterfall_c_mgc_ha_yr`.
#' @source Schaphoff, S. et al. (2018). LPJmL4 - a dynamic global vegetation
#'   model with managed land - Part 1: Model description. *Geoscientific Model
#'   Development*, 11, 1343-1375. \doi{10.5194/gmd-11-1343-2018}.
#' @export
#' @examples
#' read_lpjml_litterfall(example = TRUE)
read_lpjml_litterfall <- function(
  class = c("nv", "agr", "mgrass", "luc", "total"),
  run_dir = NULL,
  years = NULL,
  first_year = 1901L,
  example = FALSE
) {
  class <- rlang::arg_match(class)
  if (isTRUE(example)) {
    return(.example_lpjml_litterfall())
  }
  rlang::check_installed("ncdf4")
  path <- file.path(.resolve_run_dir(run_dir), .litfall_file(class))
  if (!file.exists(path)) {
    output <- .litfall_output(class)
    cli::cli_abort(c(
      "LPJmL litterfall file not found: {.file {path}}.",
      i = "It is the {.val {output}} output; add it to the run's output list.",
      i = "Runs before 2026-08-27 carry {.val litfallc} only, and their
           {.val litfallc_agr} is prefill rather than data."
    ))
  }
  nc <- ncdf4::nc_open(path)
  on.exit(ncdf4::nc_close(nc))
  .litfall_read(nc, class, years, first_year)
}

# One file per class; the total keeps LPJmL's own unsuffixed name.
.litfall_file <- function(class) {
  paste0(.litfall_output(class), ".nc")
}

.litfall_output <- function(class) {
  if (class == "total") "litfallc" else paste0("litfallc_", class)
}

# The NetCDF variable is found by elimination rather than by name. The four
# class files do not agree on their own convention -- litfallc_agr.nc holds
# `ALITFALLC_agr` where its three siblings hold their filename -- so keying on
# the name would fail on exactly one of them, and silently gain a second such
# file the next time an output is added upstream.
.litfall_var <- function(nc) {
  var <- setdiff(
    names(nc$var),
    c("lon_bnds", "lat_bnds", "time_bnds", "NamePFT")
  )
  if (length(var) == 0L) {
    cli::cli_abort("Litterfall file holds no data variable.")
  }
  var[which.max(purrr::map_int(var, \(v) length(nc$var[[v]]$size)))]
}

.litfall_read <- function(nc, class, years, first_year) {
  lon <- ncdf4::ncvar_get(nc, "lon")
  lat <- ncdf4::ncvar_get(nc, "lat")
  # Sizes are captured BEFORE the tibble. Inside tibble() a later expression
  # sees the columns already defined, so `length(lon)` there would be the
  # 199440-row lon COLUMN rather than the 720-value axis, and the grid would
  # silently expand to 55 million rows.
  nlon <- length(lon)
  nlat <- length(lat)
  var <- .litfall_var(nc)
  keep <- .fpc_year_index(years, first_year, nc$dim$time$len)
  purrr::list_rbind(purrr::map(keep, \(i) {
    a <- ncdf4::ncvar_get(
      nc,
      var,
      start = c(1, 1, i),
      count = c(nlon, nlat, 1)
    )
    tibble::tibble(
      lon = rep(lon, times = nlat),
      lat = rep(lat, each = nlon),
      year = first_year + i - 1L,
      class = class,
      # LPJmL writes grams of carbon per square metre per year; WHEP works in
      # megagrams per hectare per year, so one gram per square metre is a
      # hundredth of a megagram per hectare.
      litterfall_c_mgc_ha_yr = as.vector(a) * 0.01
    ) |>
      dplyr::filter(is.finite(.data$litterfall_c_mgc_ha_yr))
  }))
}

# Toy fixture for a runnable example: two natural cells, a wet one shedding
# heavily and an arid one shedding almost nothing.
.example_lpjml_litterfall <- function() {
  tibble::tribble(
    ~lon, ~lat, ~year, ~class, ~litterfall_c_mgc_ha_yr,
    -0.25, 5.25, 2010L, "nv", 6.42,
    12.25, 18.75, 2010L, "nv", 0.31
  )
}
