# LPJmL grass-availability method (grass_method = "lpjml").
#
# Reads managed-grassland net primary production from a finished LPJmL run and
# converts it to grazable above-ground dry-matter availability: the supply
# ceiling that feeds allocation. Availability is the PRODUCTION flux (pft_npp),
# not the realised grazing off-take (uptakec / pft_harvestc) which is instead
# the intake-validation target (Herrero 2013 ~2.3 Pg DM).

#' Build grazable grass availability.
#'
#' Multi-method wrapper for the grass forage supply ceiling that feeds
#' allocation. The default `lpjml` method reads managed-grassland net primary
#' production from a finished LPJmL run (spatially explicit, the most rigorous
#' method); `coefficient` applies a per-area grass-yield coefficient and is not
#' yet implemented (it needs a `grass_yield_coef` dataset).
#'
#' @param method Grass-availability method, `"lpjml"` or `"coefficient"`.
#' @param ... Passed to the selected method's builder, e.g.
#'   [build_grass_availability_lpjml()].
#' @return A tibble of grass availability with a `method_grass` column
#'   recording the method used.
#' @export
#' @examples
#' build_grass_availability(method = "lpjml", example = TRUE)
build_grass_availability <- function(
  method = c("lpjml", "coefficient"),
  ...
) {
  method <- rlang::arg_match(method)
  out <- switch(
    method,
    lpjml = build_grass_availability_lpjml(...),
    coefficient = cli::cli_abort(c(
      "The {.val coefficient} grass method is not yet implemented.",
      i = "It needs a {.field grass_yield_coef} dataset (a methodological choice)."
    ))
  )
  dplyr::mutate(out, method_grass = method)
}

#' Build grazable grass availability from an LPJmL run.
#'
#' Reads managed-grassland net primary production (the LPJmL grassland CFT) from
#' a finished run and converts it to grazable above-ground dry-matter
#' availability, the forage supply ceiling for feed allocation. Availability is
#' the production flux, not the realised grazing off-take (the off-take is the
#' intake-validation target, not the supply).
#'
#' @param run_dir Path to the LPJmL run output directory holding `pft_npp.nc`
#'   and `cftfrac.nc` (the `scenario_*` output folder).
#' @param years Integer vector of calendar years to read.
#' @param first_year First calendar year of the run's output time axis.
#' @param shares Accessibility and conversion parameters from
#'   [grass_access_shares()].
#' @param example If `TRUE`, return a small fixture instead of reading a run.
#' @return A tibble with `lon`, `lat`, `year`, `grass_npp_gc_m2`,
#'   `grass_avail_dm_t_ha` and `grass_avail_dm_t`.
#' @export
#' @examples
#' build_grass_availability_lpjml(example = TRUE)
build_grass_availability_lpjml <- function(
  run_dir = NULL,
  years = NULL,
  first_year = 1901L,
  shares = grass_access_shares(),
  example = FALSE
) {
  if (example) {
    return(.example_grass_availability_lpjml())
  }
  if (!rlang::is_installed("ncdf4")) {
    cli::cli_abort("Package {.pkg ncdf4} is required to read the LPJmL run.")
  }
  # Force the default-argument promise here, not lazily inside dplyr's
  # data-masked mutate (forcing it there is unstable).
  force(shares)
  npp <- .read_lpjml_grass_npp(run_dir, years, first_year)
  npp <- dplyr::mutate(
    npp,
    grass_avail_dm_t_ha = .lpjml_grass_to_dm(grass_npp_gc_m2, shares),
    grass_avail_dm_t = grass_avail_dm_t_ha * cell_area_ha
  )
  dplyr::select(
    npp,
    lon,
    lat,
    year,
    grass_npp_gc_m2,
    grass_avail_dm_t_ha,
    grass_avail_dm_t
  )
}

#' Accessibility and conversion parameters for grazable grass availability.
#'
#' @param aboveground Fraction of total grass NPP that is above ground (the
#'   grazable compartment; LPJmL `pft_npp` is whole-plant). Default 0.46.
#' @param grazable Sustainable fraction of above-ground forage that can be
#'   grazed. Default 1 (the full above-ground ceiling); set below 1 to impose a
#'   sustainable-offtake share.
#' @param w_c_dm Carbon-to-dry-matter mass fraction. Default 0.45.
#' @return A named list with `aboveground`, `grazable` and `w_c_dm`.
#' @export
#' @examples
#' grass_access_shares(grazable = 0.6)
grass_access_shares <- function(
  aboveground = 0.46,
  grazable = 1,
  w_c_dm = 0.45
) {
  list(aboveground = aboveground, grazable = grazable, w_c_dm = w_c_dm)
}

# ---- Private helpers --------------------------------------------------

# gC/m2/yr -> grazable t DM/ha/yr. 1 gC/m2 = 0.01 tC/ha; / w_c_dm -> t DM/ha.
.lpjml_grass_to_dm <- function(grass_npp_gc_m2, shares) {
  grass_npp_gc_m2 * shares$aboveground * shares$grazable * 0.01 / shares$w_c_dm
}

# Cell area (ha) for a 0.5-degree cell centred at latitude `lat`.
.cell_area_ha_lat <- function(lat) {
  earth_radius_m <- 6371000
  half_step_rad <- 0.25 * pi / 180
  lon_step_rad <- 0.5 * pi / 180
  band <- sin(lat * pi / 180 + half_step_rad) -
    sin(lat * pi / 180 - half_step_rad)
  earth_radius_m^2 * lon_step_rad * band / 1e4
}

# Read grassland-CFT NPP (rainfed + irrigated) weighted by its area fraction,
# returning per-cell production in gC/m2/yr. Bands selected by NamePFT.
.read_lpjml_grass_npp <- function(run_dir, years, first_year) {
  bands <- c("rainfed grassland", "irrigated grassland")
  npp <- .read_lpjml_bands(
    file.path(run_dir, "pft_npp.nc"),
    "NPP",
    bands,
    years,
    first_year
  )
  frac <- .read_lpjml_bands(
    file.path(run_dir, "cftfrac.nc"),
    "CFTfrac",
    bands,
    years,
    first_year
  )
  data.table::setnames(frac, "value", "cft_frac")
  m <- merge(npp, frac, by = c("lon", "lat", "year", "band"))
  out <- m[,
    .(grass_npp_gc_m2 = sum(value * cft_frac, na.rm = TRUE)),
    by = .(lon, lat, year)
  ]
  out[, cell_area_ha := .cell_area_ha_lat(lat)]
  tibble::as_tibble(out[grass_npp_gc_m2 > 0])
}

# Read named PFT bands of one LPJmL output variable for the requested years,
# returning a long data.table (lon, lat, year, band, value).
.read_lpjml_bands <- function(path, var_name, band_names, years, first_year) {
  nc <- ncdf4::nc_open(path)
  on.exit(ncdf4::nc_close(nc))
  pft <- as.character(ncdf4::ncvar_get(nc, "NamePFT"))
  lon <- ncdf4::ncvar_get(nc, "lon")
  lat <- ncdf4::ncvar_get(nc, "lat")
  band_idx <- match(band_names, pft)
  if (anyNA(band_idx)) {
    cli::cli_abort(
      "Band(s) not in {.file {path}}: {band_names[is.na(band_idx)]}."
    )
  }
  combos <- tidyr::expand_grid(b = seq_along(band_names), y = seq_along(years))
  rows <- purrr::pmap(combos, function(b, y) {
    slab <- ncdf4::ncvar_get(
      nc,
      var_name,
      start = c(1L, 1L, band_idx[b], years[y] - first_year + 1L),
      count = c(length(lon), length(lat), 1L, 1L)
    )
    data.table::data.table(
      lon = rep(lon, times = length(lat)),
      lat = rep(lat, each = length(lon)),
      year = years[y],
      band = band_names[b],
      value = as.vector(slab)
    )
  })
  data.table::rbindlist(rows)
}

# Toy fixture for the runnable example (sampled from a real read-back).
.example_grass_availability_lpjml <- function() {
  tibble::tribble(
    ~lon, ~lat, ~year, ~grass_npp_gc_m2, ~grass_avail_dm_t_ha, ~grass_avail_dm_t,
    9.25, 47.75, 2000L, 612.4, 6.258, 16980,
    -55.25, -12.25, 2000L, 488.1, 4.988, 15640,
    35.75, -1.25, 2000L, 421.7, 4.310, 13110
  )
}
