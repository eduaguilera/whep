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
#'   and `cftfrac.nc` (the `scenario_*` output folder). Defaults to the
#'   `WHEP_LPJML_RUN_DIR` environment variable, consistent with
#'   [build_feed_intake_local()].
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
  run_dir = Sys.getenv("WHEP_LPJML_RUN_DIR"),
  years = NULL,
  first_year = 1901L,
  shares = grass_access_shares(),
  example = FALSE
) {
  if (example) {
    return(.example_grass_avail_lpjml())
  }
  if (!nzchar(run_dir)) {
    cli::cli_abort(c(
      "{.arg run_dir} is empty.",
      i = "Pass it explicitly or set {.envvar WHEP_LPJML_RUN_DIR}."
    ))
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

#' Aggregate gridded grass availability to polity totals.
#'
#' Sums gridded grass availability to polity (country, or subnational where
#' available) totals, splitting each cell's grass by the cell's land-area share
#' in each polity so border cells are attributed proportionally. The polity
#' grass supply ceiling for feed allocation.
#'
#' @param grass Gridded grass availability from [build_grass_availability()],
#'   with `lon`, `lat`, `year` and `grass_avail_dm_t`.
#' @param cell_polity Cell-to-polity mapping with `lon`, `lat`, `area_code` and
#'   `polity_frac` (the cell's land-area fraction in the polity; pass 1 for a
#'   majority assignment, e.g. from `country_grid`).
#' @return A tibble with `area_code`, `year` and `grass_avail_dm_t`.
#' @export
#' @examples
#' grass <- build_grass_availability(method = "lpjml", example = TRUE)
#' cp <- tibble::tibble(
#'   lon = grass$lon,
#'   lat = grass$lat,
#'   area_code = 1L,
#'   polity_frac = 1
#' )
#' aggregate_grass_to_polity(grass, cp)
aggregate_grass_to_polity <- function(grass, cell_polity) {
  grass |>
    dplyr::mutate(lon = round(lon, 2), lat = round(lat, 2)) |>
    dplyr::inner_join(
      dplyr::mutate(cell_polity, lon = round(lon, 2), lat = round(lat, 2)),
      by = c("lon", "lat"),
      relationship = "many-to-many"
    ) |>
    dplyr::summarise(
      grass_avail_dm_t = sum(grass_avail_dm_t * polity_frac, na.rm = TRUE),
      .by = c("area_code", "year")
    )
}

#' Read natural-grass productivity from an LPJmL run.
#'
#' Sums the natural-grass PFT net primary production bands (the ungrazed
#' natural stand, climate-driven) into a per-cell productivity layer used to
#' distribute grazing livestock by grass production rather than pasture area.
#' Natural-grass NPP is exogenous to the grazing density, so it avoids the
#' livestock to grassland_lsuha to grass-NPP circularity.
#'
#' @param run_dir Path to the LPJmL run output directory holding `pft_npp.nc`.
#'   Defaults to the `WHEP_LPJML_RUN_DIR` environment variable, consistent with
#'   [build_feed_intake_local()].
#' @param years Integer vector of calendar years to read.
#' @param first_year First calendar year of the run's output time axis.
#' @param example If `TRUE`, return a small fixture instead of reading a run.
#' @return A tibble with `lon`, `lat`, `year` and `grass_npp` (gC/m2/yr).
#' @export
#' @examples
#' read_lpjml_grass_productivity(example = TRUE)
read_lpjml_grass_productivity <- function(
  run_dir = Sys.getenv("WHEP_LPJML_RUN_DIR"),
  years = NULL,
  first_year = 1901L,
  example = FALSE
) {
  if (example) {
    return(.example_grass_productivity())
  }
  if (!nzchar(run_dir)) {
    cli::cli_abort(c(
      "{.arg run_dir} is empty.",
      i = "Pass it explicitly or set {.envvar WHEP_LPJML_RUN_DIR}."
    ))
  }
  if (!rlang::is_installed("ncdf4")) {
    cli::cli_abort("Package {.pkg ncdf4} is required to read the LPJmL run.")
  }
  .read_lpjml_bands(
    file.path(run_dir, "pft_npp.nc"),
    "NPP",
    c("Tropical C4 grass", "Temperate C3 grass", "Polar C3 grass"),
    years,
    first_year
  ) |>
    tibble::as_tibble() |>
    dplyr::summarise(
      grass_npp = sum(value, na.rm = TRUE),
      .by = c(lon, lat, year)
    ) |>
    dplyr::filter(grass_npp > 0)
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
  years <- .clip_run_years(years, first_year, nc$dim[["time"]]$len, path)
  if (length(years) == 0) {
    return(.empty_lpjml_bands())
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

# Restrict requested years to the run's output time axis, warning about (and
# skipping) any outside it, so an out-of-coverage year does not abort the read
# with an out-of-bounds index. Skipped years simply carry no LPJmL grass.
.clip_run_years <- function(years, first_year, n_time, path) {
  avail <- first_year + seq_len(n_time) - 1L
  dropped <- setdiff(years, avail)
  if (length(dropped) > 0) {
    n <- length(dropped)
    cli::cli_warn(c(
      "{n} requested year{?s} fall outside the run's coverage
       ({min(avail)}-{max(avail)}) in {.file {basename(path)}} and are skipped.",
      i = "Cells in {cli::qty(n)}{?that year/those years} carry no LPJmL grass."
    ))
  }
  intersect(years, avail)
}

.empty_lpjml_bands <- function() {
  data.table::data.table(
    lon = numeric(),
    lat = numeric(),
    year = integer(),
    band = character(),
    value = numeric()
  )
}

# Toy fixture for the runnable example (sampled from a real read-back).
.example_grass_avail_lpjml <- function() {
  tibble::tribble(
    ~lon, ~lat, ~year, ~grass_npp_gc_m2, ~grass_avail_dm_t_ha, ~grass_avail_dm_t,
    9.25, 47.75, 2000L, 612.4, 6.258, 16980,
    -55.25, -12.25, 2000L, 488.1, 4.988, 15640,
    35.75, -1.25, 2000L, 421.7, 4.310, 13110
  )
}

# Toy fixture for the runnable example (sampled from a real read-back).
.example_grass_productivity <- function() {
  tibble::tribble(
    ~lon, ~lat, ~year, ~grass_npp,
    9.25, 47.75, 2000L, 412.7,
    35.75, -1.25, 2000L, 631.4,
    -55.25, -12.25, 2000L, 348.9
  )
}
