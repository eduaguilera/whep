# nolint start: object_length_linter.
# Grassland and natural-land soil carbon inputs from the LPJmL run (Module B,
# Task B2c-2). The carbon returned to soil under grassland and natural
# vegetation is the LPJmL net primary production minus harvested carbon,
# floored at zero, per the RESOLVED phase-2C spec (F1/F2). This is the
# per-land-use-class counterpart of build_soil_carbon_inputs() (which handles
# cropland per crop). The two feed the c_inputs contract of
# build_carbon_balance().
#
# PER-STAND VS PER-CELL (resolved empirically, not guessed): pft_npp values are
# PER PFT STAND (gC per m2 of the PFT's own stand), verified by reconstructing
# mnpp.nc's annual total as natfrac*sum(natural pft NPP) + sum(cftfrac*cft NPP)
# to within 0.1% at sampled land cells (the naive 43-band sum overshoots
# 2-20x). The per-stand density IS the per-hectare-of-that-land-use value the
# carbon balance needs, so:
#   - natural: the 11 natural PFTs coexist in one natural stand, so their
#     per-m2 densities ADD -> sum over bands 1-11 (they are not harvested).
#   - grassland: rainfed and irrigated grassland are SEPARATE stands, so the
#     per-hectare-of-grassland density is the stand-area-weighted mean of their
#     net (NPP - harvest) densities (weights from the cftfrac stand fractions).
# No litterfall coefficient is applied: NPP minus harvest IS the mass returned.

#' Build grassland and natural-land soil carbon inputs from LPJmL.
#'
#' @description
#' Assemble the carbon returned to soil under grassland and natural vegetation
#' as the layer the soil-organic-carbon turnover models consume, from a
#' finished LPJmL run. The class carbon input is the net primary production
#' minus harvested carbon (both per-plant-functional-type, [read_lpjml_npp()]),
#' floored at zero and converted to megagrams of carbon per hectare per year
#' (1 gC/m2 = 0.01 MgC/ha). Natural land sums the eleven natural
#' plant-functional-types (they coexist in one stand); grassland takes the
#' stand-area-weighted mean of the rainfed and irrigated grassland net inputs
#' and adds the grazing-excreta carbon from
#' [build_livestock_nutrient_flows()]'s `applied` stream spread uniformly over
#' the polity's grassland area. The humification fraction is the
#' spontaneous-grass value for grassland and the woody-residue value for
#' natural land (both from [residue_humification]).
#'
#' @param resolution `"grid"` (default, per cell and class) or `"polity"`
#'   (aggregated to `area_code`, area-weighting the per-hectare densities).
#' @param data Named list of pre-loaded inputs, each falling back to its reader
#'   when absent: `npp` and `harvestc` (per cell, PFT and year, the
#'   [read_lpjml_npp()] output); `stand_frac` (per cell, year and PFT name the
#'   managed-grassland stand fractions with columns `lon`, `lat`, `year`,
#'   `name_pft`, `stand_frac`); `country_grid` (`lon`, `lat`, `area_code`,
#'   `cell_area_frac`); `land_use` (per cell grassland `area_ha`, used to spread
#'   excreta); `excreta` (the `applied` tibble of
#'   [build_livestock_nutrient_flows()], grassland rows carry `applied_c`
#'   tonnes C); `residue_humification` (defaults to [residue_humification]).
#' @param example If `TRUE`, return a small fixture instead of reading remote
#'   data. Defaults to `FALSE`.
#' @return A tibble keyed by `(lon, lat, area_code, year, land_use)` at `"grid"`
#'   resolution (or `(area_code, year, land_use)` at `"polity"`), with
#'   `c_input_mgc_ha_yr`, `humified_fraction` and `method_c_input`, for
#'   `land_use` in `"grassland"` and `"natural"`.
#' @source LPJmL run net primary production and harvested carbon; grassland and
#'   natural carbon inputs per the WHEP historical carbon-balance design.
#' @export
#' @examples
#' build_grass_natural_carbon_inputs(example = TRUE)
build_grass_natural_carbon_inputs <- function(
  resolution = c("grid", "polity"),
  data = list(),
  example = FALSE
) {
  resolution <- rlang::arg_match(resolution)
  if (isTRUE(example)) {
    return(.example_grass_natural_carbon_inputs())
  }
  d <- .gn_resolve_inputs(data)
  natural <- .gn_natural_input(d)
  grassland <- .gn_grassland_input(d)
  dplyr::bind_rows(natural, grassland) |>
    .gn_finalise(resolution)
}

# -- Input resolution ---------------------------------------------------------

.gn_resolve_inputs <- function(data) {
  list(
    npp = data$npp %||% read_lpjml_npp("npp"),
    harvestc = data$harvestc %||% read_lpjml_npp("harvestc"),
    stand_frac = data$stand_frac %||% .gn_read_stand_frac(),
    country_grid = data$country_grid %||% .gn_read_country_grid(),
    land_use = data$land_use %||% .gn_read_land_use(),
    excreta = data$excreta,
    residue_humification = data$residue_humification %||%
      whep::residue_humification
  )
}

# The natural-land PFT names (natural stand: trees plus the three natural
# grasses), matching pft_npp.nc bands 1-11. Joined by name, never band index.
.gn_natural_pfts <- function() {
  c(
    "tropical broadleaved evergreen tree",
    "tropical broadleaved raingreen tree",
    "temperate needleleaved evergreen tree",
    "temperate broadleaved evergreen tree",
    "temperate broadleaved summergreen tree",
    "boreal needleleaved evergreen tree",
    "boreal broadleaved summergreen tree",
    "boreal needleleaved summergreen tree",
    "Tropical C4 grass",
    "Temperate C3 grass",
    "Polar C3 grass"
  )
}

.gn_grassland_pfts <- function() {
  c("rainfed grassland", "irrigated grassland")
}

# -- Natural-land carbon input ------------------------------------------------

# Natural land is not harvested, so its input is the sum of the natural PFT
# NPP densities (they coexist in one stand), converted to MgC/ha.
.gn_natural_input <- function(d) {
  hf <- .gn_humified(d$residue_humification, "woody_residue")
  d$npp |>
    dplyr::filter(.data$name_pft %in% .gn_natural_pfts()) |>
    dplyr::summarise(
      c_input_mgc_ha_yr = sum(pmax(.data$value, 0)) * 0.01,
      .by = c("lon", "lat", "year")
    ) |>
    .gn_attach_polity(d$country_grid) |>
    dplyr::mutate(
      land_use = "natural",
      humified_fraction = hf,
      method_c_input = "lpjml_npp_minus_harvest"
    )
}

# -- Grassland carbon input ---------------------------------------------------

# Grassland input: the stand-area-weighted mean of the rainfed and irrigated
# grassland net (NPP - harvest) densities, plus the grazing-excreta density.
.gn_grassland_input <- function(d) {
  hf <- .gn_humified(d$residue_humification, "weed")
  net <- .gn_grassland_net(d$npp, d$harvestc, d$stand_frac) |>
    .gn_attach_polity(d$country_grid)
  excreta <- .gn_excreta_density(d$excreta, d$land_use, d$country_grid)
  net |>
    dplyr::left_join(excreta, by = c("area_code", "year")) |>
    dplyr::mutate(
      c_input_mgc_ha_yr = .data$npp_c_mgc_ha_yr +
        dplyr::coalesce(.data$excreta_c_mgc_ha_yr, 0),
      land_use = "grassland",
      humified_fraction = hf,
      method_c_input = "lpjml_npp_minus_harvest"
    ) |>
    dplyr::select(
      "lon",
      "lat",
      "area_code",
      "year",
      "land_use",
      "c_input_mgc_ha_yr",
      "humified_fraction",
      "method_c_input"
    )
}

# Per-cell grassland NPP density (MgC/ha): net (NPP - harvest) per grassland
# stand, floored at zero, area-weighted over the rainfed/irrigated stands by
# their stand fractions. A grassland stand absent from the cell (no stand
# fraction) gets zero weight, so a nominal zero-NPP band does not dilute the
# productive stand's density; when no stand carries a fraction the density is a
# simple mean (see .gn_wmean).
.gn_grassland_net <- function(npp, harvestc, stand_frac) {
  npp_g <- .gn_grassland_pft_values(npp, "value")
  harv_g <- .gn_grassland_pft_values(harvestc, "harvest")
  npp_g |>
    dplyr::left_join(
      harv_g,
      by = c("lon", "lat", "year", "name_pft")
    ) |>
    dplyr::mutate(
      net_c = pmax(.data$value - dplyr::coalesce(.data$harvest, 0), 0)
    ) |>
    dplyr::left_join(
      stand_frac,
      by = c("lon", "lat", "year", "name_pft")
    ) |>
    dplyr::mutate(stand_frac = dplyr::coalesce(.data$stand_frac, 0)) |>
    # A band absent from the cell (no stand fraction and no production) is not a
    # real stand; drop it so it neither weights nor mean-dilutes the density.
    dplyr::filter(.data$stand_frac > 0 | .data$net_c > 0) |>
    dplyr::summarise(
      npp_c_mgc_ha_yr = .gn_wmean(.data$net_c, .data$stand_frac) * 0.01,
      .by = c("lon", "lat", "year")
    )
}

.gn_grassland_pft_values <- function(x, out_name) {
  x |>
    dplyr::filter(.data$name_pft %in% .gn_grassland_pfts()) |>
    dplyr::select("lon", "lat", "year", "name_pft", "value") |>
    dplyr::rename(!!out_name := "value")
}

# Grazing-excreta carbon density (MgC/ha of grassland), uniform per polity:
# total applied grazing-excreta carbon (tonnes C = MgC) over the polity's total
# grassland area. Returns zero density when no excreta is injected.
.gn_excreta_density <- function(excreta, land_use, country_grid) {
  if (is.null(excreta)) {
    return(tibble::tibble(
      area_code = integer(),
      year = integer(),
      excreta_c_mgc_ha_yr = numeric()
    ))
  }
  grass_c <- .gn_grass_excreta_mass(excreta)
  grass_area <- .gn_grass_area(land_use, country_grid)
  grass_c |>
    dplyr::inner_join(grass_area, by = c("area_code", "year")) |>
    dplyr::mutate(
      excreta_c_mgc_ha_yr = dplyr::if_else(
        .data$grass_area_ha > 0,
        .data$excreta_c_mg / .data$grass_area_ha,
        0
      )
    ) |>
    dplyr::select("area_code", "year", "excreta_c_mgc_ha_yr")
}

# Total grazing-excreta carbon (MgC) applied to grassland per polity-year. The
# applied stream carries land_use "Grassland" and territory on the area_code key.
.gn_grass_excreta_mass <- function(excreta) {
  excreta |>
    dplyr::filter(.data$land_use == "Grassland") |>
    dplyr::summarise(
      excreta_c_mg = sum(.data$applied_c, na.rm = TRUE),
      .by = c("year", "territory")
    ) |>
    dplyr::transmute(
      area_code = as.integer(.data$territory),
      year = as.integer(.data$year),
      excreta_c_mg = .data$excreta_c_mg
    )
}

# Total grassland area (ha) per polity-year from the land-use layer, scaled by
# each cell's polity fraction.
.gn_grass_area <- function(land_use, country_grid) {
  cg <- .normalize_country_grid(country_grid) |>
    dplyr::select("lon", "lat", "area_code", "cell_area_frac")
  land_use |>
    dplyr::filter(stringr::str_to_lower(.data$land_use) == "grassland") |>
    dplyr::inner_join(cg, by = c("lon", "lat", "area_code")) |>
    dplyr::summarise(
      grass_area_ha = sum(.data$area_ha * .data$cell_area_frac),
      .by = c("area_code", "year")
    )
}

# -- Shared helpers -----------------------------------------------------------

# Attach the overlapping polities to each cell via the country grid; a border
# cell keeps every polity it overlaps.
.gn_attach_polity <- function(cells, country_grid) {
  cg <- .normalize_country_grid(country_grid) |>
    dplyr::select("lon", "lat", "area_code")
  cells |>
    dplyr::mutate(lon = round(.data$lon, 2), lat = round(.data$lat, 2)) |>
    dplyr::inner_join(
      dplyr::mutate(cg, lon = round(.data$lon, 2), lat = round(.data$lat, 2)),
      by = c("lon", "lat")
    )
}

.gn_humified <- function(residue_humification, input_type) {
  v <- residue_humification$humified_fraction[
    residue_humification$input_type == input_type
  ]
  if (length(v) != 1L) {
    cli::cli_abort(
      "{.field residue_humification} needs one {.val {input_type}} row."
    )
  }
  v
}

.gn_wmean <- function(value, weight) {
  if (sum(weight) == 0) {
    return(mean(value))
  }
  sum(value * weight) / sum(weight)
}

# Grid output keeps the per-cell per-class rows; polity output aggregates to
# (area_code, year, land_use) area-weighting the per-hectare densities by the
# class area so total carbon mass is conserved. Without an area weight per cell
# here, the per-hectare density is a plain mean across the polity's cells.
.gn_finalise <- function(x, resolution) {
  if (resolution == "grid") {
    return(tibble::as_tibble(x))
  }
  x |>
    dplyr::summarise(
      c_input_mgc_ha_yr = mean(.data$c_input_mgc_ha_yr),
      humified_fraction = .data$humified_fraction[1],
      method_c_input = .data$method_c_input[1],
      .by = c("area_code", "year", "land_use")
    ) |>
    tibble::as_tibble()
}

# -- Reader stubs (real readers wired elsewhere) ------------------------------

.gn_read_stand_frac <- function() {
  cli::cli_abort(
    c(
      "No {.field stand_frac} reader is wired yet.",
      i = "Pass {.code data$stand_frac} (per-cell managed-grassland stand \\
           fractions from cftfrac.nc)."
    )
  )
}

.gn_read_country_grid <- function() {
  whep_read_file("spatialize-country-grid")
}

.gn_read_land_use <- function() {
  cli::cli_abort(
    c(
      "No {.field land_use} reader is wired yet.",
      i = "Pass {.code data$land_use} (per-cell grassland areas from \\
           {.fun read_luh2_landuse})."
    )
  )
}
# nolint end
