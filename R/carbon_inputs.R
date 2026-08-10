# Per-land-use-class soil carbon-input assembly (Module B, Task B2c-4). Combines
# the two carbon-input builders into the single c_inputs layer
# build_carbon_balance() consumes, keyed (lon, lat, area_code, year, land_use):
#   - cropland: build_soil_carbon_inputs() emits per CROP within cropland, so it
#     is aggregated to the cropland class by area-weighting the per-hectare
#     carbon densities (weight = the crop's harvested area) and carbon-weighting
#     the humification fraction (weight = each crop's carbon mass = density x
#     area). This is the class-level counterpart of the crop-level input.
#   - grassland + natural: build_grass_natural_carbon_inputs() already emits at
#     the class grain, so its rows pass through unchanged.
# All densities are MgC/ha/yr.

#' Assemble the per-land-use-class soil carbon inputs.
#'
#' @description
#' Build the carbon-input layer [build_carbon_balance()] consumes, keyed by
#' `(lon, lat, area_code, year, land_use)`. The cropland class aggregates the
#' per-crop cropland inputs from [build_soil_carbon_inputs()] to the class
#' grain: the class carbon density is the harvested-area-weighted mean of the
#' per-crop densities, and the humification fraction is the carbon-mass-weighted
#' mean of the per-crop fractions (mass = density times area). The grassland and
#' natural classes come from [build_grass_natural_carbon_inputs()] unchanged.
#'
#' @param resolution `"grid"` (default, per cell and class) or `"polity"`
#'   (aggregated to `area_code`, area-weighting the cropland density by the
#'   polity crop area).
#' @param years Optional integer vector of calendar years to keep. `NULL`
#'   (default) keeps every year the inputs cover. Threaded into the default
#'   [build_soil_carbon_inputs()] and [build_grass_natural_carbon_inputs()]
#'   builders so their readers slice to the requested years; ignored for inputs
#'   supplied via `data`.
#' @param data Named list of pre-loaded inputs, each falling back to its builder
#'   when absent: `cropland` (the [build_soil_carbon_inputs()] output, per cell,
#'   crop and year, with `total_c_input_mgc_ha_yr` and `humified_fraction`);
#'   `crop_area` (per cell, crop and year harvested area with columns `lon`,
#'   `lat`, `area_code`, `item_prod_code`, `year`, `crop_area_ha`, used to
#'   area-weight the crop densities); `grass_natural` (the
#'   [build_grass_natural_carbon_inputs()] output at the class grain); and
#'   optional `land_use` (per-cell class `area_ha`, used to area-weight
#'   grassland/natural polity output); and `country_grid`, the polycell support
#'   resolved to one row per cell and `area_code`, from which `crop_area` is
#'   derived when absent. It is the same support [build_soil_carbon_inputs()]
#'   reads, so the weights and the carbon they weight can never come from two
#'   different crosswalks. When
#'   `cropland` or `grass_natural` are absent the respective builder is called
#'   with the remaining members of `data`.
#' @param example If `TRUE`, return a small fixture instead of reading remote
#'   data. Defaults to `FALSE`.
#' @return A tibble keyed by `(lon, lat, area_code, year, land_use)` at `"grid"`
#'   resolution (or `(area_code, year, land_use)` at `"polity"`), with
#'   `c_input_mgc_ha_yr`, `humified_fraction` and `method_c_input`, for
#'   `land_use` in `"cropland"`, `"grassland"` and `"natural"`, plus the polity
#'   columns below.
#' @inheritSection whep_polity_columns Polity columns
#' @source Cropland inputs from [build_soil_carbon_inputs()]; grassland and
#'   natural inputs from [build_grass_natural_carbon_inputs()]; assembled per
#'   the WHEP historical carbon-balance design.
#' @export
#' @examples
#' build_carbon_inputs(example = TRUE)
build_carbon_inputs <- function(
  resolution = c("grid", "polity"),
  data = list(),
  years = NULL,
  example = FALSE
) {
  resolution <- rlang::arg_match(resolution)
  if (isTRUE(example)) {
    return(.example_carbon_inputs())
  }
  d <- .ci_resolve_inputs(data, years)
  cropland <- .ci_cropland_class(d$cropland, d$crop_area)
  dplyr::bind_rows(cropland, d$grass_natural) |>
    .ci_finalise(resolution, data$land_use) |>
    .add_reporting_polity_columns()
}

# -- Input resolution ---------------------------------------------------------

.ci_resolve_inputs <- function(data, years = NULL) {
  list(
    # .sci_build() rather than build_soil_carbon_inputs(): the reporting polity
    # columns the exported function attaches are discarded by
    # .ci_cropland_class() below and re-added to this function's own output, so
    # paying for them on the pre-collapse intermediate is pure cost (#624).
    cropland = data$cropland %||% .sci_build("grid", data, years),
    crop_area = data$crop_area %||% .ci_crop_area(data),
    grass_natural = data$grass_natural %||%
      build_grass_natural_carbon_inputs(data = data, years = years)
  )
}

# -- Cropland crop -> class aggregation ---------------------------------------

# Aggregate the per-crop cropland densities to the cropland class per cell-year.
# The class carbon density is the harvested-area-weighted mean of the per-crop
# densities; the humification fraction is the carbon-mass-weighted mean (mass =
# density x area), so a crop supplying more carbon dominates the class fraction.
# `class_area_ha` (the cell's total cropland area) is carried so the downstream
# polity aggregation can area-weight the per-hectare density and conserve mass.
.ci_cropland_class <- function(cropland, crop_area) {
  join_keys <- c("lon", "lat", "area_code", "item_prod_code")
  if (rlang::has_name(crop_area, "year")) {
    join_keys <- c(join_keys, "year")
  }
  cropland |>
    dplyr::inner_join(
      crop_area,
      by = join_keys
    ) |>
    dplyr::mutate(
      c_mass = .data$total_c_input_mgc_ha_yr * .data$crop_area_ha
    ) |>
    dplyr::summarise(
      c_input_mgc_ha_yr = .ci_wmean(
        .data$total_c_input_mgc_ha_yr,
        .data$crop_area_ha
      ),
      humified_fraction = .ci_wmean(.data$humified_fraction, .data$c_mass),
      class_area_ha = sum(.data$crop_area_ha),
      .by = c("lon", "lat", "area_code", "year")
    ) |>
    dplyr::mutate(
      land_use = "cropland",
      method_c_input = "humified_weighted"
    )
}

# -- Finalisation -------------------------------------------------------------

# Grid output keeps per-cell per-class rows; polity output aggregates to
# (area_code, year, land_use). The per-hectare density is area-weighted by the
# cell's class area (`class_area_ha`) so the polity density times the polity
# class area equals the summed grid-level carbon mass; the humification fraction
# is carbon-mass-weighted (mass = density x class area) so humified carbon is
# likewise conserved. Grassland/natural class areas are attached from
# `land_use` when supplied; a class with no available area retains the plain
# mean fallback via .ci_wmean's zero-weight guard.
.ci_finalise <- function(x, resolution, land_use = NULL) {
  drop_cols <- c("class_area_ha")
  if (resolution == "grid") {
    return(tibble::as_tibble(dplyr::select(x, -dplyr::any_of(drop_cols))))
  }
  if (!is.null(land_use)) {
    x <- .ci_attach_land_use_area(x, land_use)
  }
  x |>
    dplyr::mutate(
      area_weight = dplyr::coalesce(.data$class_area_ha, 0),
      c_mass = .data$c_input_mgc_ha_yr * .data$area_weight
    ) |>
    dplyr::summarise(
      c_input_mgc_ha_yr = .ci_wmean(.data$c_input_mgc_ha_yr, .data$area_weight),
      humified_fraction = .ci_wmean(.data$humified_fraction, .data$c_mass),
      method_c_input = .data$method_c_input[1],
      .by = c("area_code", "year", "land_use")
    ) |>
    tibble::as_tibble()
}

# Supply class-area weights for grassland/natural rows from the same LUH2 layer
# consumed by the carbon balance. Cropland retains its harvested-area weight;
# unmatched classes retain the existing zero-weight/plain-mean fallback.
.ci_attach_land_use_area <- function(x, land_use) {
  areas <- land_use |>
    dplyr::mutate(
      lon = round(.data$lon, 2),
      lat = round(.data$lat, 2),
      land_use = stringr::str_to_lower(.data$land_use)
    ) |>
    dplyr::summarise(
      land_area_ha = sum(.data$area_ha),
      .by = c("lon", "lat", "area_code", "year", "land_use")
    )
  x |>
    dplyr::left_join(
      areas,
      by = c("lon", "lat", "area_code", "year", "land_use")
    ) |>
    dplyr::mutate(
      class_area_ha = dplyr::coalesce(
        .data$class_area_ha,
        .data$land_area_ha
      )
    ) |>
    dplyr::select(-"land_area_ha")
}

.ci_wmean <- function(value, weight) {
  if (sum(weight) == 0) {
    return(mean(value))
  }
  sum(value * weight) / sum(weight)
}

# -- Crop-area reader ---------------------------------------------------------

# Per cell-crop harvested area (ha), split between the cell's polycells by their
# share of the cell's land, from the same static support + crop_patterns
# build_soil_carbon_inputs uses (crop_patterns is time-invariant, so no year
# key). Only reached when data$crop_area is absent; country_grid and
# crop_patterns fall back to the same default readers build_soil_carbon_inputs
# uses -- `.sci_read_country_grid()` being the carbon path's shared polycell
# support -- so these weights and the cropland carbon they weight can never be
# built on two different crosswalks.
.ci_crop_area <- function(data) {
  country_grid <- data$country_grid %||% .sci_read_country_grid()
  crop_patterns <- data$crop_patterns %||% .sci_read_crop_patterns()
  cg <- .normalize_carbon_support(country_grid) |>
    dplyr::mutate(lon = round(.data$lon, 2), lat = round(.data$lat, 2))
  crop_patterns |>
    dplyr::mutate(
      lon = round(.data$lon, 2),
      lat = round(.data$lat, 2),
      item_prod_code = as.character(.data$item_prod_code)
    ) |>
    dplyr::inner_join(cg, by = c("lon", "lat")) |>
    dplyr::transmute(
      lon = .data$lon,
      lat = .data$lat,
      area_code = .data$area_code,
      item_prod_code = .data$item_prod_code,
      crop_area_ha = .data$crop_area_ha * .data$cell_area_frac
    )
}
