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
#' @param data Named list of pre-loaded inputs, each falling back to its builder
#'   when absent: `cropland` (the [build_soil_carbon_inputs()] output, per cell,
#'   crop and year, with `total_c_input_mgc_ha_yr` and `humified_fraction`);
#'   `crop_area` (per cell, crop and year harvested area with columns `lon`,
#'   `lat`, `area_code`, `item_prod_code`, `year`, `crop_area_ha`, used to
#'   area-weight the crop densities); `grass_natural` (the
#'   [build_grass_natural_carbon_inputs()] output at the class grain). When
#'   `cropland` or `grass_natural` are absent the respective builder is called
#'   with the remaining members of `data`.
#' @param example If `TRUE`, return a small fixture instead of reading remote
#'   data. Defaults to `FALSE`.
#' @return A tibble keyed by `(lon, lat, area_code, year, land_use)` at `"grid"`
#'   resolution (or `(area_code, year, land_use)` at `"polity"`), with
#'   `c_input_mgc_ha_yr`, `humified_fraction` and `method_c_input`, for
#'   `land_use` in `"cropland"`, `"grassland"` and `"natural"`.
#' @source Cropland inputs from [build_soil_carbon_inputs()]; grassland and
#'   natural inputs from [build_grass_natural_carbon_inputs()]; assembled per
#'   the WHEP historical carbon-balance design.
#' @export
#' @examples
#' build_carbon_inputs(example = TRUE)
build_carbon_inputs <- function(
  resolution = c("grid", "polity"),
  data = list(),
  example = FALSE
) {
  resolution <- rlang::arg_match(resolution)
  if (isTRUE(example)) {
    return(.example_carbon_inputs())
  }
  d <- .ci_resolve_inputs(data)
  cropland <- .ci_cropland_class(d$cropland, d$crop_area)
  dplyr::bind_rows(cropland, d$grass_natural) |>
    .ci_finalise(resolution)
}

# -- Input resolution ---------------------------------------------------------

.ci_resolve_inputs <- function(data) {
  list(
    cropland = data$cropland %||% build_soil_carbon_inputs(data = data),
    crop_area = data$crop_area %||% .ci_crop_area(data),
    grass_natural = data$grass_natural %||%
      build_grass_natural_carbon_inputs(data = data)
  )
}

# -- Cropland crop -> class aggregation ---------------------------------------

# Aggregate the per-crop cropland densities to the cropland class per cell-year.
# The class carbon density is the harvested-area-weighted mean of the per-crop
# densities; the humification fraction is the carbon-mass-weighted mean (mass =
# density x area), so a crop supplying more carbon dominates the class fraction.
.ci_cropland_class <- function(cropland, crop_area) {
  cropland |>
    dplyr::inner_join(
      crop_area,
      by = c("lon", "lat", "area_code", "item_prod_code")
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
      .by = c("lon", "lat", "area_code", "year")
    ) |>
    dplyr::mutate(
      land_use = "cropland",
      method_c_input = "humified_weighted"
    )
}

# -- Finalisation -------------------------------------------------------------

# Grid output keeps per-cell per-class rows; polity output aggregates to
# (area_code, year, land_use) by a plain mean of the per-hectare densities
# across the polity's cells (each class density is already per hectare of that
# class), matching build_grass_natural_carbon_inputs's polity aggregation.
.ci_finalise <- function(x, resolution) {
  if (resolution == "grid") {
    return(tibble::as_tibble(x))
  }
  x |>
    dplyr::summarise(
      c_input_mgc_ha_yr = mean(.data$c_input_mgc_ha_yr),
      humified_fraction = mean(.data$humified_fraction),
      method_c_input = .data$method_c_input[1],
      .by = c("area_code", "year", "land_use")
    ) |>
    tibble::as_tibble()
}

.ci_wmean <- function(value, weight) {
  if (sum(weight) == 0) {
    return(mean(value))
  }
  sum(value * weight) / sum(weight)
}

# -- Crop-area reader ---------------------------------------------------------

# Per cell-crop harvested area (ha), scaled by the cell's land fraction, from
# the same static country_grid + crop_patterns build_soil_carbon_inputs uses
# (crop_patterns is time-invariant, so no year key). Only reached when
# data$crop_area is absent; requires data$country_grid and data$crop_patterns
# (the spatialization inputs) to be supplied.
.ci_crop_area <- function(data) {
  if (is.null(data$country_grid) || is.null(data$crop_patterns)) {
    cli::cli_abort(c(
      "No {.field crop_area} supplied and it cannot be derived.",
      i = "Pass {.code data$crop_area} (per cell-crop harvested area) or both
           {.code data$country_grid} and {.code data$crop_patterns}."
    ))
  }
  cg <- .normalize_country_grid(data$country_grid) |>
    dplyr::mutate(lon = round(.data$lon, 2), lat = round(.data$lat, 2))
  data$crop_patterns |>
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
