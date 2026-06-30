#' Assemble soil carbon inputs per cell, crop and year.
#'
#' @description
#' Builds the carbon returned to soil from crop residues, crop roots and
#' applied manure, as the carbon-input layer the soil-organic-carbon turnover
#' models ([calculate_soc_dynamics()]) consume. Crop residue and root carbon
#' come from [calculate_npp_carbon_nitrogen()] (per polity, crop and year);
#' manure carbon comes from [build_livestock_nutrient_flows()]'s `applied`
#' stream. The three component carbon masses are converted to megagrams of
#' carbon (1 tonne = 1 Mg), gridded to cells in proportion to each crop's
#' harvested area, and divided by the cell-crop area to give Mg C per hectare
#' per year. A carbon-weighted humification fraction is computed per cell-year
#' from [residue_humification].
#'
#' At `"polity"` resolution the component carbon masses are summed back to
#' `(area_code, item_prod_code, year)` and the per-hectare values and humified
#' fraction re-derived from the polity totals.
#'
#' @param resolution `"grid"` (default, per cell) or `"polity"` (aggregated to
#'   `area_code`).
#' @param data Optional named list of pre-loaded inputs, each falling back to
#'   its reader when absent: `npp` (residue and root carbon per `area_code`,
#'   `item_prod_code`, `year`, columns `residue_c_t` and `root_c_t`, tonnes C);
#'   `manure` (the `applied` tibble of [build_livestock_nutrient_flows()], with
#'   `crop` already on the `item_prod_code` key and `territory` on the
#'   `area_code` key); `country_grid` and `crop_patterns` (the spatialization
#'   inputs, `crop_patterns` carrying per-cell `crop_area_ha`);
#'   `residue_humification` (defaults to [residue_humification]).
#' @param example If `TRUE`, return a small fixture instead of reading remote
#'   data. Defaults to `FALSE`.
#'
#' @return A tibble keyed by `(lon, lat, area_code, item_prod_code, year)` at
#'   `"grid"` resolution (or `(area_code, item_prod_code, year)` at
#'   `"polity"`), with `residue_c_mgc_ha_yr`, `root_c_mgc_ha_yr`,
#'   `manure_c_mgc_ha_yr`, `total_c_input_mgc_ha_yr`, `humified_fraction` and
#'   `method_c_input`.
#'
#' @export
#'
#' @examples
#' build_soil_carbon_inputs(example = TRUE)
build_soil_carbon_inputs <- function(
  resolution = c("grid", "polity"),
  data = list(),
  example = FALSE
) {
  resolution <- rlang::arg_match(resolution)
  if (isTRUE(example)) {
    return(.example_soil_carbon_inputs())
  }
  d <- .sci_resolve_inputs(data)
  components <- .sci_assemble_components(d$npp, d$manure)
  gridded <- .sci_to_grid(components, d$country_grid, d$crop_patterns)
  .sci_finalise(gridded, resolution, d$residue_humification)
}

# Private helpers ----

.sci_resolve_inputs <- function(data) {
  list(
    npp = data$npp %||% .sci_read_npp(),
    manure = data$manure %||% .sci_read_manure(),
    country_grid = data$country_grid %||% .sci_read_country_grid(),
    crop_patterns = data$crop_patterns %||% .sci_read_crop_patterns(),
    residue_humification = data$residue_humification %||%
      whep::residue_humification
  )
}

# Long component table: one row per polity x crop x year x input_type, carrying
# the carbon mass in Mg C (1 tonne = 1 Mg) and the humification key.
.sci_assemble_components <- function(npp, manure) {
  residue <- npp |>
    dplyr::transmute(
      area_code = as.integer(.data$area_code),
      item_prod_code = as.character(.data$item_prod_code),
      year = as.integer(.data$year),
      input_type = "crop_residue",
      c_mass_mg = .data$residue_c_t
    )
  root <- npp |>
    dplyr::transmute(
      area_code = as.integer(.data$area_code),
      item_prod_code = as.character(.data$item_prod_code),
      year = as.integer(.data$year),
      input_type = "root",
      c_mass_mg = .data$root_c_t
    )
  dplyr::bind_rows(residue, root, .sci_manure_components(manure))
}

# Manure carbon applied to cropland, keyed like the NPP components. The applied
# stream's `crop` must already be on the item_prod_code key and `territory` on
# the area_code key (the caller harmonises names to codes upstream).
.sci_manure_components <- function(manure) {
  manure |>
    dplyr::filter(
      .data$land_use == "Cropland",
      !is.na(.data$crop)
    ) |>
    dplyr::summarise(
      c_mass_mg = sum(.data$applied_c, na.rm = TRUE),
      .by = c("year", "territory", "crop")
    ) |>
    dplyr::transmute(
      area_code = as.integer(.data$territory),
      item_prod_code = as.character(.data$crop),
      year = as.integer(.data$year),
      input_type = "manure",
      c_mass_mg = .data$c_mass_mg
    )
}

# Distribute each polity-crop-year carbon mass across cells in proportion to the
# cell's harvested area, then attach the cell-crop area for per-hectare scaling.
.sci_to_grid <- function(components, country_grid, crop_patterns) {
  cells <- .sci_cell_crop_area(country_grid, crop_patterns)
  weights <- cells |>
    dplyr::mutate(
      area_weight = .data$crop_area_ha /
        sum(.data$crop_area_ha),
      .by = c("area_code", "item_prod_code")
    )
  components |>
    dplyr::inner_join(
      weights,
      by = c("area_code", "item_prod_code"),
      relationship = "many-to-many"
    ) |>
    dplyr::mutate(c_mass_mg = .data$c_mass_mg * .data$area_weight) |>
    dplyr::select(
      "lon",
      "lat",
      "area_code",
      "item_prod_code",
      "year",
      "input_type",
      "c_mass_mg",
      "crop_area_ha"
    )
}

# Per-cell harvested area of each crop, scaled by the cell's land fraction.
.sci_cell_crop_area <- function(country_grid, crop_patterns) {
  grid <- .normalize_country_grid(country_grid) |>
    dplyr::mutate(lon = round(.data$lon, 2), lat = round(.data$lat, 2))
  crop_patterns |>
    dplyr::mutate(
      lon = round(.data$lon, 2),
      lat = round(.data$lat, 2),
      item_prod_code = as.character(.data$item_prod_code)
    ) |>
    dplyr::inner_join(grid, by = c("lon", "lat")) |>
    dplyr::transmute(
      lon = .data$lon,
      lat = .data$lat,
      area_code = .data$area_code,
      item_prod_code = .data$item_prod_code,
      crop_area_ha = .data$crop_area_ha * .data$cell_area_frac
    )
}

# Sum component masses to the requested grain, derive per-hectare values and the
# carbon-weighted humified fraction, stamp the method.
.sci_finalise <- function(gridded, resolution, residue_humification) {
  keys <- if (resolution == "polity") {
    c("area_code", "item_prod_code", "year")
  } else {
    c("lon", "lat", "area_code", "item_prod_code", "year")
  }
  gridded |>
    .sci_sum_components(keys) |>
    .sci_per_hectare() |>
    .sci_humified_fraction(keys, residue_humification) |>
    dplyr::mutate(method_c_input = "humified_weighted") |>
    tibble::as_tibble()
}

# Wide per-component carbon mass plus the crop area, summed to the grain. The
# crop area is summed once per cell (the components share a cell's area) so the
# polity grain recovers the polity crop area, the grid grain the cell area.
.sci_sum_components <- function(gridded, keys) {
  cell_keys <- unique(c(keys, "lon", "lat"))
  per_cell <- gridded |>
    dplyr::summarise(
      crop_area_ha = .data$crop_area_ha[1],
      residue_c_mg = sum(
        .data$c_mass_mg[.data$input_type == "crop_residue"]
      ),
      root_c_mg = sum(.data$c_mass_mg[.data$input_type == "root"]),
      manure_c_mg = sum(.data$c_mass_mg[.data$input_type == "manure"]),
      .by = dplyr::all_of(cell_keys)
    )
  per_cell |>
    dplyr::summarise(
      crop_area_ha = sum(.data$crop_area_ha),
      residue_c_mg = sum(.data$residue_c_mg),
      root_c_mg = sum(.data$root_c_mg),
      manure_c_mg = sum(.data$manure_c_mg),
      .by = dplyr::all_of(keys)
    )
}

# Per-hectare carbon by dividing the grain's carbon mass by its crop area.
.sci_per_hectare <- function(x) {
  dplyr::mutate(
    x,
    residue_c_mgc_ha_yr = .sci_safe_div(.data$residue_c_mg, .data$crop_area_ha),
    root_c_mgc_ha_yr = .sci_safe_div(.data$root_c_mg, .data$crop_area_ha),
    manure_c_mgc_ha_yr = .sci_safe_div(.data$manure_c_mg, .data$crop_area_ha),
    total_c_input_mgc_ha_yr = .data$residue_c_mgc_ha_yr +
      .data$root_c_mgc_ha_yr +
      .data$manure_c_mgc_ha_yr
  )
}

.sci_safe_div <- function(num, den) {
  dplyr::if_else(den > 0, num / den, 0)
}

# Carbon-weighted humification fraction across the present components.
.sci_humified_fraction <- function(x, keys, residue_humification) {
  h <- .sci_humification_lookup(residue_humification)
  x |>
    dplyr::mutate(
      total_c_mg = .data$residue_c_mg +
        .data$root_c_mg +
        .data$manure_c_mg,
      humified_fraction = .sci_safe_div(
        .data$residue_c_mg *
          h$crop_residue +
          .data$root_c_mg * h$root +
          .data$manure_c_mg * h$manure,
        .data$total_c_mg
      )
    ) |>
    dplyr::select(
      dplyr::all_of(keys),
      "residue_c_mgc_ha_yr",
      "root_c_mgc_ha_yr",
      "manure_c_mgc_ha_yr",
      "total_c_input_mgc_ha_yr",
      "humified_fraction"
    )
}

.sci_humification_lookup <- function(residue_humification) {
  pick <- function(type) {
    v <- residue_humification$humified_fraction[
      residue_humification$input_type == type
    ]
    if (length(v) != 1L) {
      cli::cli_abort(
        "{.field residue_humification} needs one {.val {type}} row."
      )
    }
    v
  }
  list(
    crop_residue = pick("crop_residue"),
    root = pick("root"),
    manure = pick("manure")
  )
}

.sci_read_npp <- function() {
  cli::cli_abort(
    c(
      "No {.field npp} reader is wired yet.",
      i = "Pass {.code data$npp} (residue and root carbon per polity, crop and \\
           year) from {.fun calculate_npp_carbon_nitrogen}."
    )
  )
}

.sci_read_manure <- function() {
  cli::cli_abort(
    c(
      "No {.field manure} reader is wired yet.",
      i = "Pass {.code data$manure} (the {.field applied} stream of \\
           {.fun build_livestock_nutrient_flows})."
    )
  )
}

.sci_read_country_grid <- function() {
  cli::cli_abort(
    "No {.field country_grid} reader is wired yet; pass {.code data$country_grid}."
  )
}

.sci_read_crop_patterns <- function() {
  cli::cli_abort(
    "No {.field crop_patterns} reader is wired yet; pass {.code data$crop_patterns}."
  )
}
