#' Assemble soil carbon inputs per cell, crop and year.
#'
#' @description
#' Builds the carbon returned to soil from crop residues, crop roots, weeds and
#' applied manure, as the carbon-input layer the soil-organic-carbon turnover
#' models ([calculate_soc_dynamics()]) consume. Soil-returned residue carbon
#' (residue net of the fraction removed for feed, fuel and burning), root carbon
#' and weed (spontaneous-grass) carbon come from
#' [calculate_npp_carbon_nitrogen()] (per polity, crop and year); manure carbon
#' comes from [build_livestock_nutrient_flows()]'s `applied` stream. The four
#' component carbon masses are converted to megagrams of carbon (1 tonne =
#' 1 Mg), gridded to cells in proportion to each crop's harvested area, and
#' divided by the cell-crop area to give Mg C per hectare per year. A
#' carbon-weighted humification fraction is computed per cell-year from
#' [residue_humification], with the weed carbon humified at the weed
#' (spontaneous-grass) coefficient.
#'
#' At `"polity"` resolution the component carbon masses are summed back to
#' `(area_code, item_prod_code, year)` and the per-hectare values and humified
#' fraction re-derived from the polity totals.
#'
#' @param resolution `"grid"` (default, per cell) or `"polity"` (aggregated to
#'   `area_code`).
#' @param data Optional named list of pre-loaded inputs, each falling back to
#'   its reader when absent: `npp` (soil-returned residue, root and weed carbon
#'   per `area_code`, `item_prod_code`, `year`, columns `residue_soil_c_t`,
#'   `root_c_t` and `weed_npp_c_t`, tonnes C); `manure` (the `applied` tibble of
#'   [build_livestock_nutrient_flows()], with `crop` either an existing
#'   `item_prod_code` or an `item_prod` name from [items_prod_full] (matched
#'   case-insensitively), and `territory` a stringified `area_code` or `iso3c`);
#'   `country_grid` and `crop_patterns` (the spatialization inputs,
#'   `crop_patterns` carrying per-cell `crop_area_ha`); `residue_humification`
#'   (defaults to [residue_humification]).
#' @param example If `TRUE`, return a small fixture instead of reading remote
#'   data. Defaults to `FALSE`.
#'
#' @return A tibble keyed by `(lon, lat, area_code, item_prod_code, year)` at
#'   `"grid"` resolution (or `(area_code, item_prod_code, year)` at
#'   `"polity"`), with `residue_c_mgc_ha_yr`, `root_c_mgc_ha_yr`,
#'   `weed_c_mgc_ha_yr`, `manure_c_mgc_ha_yr`, `total_c_input_mgc_ha_yr`,
#'   `humified_fraction` and `method_c_input`.
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
# the carbon mass in Mg C (1 tonne = 1 Mg) and the humification key. Residue
# uses residue_soil_c_t (residue net of feed/fuel/burning removal), never the
# gross residue_c_t; weeds carry the combined above- and below-ground weed
# carbon humified at the spontaneous-grass coefficient.
.sci_assemble_components <- function(npp, manure) {
  .sci_check_npp(npp)
  residue <- .sci_npp_component(npp, "crop_residue", "residue_soil_c_t")
  root <- .sci_npp_component(npp, "root", "root_c_t")
  weed <- .sci_npp_component(npp, "weed", "weed_npp_c_t")
  dplyr::bind_rows(residue, root, weed, .sci_manure_components(manure))
}

.sci_npp_component <- function(npp, input_type, c_col) {
  npp |>
    dplyr::transmute(
      area_code = as.integer(.data$area_code),
      item_prod_code = as.character(.data$item_prod_code),
      year = as.integer(.data$year),
      input_type = input_type,
      c_mass_mg = .data[[c_col]]
    )
}

.sci_check_npp <- function(npp) {
  required <- c("residue_soil_c_t", "root_c_t", "weed_npp_c_t")
  missing <- required[
    !purrr::map_lgl(required, \(col) rlang::has_name(npp, col))
  ]
  if (length(missing) == 0) {
    return(invisible(npp))
  }
  cli::cli_abort(c(
    "{.field npp} is missing required carbon column{?s} {.field {missing}}.",
    i = "Supply {.code data$npp} from {.fun calculate_npp_carbon_nitrogen} run
         with a {.field residue_soil_dm_t} column (from
         {.fun calculate_residue_destinies}) so soil-returned residue carbon is
         used, not gross residue."
  ))
}

# Manure carbon applied to cropland, keyed like the NPP components. The manure
# engine emits crop names, while hand-supplied applied streams may already carry
# item_prod_code strings; resolve either form through items_prod_full. Territory
# is a stringified area_code or an iso3c, resolved via the same helper the
# N-inputs manure engine uses (both mappings abort rather than silently emit NA).
.sci_manure_components <- function(manure) {
  manure |>
    dplyr::filter(
      .data$land_use == "Cropland",
      !is.na(.data$crop)
    ) |>
    dplyr::mutate(
      item_prod_code = .sci_manure_crop_to_item_prod_code(.data$crop)
    ) |>
    dplyr::summarise(
      c_mass_mg = sum(.data$applied_c, na.rm = TRUE),
      .by = c("year", "territory", "item_prod_code")
    ) |>
    dplyr::transmute(
      area_code = .manure_territory_to_area_code(.data$territory),
      item_prod_code = .data$item_prod_code,
      year = as.integer(.data$year),
      input_type = "manure",
      c_mass_mg = .data$c_mass_mg
    )
}

.sci_manure_crop_to_item_prod_code <- function(crop) {
  crop <- trimws(as.character(crop))
  lookup <- whep::items_prod_full |>
    dplyr::transmute(
      item_prod_code = trimws(as.character(.data$item_prod_code)),
      crop_lower = stringr::str_to_lower(trimws(as.character(.data$item_prod)))
    ) |>
    dplyr::filter(
      !is.na(.data$item_prod_code),
      .data$item_prod_code != ""
    )

  codes <- unique(lookup$item_prod_code)
  resolved <- rep(NA_character_, length(crop))
  is_code <- !is.na(crop) & crop %in% codes
  resolved[is_code] <- crop[is_code]

  name_lookup <- lookup |>
    dplyr::filter(
      !is.na(.data$crop_lower),
      .data$crop_lower != ""
    ) |>
    dplyr::distinct(.data$crop_lower, .data$item_prod_code)
  duplicate_name <- duplicated(name_lookup$crop_lower) |
    duplicated(name_lookup$crop_lower, fromLast = TRUE)
  unambiguous_names <- name_lookup[!duplicate_name, ]

  needs_name <- !is.na(crop) & is.na(resolved)
  name_match <- match(
    stringr::str_to_lower(crop[needs_name]),
    unambiguous_names$crop_lower
  )
  resolved[needs_name] <- unambiguous_names$item_prod_code[name_match]

  unresolved <- unique(crop[!is.na(crop) & is.na(resolved)])
  if (length(unresolved) > 0) {
    cli::cli_abort(c(
      "Could not resolve manure {.field crop} to an {.field item_prod_code}.",
      i = "Unrecognised or ambiguous value{?s}: {.val {unresolved}}. Expected
           an existing {.field item_prod_code} or an {.field item_prod} name in
           {.code whep::items_prod_full} (matched case-insensitively)."
    ))
  }
  resolved
}

# Distribute each polity-crop-year carbon mass across cells in proportion to the
# cell's harvested area, then attach the cell-crop area for per-hectare scaling.
.sci_to_grid <- function(components, country_grid, crop_patterns) {
  cells <- .sci_cell_crop_area(country_grid, crop_patterns)
  weights <- cells |>
    # Zero, missing and non-finite harvested areas provide no spatial support.
    # Keeping a zero-only group here would divide 0 by 0 and silently turn its
    # carbon mass into NaN; dropping it lets .sci_warn_unspatialized() report
    # the unsupported polity-crop group through the existing warning path.
    dplyr::filter(
      is.finite(.data$crop_area_ha),
      .data$crop_area_ha > 0
    ) |>
    dplyr::mutate(
      area_weight = .data$crop_area_ha /
        sum(.data$crop_area_ha),
      .by = c("area_code", "item_prod_code")
    )
  .sci_warn_unspatialized(components, weights)
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

# The inner_join that spatializes polity-crop carbon to cells silently drops any
# (area_code, item_prod_code) present in the carbon components but absent from
# the (time-invariant) crop_patterns. Surface that carbon loss rather than
# letting it vanish, matching this codebase's no-silent-failures convention.
.sci_warn_unspatialized <- function(components, weights) {
  lost <- components |>
    dplyr::anti_join(
      dplyr::distinct(weights, .data$area_code, .data$item_prod_code),
      by = c("area_code", "item_prod_code")
    )
  if (nrow(lost) == 0) {
    return(invisible(NULL))
  }
  crops <- sort(unique(lost$item_prod_code))
  cli::cli_warn(c(
    "!" = "{nrow(lost)} polity-crop carbon component{?s}
           ({round(sum(lost$c_mass_mg), 3)} Mg C) had no crop-pattern cells and
           {?was/were} dropped from the gridded soil carbon input.",
    i = "Unspatialized item_prod_code{?s}: {.val {crops}}. Add {?its/their}
         cells to {.field crop_patterns} to retain the carbon."
  ))
  invisible(lost)
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
      weed_c_mg = sum(.data$c_mass_mg[.data$input_type == "weed"]),
      manure_c_mg = sum(.data$c_mass_mg[.data$input_type == "manure"]),
      .by = dplyr::all_of(cell_keys)
    )
  per_cell |>
    dplyr::summarise(
      crop_area_ha = sum(.data$crop_area_ha),
      residue_c_mg = sum(.data$residue_c_mg),
      root_c_mg = sum(.data$root_c_mg),
      weed_c_mg = sum(.data$weed_c_mg),
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
    weed_c_mgc_ha_yr = .sci_safe_div(.data$weed_c_mg, .data$crop_area_ha),
    manure_c_mgc_ha_yr = .sci_safe_div(.data$manure_c_mg, .data$crop_area_ha),
    total_c_input_mgc_ha_yr = .data$residue_c_mgc_ha_yr +
      .data$root_c_mgc_ha_yr +
      .data$weed_c_mgc_ha_yr +
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
        .data$weed_c_mg +
        .data$manure_c_mg,
      humified_fraction = .sci_safe_div(
        .data$residue_c_mg *
          h$crop_residue +
          .data$root_c_mg * h$root +
          .data$weed_c_mg * h$weed +
          .data$manure_c_mg * h$manure,
        .data$total_c_mg
      )
    ) |>
    dplyr::select(
      dplyr::all_of(keys),
      "residue_c_mgc_ha_yr",
      "root_c_mgc_ha_yr",
      "weed_c_mgc_ha_yr",
      "manure_c_mgc_ha_yr",
      "total_c_input_mgc_ha_yr",
      "humified_fraction"
    )
}

.sci_humification_lookup <- function(residue_humification) {
  list(
    crop_residue = .sci_humification_pick(residue_humification, "crop_residue"),
    root = .sci_humification_pick(residue_humification, "root"),
    weed = .sci_humification_pick(residue_humification, "weed"),
    manure = .sci_humification_pick(residue_humification, "manure")
  )
}

.sci_humification_pick <- function(residue_humification, type) {
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

.sci_read_npp <- function() {
  cli::cli_abort(
    c(
      "No {.field npp} reader is wired yet.",
      i = "Pass {.code data$npp} (soil-returned residue, root and weed carbon \\
           per polity, crop and year) from {.fun calculate_npp_carbon_nitrogen}."
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
