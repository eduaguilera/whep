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
#' @param years Optional integer vector of calendar years to keep. `NULL`
#'   (default) keeps every year the inputs cover. Threaded into the default NPP
#'   and manure readers so they slice to the requested years; ignored for inputs
#'   supplied via `data`.
#' @param data Optional named list of pre-loaded inputs, each falling back to
#'   its reader when absent: `npp` (soil-returned residue, root and weed carbon
#'   per `area_code`, `item_prod_code`, `year`, columns `residue_soil_c_t`,
#'   `root_c_t` and `weed_npp_c_t`, tonnes C); `manure` (the `applied` tibble of
#'   [build_livestock_nutrient_flows()], with `crop` either an existing
#'   `item_prod_code` or an `item_prod` name from [items_prod_full] (matched
#'   case-insensitively), and `territory` a stringified `area_code` or `iso3c`);
#'   `country_grid` and `crop_patterns` (the spatialization inputs,
#'   `crop_patterns` carrying per-cell `crop_area_ha`); `harvested_area` (the
#'   FAOSTAT national harvested area per `area_code`, `item_prod_code`, `year`
#'   in a `faostat_area_ha` column, used to renormalize each polity-crop-year's
#'   spatialized cell area to the national total so per-hectare densities are
#'   the national density and carbon mass is conserved; defaults to the same
#'   `get_primary_production()` table the NPP reader uses, and is skipped when a
#'   hand-supplied `npp` keeps the pipeline offline unless supplied here);
#'   `residue_humification` (defaults to [residue_humification]).
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
  years = NULL,
  example = FALSE
) {
  resolution <- rlang::arg_match(resolution)
  if (isTRUE(example)) {
    return(.example_soil_carbon_inputs())
  }
  d <- .sci_resolve_inputs(data, years)
  components <- .sci_assemble_components(d$npp, d$manure)
  gridded <- .sci_to_grid(
    components,
    d$country_grid,
    d$crop_patterns,
    d$harvested_area
  )
  .sci_finalise(gridded, resolution, d$residue_humification)
}

# Private helpers ----

# harvested_area is the FAOSTAT national harvested area per (area_code,
# item_prod_code, year); .sci_to_grid renormalizes each polity-crop-year's
# spatialized cell area so its cell-sum equals this national truth (see there).
# It is paired with the default NPP reader: the turnkey path derives it from the
# same get_primary_production() table the NPP chain starts from, while a
# hand-supplied npp keeps the BYO path offline (harvested_area stays NULL and no
# renormalization happens) unless the caller also supplies data$harvested_area.
.sci_resolve_inputs <- function(data, years = NULL) {
  harvested_area <- data$harvested_area %||%
    (if (is.null(data$npp)) .sci_read_harvested_area(years) else NULL)
  list(
    npp = data$npp %||% .sci_read_npp(years),
    manure = data$manure %||% .sci_read_manure(years),
    country_grid = data$country_grid %||% .sci_read_country_grid(),
    crop_patterns = data$crop_patterns %||% .sci_read_crop_patterns(),
    harvested_area = harvested_area,
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
      item_prod_code = .sci_manure_crop_prod_code(.data$crop)
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

.sci_manure_crop_prod_code <- function(crop) {
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
# The spatialized crop_area_ha is only a spatial PATTERN: its cell-sum can
# undershoot (or overshoot) the FAOSTAT national harvested area, which would
# distort the per-hectare density (per-ha = polity mass / sum of spatialized
# area). When harvested_area is supplied, renormalize each polity-crop-year's
# cell area so its cell-sum equals the FAOSTAT national area, making per-ha the
# national density and conserving the polity carbon mass (see
# .sci_rescale_cell_area). This leaves area_weight, and therefore the cell mass
# distribution, unchanged.
.sci_to_grid <- function(
  components,
  country_grid,
  crop_patterns,
  harvested_area = NULL
) {
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
    .sci_rescale_cell_area(harvested_area) |>
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

# Renormalize the per-cell crop area to the FAOSTAT national harvested area.
# Since area_weight = crop_area_ha / sum(crop_area_ha) per polity-crop, the
# scaled cell area crop_area_ha * faostat_area / sum(crop_area_ha) is exactly
# area_weight * faostat_area. Per-ha then becomes (polity_mass * area_weight) /
# (area_weight * faostat_area) = polity_mass / faostat_area (uniform across the
# polity's cells for that crop), and the cell masses still sum to the polity
# mass, so mass is conserved. Groups with no supplied FAOSTAT area (NA/<=0) keep
# their spatialized area unchanged, preserving the offline BYO-inputs path.
.sci_rescale_cell_area <- function(joined, harvested_area) {
  if (is.null(harvested_area) || nrow(harvested_area) == 0) {
    return(joined)
  }
  faostat <- harvested_area |>
    dplyr::transmute(
      area_code = as.integer(.data$area_code),
      item_prod_code = as.character(.data$item_prod_code),
      year = as.integer(.data$year),
      faostat_area_ha = as.numeric(.data$faostat_area_ha)
    )
  joined |>
    dplyr::left_join(
      faostat,
      by = c("area_code", "item_prod_code", "year")
    ) |>
    dplyr::mutate(
      crop_area_ha = dplyr::if_else(
        is.finite(.data$faostat_area_ha) & .data$faostat_area_ha > 0,
        .data$area_weight * .data$faostat_area_ha,
        .data$crop_area_ha
      )
    ) |>
    dplyr::select(-"faostat_area_ha")
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
  # Pre-mask the carbon mass by input_type ONCE (vectorised) so the per-cell
  # aggregation is a plain GForce grouped sum, instead of a dplyr summarise that
  # re-resolves .data and sub-sets c_mass_mg per group. At global cell grain the
  # per-group .data-pronoun path dominated the whole soil-carbon-input read.
  # fifelse propagates NA (in c_mass_mg or input_type) exactly as the
  # `c_mass_mg[input_type == ...]` subset did; data.table `by=` keeps
  # first-appearance group order, matching dplyr `.by`.
  dt <- data.table::as.data.table(gridded)
  dt[, `:=`(
    .residue = data.table::fifelse(input_type == "crop_residue", c_mass_mg, 0),
    .root = data.table::fifelse(input_type == "root", c_mass_mg, 0),
    .weed = data.table::fifelse(input_type == "weed", c_mass_mg, 0),
    .manure = data.table::fifelse(input_type == "manure", c_mass_mg, 0)
  )]
  per_cell <- dt[,
    .(
      crop_area_ha = crop_area_ha[1L],
      residue_c_mg = sum(.residue),
      root_c_mg = sum(.root),
      weed_c_mg = sum(.weed),
      manure_c_mg = sum(.manure)
    ),
    by = cell_keys
  ]
  out <- per_cell[,
    .(
      crop_area_ha = sum(crop_area_ha),
      residue_c_mg = sum(residue_c_mg),
      root_c_mg = sum(root_c_mg),
      weed_c_mg = sum(weed_c_mg),
      manure_c_mg = sum(manure_c_mg)
    ),
    by = keys
  ]
  tibble::as_tibble(as.data.frame(out))
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

# -- Default input readers ----------------------------------------------------

# Soil-returned residue, root and weed carbon per polity, crop and year, from
# the crop-NPP chain run on the real primary production: get_primary_production()
# (product tonnage and harvested area per crop) -> calculate_crop_npp() (residue
# and root dry matter) -> calculate_residue_destinies() (the soil-returned
# residue fraction) -> calculate_npp_carbon_nitrogen() (the carbon partition,
# including residue_soil_c_t, root_c_t and weed_npp_c_t).
.sci_read_npp <- function(years = NULL) {
  .sci_npp_from_primary_prod(.filter_years(get_primary_production(), years))
}

# FAOSTAT national harvested area (ha) per (area_code, item_prod_code, year),
# the truth .sci_to_grid renormalizes the spatialized cell area to. Read from
# the same get_primary_production() table the NPP chain starts from, filtered to
# crop harvested-area rows (grassland and livestock dropped, matching
# .sci_crop_prod_wide). One table serves every input_type (residue/root/weed and
# manure), all keyed by (area_code, item_prod_code, year).
.sci_read_harvested_area <- function(years = NULL) {
  .sci_harvested_area(.filter_years(get_primary_production(), years))
}

.sci_harvested_area <- function(primary_prod) {
  grass <- c(3000L, 3002L, 3003L)
  primary_prod |>
    dplyr::filter(
      .data$unit == "ha",
      !is.na(.data$item_prod_code),
      is.na(.data$live_anim_code),
      !.data$item_cbs_code %in% grass
    ) |>
    dplyr::summarise(
      faostat_area_ha = sum(.data$value, na.rm = TRUE),
      .by = c("area_code", "item_prod_code", "year")
    ) |>
    dplyr::filter(.data$faostat_area_ha > 0) |>
    dplyr::transmute(
      area_code = as.integer(.data$area_code),
      item_prod_code = as.character(.data$item_prod_code),
      year = as.integer(.data$year),
      faostat_area_ha = .data$faostat_area_ha
    )
}

# Run the crop-NPP carbon chain on a get_primary_production() table and keep the
# soil-carbon input columns build_soil_carbon_inputs() consumes.
.sci_npp_from_primary_prod <- function(primary_prod) {
  primary_prod |>
    .sci_crop_prod_wide() |>
    calculate_crop_npp() |>
    calculate_residue_destinies(method = "krausmann_regional") |>
    calculate_npp_carbon_nitrogen() |>
    dplyr::transmute(
      area_code = as.integer(.data$area_code),
      item_prod_code = as.character(.data$item_prod_code),
      year = as.integer(.data$year),
      residue_soil_c_t = .data$residue_soil_c_t,
      root_c_t = .data$root_c_t,
      weed_npp_c_t = .data$weed_npp_c_t
    )
}

# Reshape the long primary-production table to one crop-polity-year row carrying
# production tonnage and harvested area, plus the Krausmann/HANPP regions the
# residue-destiny split needs. Grassland and livestock rows are dropped; only
# crop production (tonnes) and area (ha) are kept.
.sci_crop_prod_wide <- function(primary_prod) {
  grass <- c(3000L, 3002L, 3003L)
  wide <- primary_prod |>
    dplyr::filter(
      .data$unit %in% c("tonnes", "ha"),
      !is.na(.data$item_prod_code),
      is.na(.data$live_anim_code),
      !.data$item_cbs_code %in% grass
    ) |>
    dplyr::mutate(
      unit = dplyr::if_else(.data$unit == "tonnes", "production_t", "area_ha")
    ) |>
    dplyr::summarise(
      value = sum(.data$value, na.rm = TRUE),
      .by = c("area_code", "item_prod_code", "year", "unit")
    ) |>
    tidyr::pivot_wider(
      names_from = "unit",
      values_from = "value",
      values_fill = 0
    )
  wide |>
    dplyr::filter(.data$production_t > 0, .data$area_ha > 0) |>
    dplyr::mutate(item_prod_code = as.character(.data$item_prod_code)) |>
    dplyr::left_join(.sci_crop_regions(), by = "area_code")
}

# The Krausmann recovery and HANPP feed-use regions per polity, from
# whep::regions_full, keyed by the legacy numeric area_code.
.sci_crop_regions <- function() {
  whep::regions_full |>
    dplyr::transmute(
      area_code = as.integer(.data$code),
      region_krausmann = .data$region_krausmann,
      region_hanpp = .data$region_HANPP
    ) |>
    dplyr::distinct(.data$area_code, .keep_all = TRUE)
}

# Manure carbon applied to cropland, the `applied` stream of
# build_livestock_nutrient_flows(). The realised feed intake it consumes is the
# redistribute_feed() national result (the same contract estimate_n_excretion
# needs: year, territory, sub_territory, livestock_category, item_cbs_code,
# feed_quality, intake_dm_t), built turnkey from the cached production and CBS
# pins via .run_redistribute_national() (get_feed_intake()'s own internal engine;
# its public output is a reshaped per-animal table that is NOT the excretion
# contract, so the engine result is used directly). feed_mode is "historical"
# (distribute_surplus = FALSE). allocate_manure_to_land() requires a cropland
# receptivity layer, supplied turnkey by .sci_manure_crop_layer() with the
# fixed-ceiling (EU Nitrates-Directive 170 kg N/ha) cap, which needs only
# harvested area. The `crop` it emits is the item_prod_code, so it resolves
# straight back through .sci_manure_crop_prod_code().
.sci_read_manure <- function(years = NULL) {
  production <- .filter_years(get_primary_production(), years)
  cbs <- .filter_years(get_wide_cbs(), years)
  intake <- .run_redistribute_national(
    production = production,
    cbs = cbs,
    demand_tier = "ipcc",
    options = list(distribute_surplus = FALSE)
  )
  build_livestock_nutrient_flows(
    intake,
    resolution = "national",
    methods = list(allocation = list(cap_method = "fixed_ceiling")),
    gridded = list(crops = .sci_manure_crop_layer(production))
  )$applied
}

# Turnkey cropland receptivity layer for the manure allocation: per polity-crop
# harvested area (ha) from the primary production, keyed to the manure engine's
# (year, territory, sub_territory, crop) grain. `crop` is the item_prod_code so
# it resolves straight back through .sci_manure_crop_prod_code();
# manure_n_receptivity is the harvested area, so collected manure is spread
# across a polity's crops in proportion to each crop's area (the same basis
# .sci_to_grid re-grids by); the fixed_ceiling cap needs only crop_area_ha.
# Grassland and livestock rows are excluded, matching .sci_crop_prod_wide().
.sci_manure_crop_layer <- function(production) {
  grass <- c(3000L, 3002L, 3003L)
  production |>
    dplyr::filter(
      .data$unit == "ha",
      !is.na(.data$item_prod_code),
      is.na(.data$live_anim_code),
      !.data$item_cbs_code %in% grass
    ) |>
    dplyr::summarise(
      crop_area_ha = sum(.data$value, na.rm = TRUE),
      .by = c("area_code", "item_prod_code", "year")
    ) |>
    dplyr::filter(.data$crop_area_ha > 0) |>
    dplyr::transmute(
      year = as.integer(.data$year),
      territory = as.character(.data$area_code),
      sub_territory = NA_character_,
      crop = as.character(.data$item_prod_code),
      manure_n_receptivity = .data$crop_area_ha,
      crop_area_ha = .data$crop_area_ha
    )
}

# The cell -> polity crosswalk from the spatialization country grid.
.sci_read_country_grid <- function() {
  whep_read_file("spatialize-country-grid")
}

# Per-cell crop harvested area (ha) used to grid the polity-crop carbon masses.
# crop_patterns.parquet (pin "spatialize-crop-patterns") carries only the static
# per-cell harvest_fraction, so the absolute per-cell crop area is
# harvest_fraction times the cell's cropland area from the gridded-cropland pin
# ("spatialize-gridded-cropland", per-cell-year cropland_ha, collapsed to a
# time-invariant per-cell mean to match the time-invariant crop_patterns
# contract).
.sci_read_crop_patterns <- function() {
  .sci_combine_crop_patterns(
    whep_read_file("spatialize-crop-patterns"),
    whep_read_file("spatialize-gridded-cropland")
  )
}

# Turn the static per-cell harvest_fraction and the per-cell-year gridded
# cropland area into a time-invariant per-cell-crop area (ha):
# crop_area_ha = harvest_fraction * mean-over-years cropland_ha. Kept pure so
# it is testable without the pins.
.sci_combine_crop_patterns <- function(patterns, cropland) {
  patterns <- patterns |>
    dplyr::mutate(
      lon = round(.data$lon, 2),
      lat = round(.data$lat, 2),
      item_prod_code = as.character(.data$item_prod_code)
    )
  cropland <- cropland |>
    dplyr::mutate(lon = round(.data$lon, 2), lat = round(.data$lat, 2)) |>
    dplyr::summarise(
      cropland_ha = mean(.data$cropland_ha, na.rm = TRUE),
      .by = c("lon", "lat")
    )
  patterns |>
    dplyr::inner_join(cropland, by = c("lon", "lat")) |>
    dplyr::transmute(
      lon = .data$lon,
      lat = .data$lat,
      item_prod_code = .data$item_prod_code,
      crop_area_ha = .data$harvest_fraction * .data$cropland_ha
    )
}
