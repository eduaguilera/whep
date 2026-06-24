# Variable registry for the validation harness + WHEP-side extractors.
#
# A "variable" is the unit of what gets validated. Each declares an ARCHETYPE
# that decides how it is checked:
#   - external   : WHEP value vs an authoritative external figure (production,
#                  area, occupation intensity). Comparator: ratio within tol.
#   - parameter  : a coefficient/weight WHEP uses vs an authoritative coefficient
#                  (cycle length, cropping intensity). Comparator: ratio within tol.
#   - internal   : WHEP's own consistency, no external source (time-series
#                  stability). Handled by stability.R.
#
# `grain` says at what level the comparison is made; `gt_sources` is the hint the
# research agent uses to find ground truth. The WHEP value for each variable is
# produced by the extractor functions below (all run from packaged data + the
# public production pin - no LPJmL inputs).

whep_variable_registry <- function() {
  tibble::tribble(
    ~name, ~archetype, ~unit, ~grain, ~tolerance_pct, ~gt_sources,
    "production", "external", "tonnes", "country_crop_year", 8,
    "USDA NASS (local) / EUROSTAT / national stats / FAOSTAT",
    "area", "external", "ha", "country_crop_year", 8,
    "USDA NASS / EUROSTAT / national stats (area harvested)",
    "occupation", "external", "ha_yr_per_t", "country_crop_year", 35,
    "ecoinvent / WFLDB / published LCA land occupation (m2*yr per kg)",
    "land_per_tonne", "external", "ha_yr_per_t", "country_crop_year", 35,
    "LCA literature (full-year basis): physical land held per kg, m2*yr per kg",
    "cycle_length", "parameter", "months", "crop", 30,
    "FAO crop calendars / GGCMI mean growing-season length",
    "cropping_intensity", "parameter", "ratio", "country_crop", 20,
    "GAEZ / SPAM harvested-to-physical (multi-cropping) ratio",
    "stability", "internal", "ratio", "timeseries", NA_real_,
    "none (WHEP's own series); see stability.R"
  )
}

# Print the registry as a manifest the workflow can parse.
emit_variable_manifest <- function() {
  cat("VARIABLES_JSON_START\n")
  cat(jsonlite::toJSON(
    whep_variable_registry(),
    dataframe = "rows",
    auto_unbox = TRUE,
    pretty = TRUE
  ))
  cat("\nVARIABLES_JSON_END\n")
}

# --- WHEP-side extractors ----------------------------------------------------

# Cycle length (months) per crop - the MIRCA2000 occupation weight WHEP uses.
extract_cycle_length <- function() {
  path <- system.file("extdata", "mirca_season.csv", package = "whep")
  readr::read_csv(path, show_col_types = FALSE) |>
    dplyr::transmute(
      item_cbs_code = as.integer(.data$item_cbs_code),
      whep_value = as.numeric(.data$season_months)
    )
}

# Land occupation intensity (ha-yr per tonne) per (area, item, year): the
# land-time WHEP's hectare-year extension ties up, divided by production.
extract_occupation_intensity <- function(production, years = NULL) {
  harvested <- .harvested_area_for_validation(production)
  occ <- build_hayr_land_extension(harvested = harvested) |>
    dplyr::select(
      "year",
      "area_code",
      "item_cbs_code",
      occupation_ha_yr = "impact_u"
    )
  prod_t <- production |>
    dplyr::filter(.data$unit == "tonnes", .data$value > 0) |>
    dplyr::summarise(
      tonnes = sum(.data$value),
      .by = c("year", "area_code", "item_cbs_code")
    )
  out <- occ |>
    dplyr::inner_join(
      prod_t,
      by = c("year", "area_code", "item_cbs_code")
    ) |>
    dplyr::mutate(whep_value = .data$occupation_ha_yr / .data$tonnes)
  .filter_years(out, years)
}

# Land per tonne (ha-yr/t) per (area, item, year): CROPGRIDS physical area
# (land actually held for the year) divided by production. This is the
# FULL-YEAR occupation basis, like-for-like with LCA m2*yr/kg - unlike the
# "active" hayr occupation, it charges the whole year, not just the cycle.
extract_land_per_tonne <- function(production, years = NULL) {
  harvested <- .harvested_area_for_validation(production)
  physical <- build_cropgrids_land_extension(harvested = harvested) |>
    dplyr::select(
      "year",
      "area_code",
      "item_cbs_code",
      physical_ha = "impact_u"
    )
  prod_t <- production |>
    dplyr::filter(.data$unit == "tonnes", .data$value > 0) |>
    dplyr::summarise(
      tonnes = sum(.data$value),
      .by = c("year", "area_code", "item_cbs_code")
    )
  out <- physical |>
    dplyr::inner_join(
      prod_t,
      by = c("year", "area_code", "item_cbs_code")
    ) |>
    dplyr::mutate(whep_value = .data$physical_ha / .data$tonnes)
  .filter_years(out, years)
}

# Cropping intensity (harvested / physical) per (area, item): WHEP's per-crop
# multi-cropping factor, from CROPGRIDS physical area vs harvested area.
extract_cropping_intensity <- function(production, years = NULL) {
  harvested <- .harvested_area_for_validation(production)
  physical <- build_cropgrids_land_extension(harvested = harvested) |>
    dplyr::select(
      "year",
      "area_code",
      "item_cbs_code",
      physical_ha = "impact_u"
    )
  out <- harvested |>
    dplyr::inner_join(
      physical,
      by = c("year", "area_code", "item_cbs_code")
    ) |>
    dplyr::filter(.data$physical_ha > 0) |>
    dplyr::mutate(whep_value = .data$harvested_ha / .data$physical_ha)
  .filter_years(out, years)
}

# --- helpers -----------------------------------------------------------------

.harvested_area_for_validation <- function(production) {
  production |>
    dplyr::filter(.data$unit == "ha", !is.na(.data$item_cbs_code)) |>
    dplyr::summarise(
      harvested_ha = sum(.data$value, na.rm = TRUE),
      .by = c("year", "area_code", "item_cbs_code")
    )
}

.filter_years <- function(x, years) {
  if (is.null(years)) {
    return(x)
  }
  dplyr::filter(x, .data$year %in% as.integer(years))
}
