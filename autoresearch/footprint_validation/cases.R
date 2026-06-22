# Deterministic case-grid generator for the validation harness.
#
# Builds a Cartesian grid of validation probes from WHEP production:
#   top-N producing countries (ranked per crop-year by tonnes)
#     x  the requested years
#     x  the requested crops
#     x  the requested layers (production tonnes / harvested area ha)
#
# Only entities that map to a real ISO3 country are kept, so FAO aggregates
# (e.g. area 351 "China (PRC)", regional/world totals) are excluded - they have
# no iso3c in regions_full. probe_ids follow the corpus convention
# "<prefix>-<iso3>-<crop>-<year>" so generated cases reuse any pinned ground
# truth with the same id.

# production: a WHEP production tibble (area_code, item_prod_code, year, unit,
#   value). lookups: whep_validation_lookups(). crops: production item names
#   (e.g. c("Wheat", "Rice")). layers: any of "production", "area".
generate_cases <- function(
  production,
  lookups,
  years,
  crops,
  top_n = 5L,
  layers = c("production")
) {
  layer_spec <- .case_layer_spec(layers)
  crop_codes <- .case_crop_codes(crops, lookups)
  iso_by_area <- .case_area_iso(lookups)

  ranked <- .case_top_producers(
    production,
    crop_codes,
    iso_by_area,
    years,
    top_n
  )

  tidyr::expand_grid(ranked, layer_spec) |>
    dplyr::transmute(
      probe_id = sprintf(
        "%s-%s-%s-%d",
        .data$prefix,
        .data$area_iso3,
        .data$crop_slug,
        .data$year
      ),
      pool = "grid",
      layer = .data$layer,
      area_iso3 = .data$area_iso3,
      item_name = .data$item_name,
      year = .data$year,
      element = NA_character_,
      unit = .data$unit,
      notes = sprintf("top-%d producer (rank %d by tonnes)", top_n, .data$rank)
    ) |>
    dplyr::distinct(.data$probe_id, .keep_all = TRUE)
}

# --- helpers -----------------------------------------------------------------

.case_layer_spec <- function(layers) {
  spec <- tibble::tribble(
    ~layer, ~prefix, ~unit,
    "production", "prod", "tonnes",
    "area", "area", "ha"
  )
  unknown <- setdiff(layers, spec$layer)
  if (length(unknown) > 0) {
    cli::cli_abort("Unknown layer{?s}: {.val {unknown}}.")
  }
  dplyr::filter(spec, .data$layer %in% layers)
}

.case_crop_codes <- function(crops, lookups) {
  out <- lookups$items_prod |>
    dplyr::transmute(
      item_name = .data$item_prod,
      item_prod_code = suppressWarnings(as.integer(.data$item_prod_code))
    ) |>
    dplyr::filter(.data$item_name %in% crops, !is.na(.data$item_prod_code)) |>
    dplyr::distinct(.data$item_name, .keep_all = TRUE) |>
    dplyr::mutate(crop_slug = .case_slug(.data$item_name))
  missing <- setdiff(crops, out$item_name)
  if (length(missing) > 0) {
    cli::cli_warn("No production item match for crop{?s}: {.val {missing}}.")
  }
  out
}

.case_area_iso <- function(lookups) {
  lookups$regions |>
    dplyr::transmute(
      area_code = suppressWarnings(as.integer(.data$code)),
      area_iso3 = .data$iso3c
    ) |>
    dplyr::filter(!is.na(.data$area_code), !is.na(.data$area_iso3)) |>
    dplyr::distinct(.data$area_code, .keep_all = TRUE)
}

.case_top_producers <- function(
  production,
  crop_codes,
  iso_by_area,
  years,
  top_n
) {
  production |>
    dplyr::filter(
      .data$unit == "tonnes",
      .data$year %in% years,
      .data$item_prod_code %in% crop_codes$item_prod_code
    ) |>
    dplyr::transmute(
      area_code = suppressWarnings(as.integer(.data$area_code)),
      item_prod_code = suppressWarnings(as.integer(.data$item_prod_code)),
      year = suppressWarnings(as.integer(.data$year)),
      value = .data$value
    ) |>
    dplyr::inner_join(iso_by_area, by = "area_code") |>
    dplyr::inner_join(crop_codes, by = "item_prod_code") |>
    dplyr::filter(.data$value > 0) |>
    dplyr::slice_max(
      .data$value,
      n = top_n,
      by = c("item_prod_code", "year"),
      with_ties = FALSE
    ) |>
    dplyr::mutate(
      rank = dplyr::row_number(dplyr::desc(.data$value)),
      .by = c("item_prod_code", "year")
    )
}

.case_slug <- function(item_name) {
  item_name |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("[^a-z0-9]+", "-") |>
    stringr::str_replace_all("^-|-$", "")
}
