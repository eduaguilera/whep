#' Build the biodiversity footprint extension.
#'
#' @description
#' Layer land-use biodiversity characterization factors (CFs) on a land
#' extension to produce a biodiversity footprint extension keyed by
#' `(year, area_code, item_cbs_code)`, ready for [build_footprint()].
#'
#' Each sector's occupied land area (hectares, taken from a land extension) is
#' multiplied by the country- and land-use-specific CF in [biodiversity_cf]
#' (Chaudhary & Brooks 2018), giving potential global species loss in
#' Potentially Disappeared Fraction of species (PDF). Crop sectors take the
#' `"crop"` CF and grassland sectors (group "Grass", item_cbs 3000/3002/3003)
#' take the `"pasture"` CF.
#'
#' This layers directly on the land footprint, so the land source is the
#' caller's choice: pass any land extension (e.g. from
#' [build_cropgrids_land_extension()], [get_crop_land_extension()] or
#' [build_grassland_land_extension()]) through `data$land`.
#'
#' Areas whose `area_code` has no ISO3 (region aggregates such as "Rest of
#' Africa") or whose country has no published CF receive no biodiversity row
#' and are treated as zero by [build_footprint()].
#'
#' @param method Characterization-factor source. Currently only
#'   `"chaudhary_brooks_2018"`, the country-aggregated global land-occupation
#'   CFs of Chaudhary & Brooks (2018).
#' @param intensity Land-use intensity assumed for all occupied land, one of
#'   `"intense"` (default), `"light"` or `"minimal"`. Selects the matching CF
#'   and is recorded in `method_biodiversity`.
#' @param data Optional named list of pre-loaded inputs to avoid remote reads:
#'   `land` (a land extension tibble with columns `year`, `area_code`,
#'   `item_cbs_code` and `impact_u`, the occupied land area in hectares) and
#'   `cf` (a CF table overriding [biodiversity_cf]). When `land` is absent it is
#'   built from [build_cropgrids_land_extension()] and
#'   [build_grassland_land_extension()].
#' @param example If `TRUE`, return a small fixture instead of reading remote
#'   data. Defaults to `FALSE`.
#'
#' @return A tibble with columns `year`, `area_code`, `item_cbs_code`,
#'   `impact_u` (potential species loss in PDF) and `method_biodiversity` (the
#'   chosen CF source and intensity).
#'
#' @export
#'
#' @examples
#' build_biodiversity_extension(example = TRUE)
build_biodiversity_extension <- function(
  method = c("chaudhary_brooks_2018"),
  intensity = c("intense", "light", "minimal"),
  data = list(),
  example = FALSE
) {
  method <- match.arg(method)
  intensity <- match.arg(intensity)
  if (isTRUE(example)) {
    return(.example_biodiv_extension())
  }

  land <- if (is.null(data$land)) .biodiversity_default_land() else data$land
  cf <- if (is.null(data$cf)) whep::biodiversity_cf else data$cf

  land |>
    .biodiversity_apply_cf(cf, intensity) |>
    .biodiversity_finalise(method, intensity)
}

# Square metres per hectare: land extensions are in hectares, CFs in PDF / m2.
.m2_per_ha <- function() 1e4

# Default land extension when the caller passes none: self-contained per-crop
# physical cropland (CROPGRIDS) plus grassland occupation.
.biodiversity_default_land <- function() {
  cols <- c("year", "area_code", "item_cbs_code", "impact_u")
  dplyr::bind_rows(
    dplyr::select(build_cropgrids_land_extension(), dplyr::all_of(cols)),
    dplyr::select(build_grassland_land_extension(), dplyr::all_of(cols))
  )
}

# Classify each sector as crop or pasture, map area_code -> ISO3, and multiply
# land area (ha) by the country/land-use CF (PDF / m2) to get PDF.
.biodiversity_apply_cf <- function(land, cf, intensity) {
  area_iso <- .current_area_lookup(include_unmapped = FALSE) |>
    tibble::as_tibble() |>
    dplyr::select("area_code", iso3c = "area_iso3c") |>
    dplyr::filter(!is.na(.data$iso3c)) |>
    dplyr::distinct(.data$area_code, .keep_all = TRUE)

  cf_selected <- cf |>
    dplyr::filter(.data$intensity == .env$intensity) |>
    dplyr::select("iso3c", "land_use_type", "cf_mean")

  land |>
    dplyr::mutate(
      land_use_type = dplyr::if_else(
        as.integer(.data$item_cbs_code) %in% .grass_item_cbs(),
        "pasture",
        "crop"
      )
    ) |>
    dplyr::inner_join(area_iso, by = "area_code") |>
    dplyr::inner_join(cf_selected, by = c("iso3c", "land_use_type")) |>
    dplyr::mutate(impact_u = .data$impact_u * .m2_per_ha() * .data$cf_mean)
}

# Sum to the extension key and tag the method.
.biodiversity_finalise <- function(impacts, method, intensity) {
  impacts |>
    dplyr::summarise(
      impact_u = sum(.data$impact_u, na.rm = TRUE),
      .by = c("year", "area_code", "item_cbs_code")
    ) |>
    dplyr::mutate(
      method_biodiversity = paste0(method, "_occupation_", intensity)
    ) |>
    dplyr::arrange(.data$year, .data$area_code, .data$item_cbs_code) |>
    tibble::as_tibble()
}
