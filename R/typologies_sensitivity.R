#' Run one-at-a-time sensitivity analysis on typology thresholds.
#'
#' @description Varies each threshold by +/- `variation` (one at a time)
#'   and reports the share of province-year observations that retain the
#'   same typology as the baseline classification.
#' @param n_prov_destiny Nitrogen flows tibble. If `NULL`, loaded
#'   automatically.
#' @param variation Relative variation applied to each threshold (default
#'   0.2 = 20%).
#' @return A tibble with columns `threshold`, `direction`, and
#'   `agreement_pct`.
#' @export
run_typology_sensitivity <- function(
  n_prov_destiny = NULL,
  variation = 0.2
) {
  thresholds <- .sensitivity_thresholds()
  baseline <- create_typologies_spain(
    n_prov_destiny = n_prov_destiny,
    make_map = FALSE
  )

  grid <- .oat_threshold_grid(thresholds, variation)

  grid |>
    purrr::pmap(function(threshold, direction, th) {
      result <- .reclassify_typology(baseline, th)
      .compute_agreement(baseline, result, threshold, direction)
    }) |>
    purrr::list_rbind()
}

# --- Private helpers ---------------------------------------------------------

.sensitivity_thresholds <- function() {
  list(
    synthetic_crop_int = 0.4,
    crop_productivity_int = 10,
    synthetic_crop_ext = 0.4,
    crop_productivity_ext = 10,
    livestock_density_int = 1.3,
    imported_feed_int = 0.6,
    feed_seminatural_int = 0.4,
    livestock_density_ext_lo = 1.0,
    livestock_density_ext_hi = 1.3,
    imported_feed_ext = 0.6,
    feed_seminatural_ext = 0.4,
    local_feed_connected = 0.3,
    manure_connected = 0.25,
    crop_productivity_connected = 30,
    local_feed_disconnected = 0.6,
    manure_disconnected = 0.6
  )
}

.reclassify_typology <- function(indicators, th) {
  indicators |>
    dplyr::mutate(
      Typology_base = dplyr::case_when(
        production_seminatural > production_crops ~
          "Semi-natural agroecosystems",
        production_crops > animal_ingestion &
          synthetic_share > th$synthetic_crop_int &
          crop_productivity >= th$crop_productivity_int ~
          "Specialized cropping systems (intensive)",
        production_crops > animal_ingestion &
          synthetic_share <= th$synthetic_crop_ext &
          crop_productivity < th$crop_productivity_ext ~
          "Specialized cropping systems (extensive)",
        Livestock_density > th$livestock_density_int &
          imported_feed_share > th$imported_feed_int &
          feed_from_seminatural_share < th$feed_seminatural_int ~
          "Specialized livestock systems (intensive)",
        Livestock_density > th$livestock_density_ext_lo &
          Livestock_density <= th$livestock_density_ext_hi &
          imported_feed_share > th$imported_feed_ext &
          feed_from_seminatural_share < th$feed_seminatural_ext ~
          "Specialized livestock systems (extensive)",
        local_feed_share > th$local_feed_connected &
          Manure_share > th$manure_connected &
          crop_productivity >= th$crop_productivity_connected ~
          "Connected crop-livestock systems (intensive)",
        local_feed_share > th$local_feed_connected &
          Manure_share > th$manure_connected &
          crop_productivity < th$crop_productivity_connected ~
          "Connected crop-livestock systems (extensive)",
        local_feed_share < th$local_feed_disconnected &
          Manure_share < th$manure_disconnected ~
          "Disconnected crop-livestock systems (intensive)",
        TRUE ~ "Disconnected crop-livestock systems (extensive)"
      )
    )
}

.oat_threshold_grid <- function(thresholds, variation) {
  names(thresholds) |>
    purrr::map(function(name) {
      val <- thresholds[[name]]
      lo <- utils::modifyList(
        thresholds,
        stats::setNames(list(val * (1 - variation)), name)
      )
      hi <- utils::modifyList(
        thresholds,
        stats::setNames(list(val * (1 + variation)), name)
      )
      tibble::tibble(
        threshold = name,
        direction = c("low", "high"),
        th = list(lo, hi)
      )
    }) |>
    purrr::list_rbind()
}

.compute_agreement <- function(baseline, result, threshold, direction) {
  baseline |>
    dplyr::ungroup() |>
    dplyr::select(year, province_name, Typology_base) |>
    dplyr::left_join(
      dplyr::select(result, year, province_name, Typology_base),
      by = c("year", "province_name"),
      suffix = c("_base", "_new")
    ) |>
    dplyr::summarise(
      agreement_pct = mean(
        Typology_base_base == Typology_base_new,
        na.rm = TRUE
      ) *
        100
    ) |>
    dplyr::mutate(threshold = threshold, direction = direction) |>
    dplyr::select(threshold, direction, agreement_pct)
}
