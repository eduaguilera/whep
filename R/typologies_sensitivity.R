#' Run one-at-a-time sensitivity analysis on typology thresholds.
#'
#' @description Varies each threshold by +/- `variation` (one at a time)
#'   and reports the share of province-year observations that retain the
#'   same typology as the baseline classification.
#' @param n_prov_destiny Nitrogen flows tibble. If `NULL`, loaded
#'   automatically.
#' @param variation Relative variation applied to each threshold (default
#'   0.2 = 20%).
#' @param baseline Pre-computed indicator table from
#'   `create_typologies_spain()`, carrying `year`, `province_name` and the
#'   indicator columns the thresholds act on. If `NULL`, computed
#'   automatically (slow).
#' @return A tibble with columns `threshold`, `direction`, and
#'   `agreement_pct`.
#' @export
#'
#' @examples
#' # `baseline` carries the indicator columns the thresholds act on plus the
#' # unperturbed `Typology_base` each perturbation is compared against. Two
#' # provinces show the output shape; the real analysis runs over 50 provinces
#' # and 1860-2021.
#' baseline <- tibble::tribble(
#'   ~year,
#'   ~province_name,
#'   ~production_seminatural,
#'   ~production_crops,
#'   ~animal_ingestion,
#'   ~synthetic_share,
#'   ~crop_productivity,
#'   ~Livestock_density,
#'   ~imported_feed_share,
#'   ~feed_from_seminatural_share,
#'   ~local_feed_share,
#'   ~Manure_share,
#'   ~Typology_base,
#'   2000,
#'   "A",
#'   1,
#'   100,
#'   5,
#'   0.8,
#'   40,
#'   0.1,
#'   0.1,
#'   0.1,
#'   0.1,
#'   0.1,
#'   "Specialized cropping systems (intensive)",
#'   2000,
#'   "B",
#'   1,
#'   10,
#'   50,
#'   0.1,
#'   40,
#'   0.5,
#'   0.1,
#'   0.5,
#'   0.5,
#'   0.5,
#'   "Connected crop-livestock systems (intensive)"
#' )
#' sensitivity <- run_typology_sensitivity(baseline = baseline)
run_typology_sensitivity <- function(
  n_prov_destiny = NULL,
  variation = 0.2,
  baseline = NULL
) {
  thresholds <- .typology_thresholds()
  baseline <- baseline %||%
    create_typologies_spain(
      n_prov_destiny = n_prov_destiny,
      make_map = FALSE
    )

  grid <- .oat_threshold_grid(thresholds, variation)

  grid |>
    purrr::pmap(function(threshold, direction, th) {
      result <- .classify_typology_base(baseline, th)
      .compute_agreement(baseline, result, threshold, direction)
    }) |>
    purrr::list_rbind()
}

# --- Private helpers ---------------------------------------------------------

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
