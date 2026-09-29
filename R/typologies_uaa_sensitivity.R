#' Reclassify typologies with Julia's and Josette's thresholds, on both area
#' bases
#'
#' @description Julia's and Josette's own typology methods
#'   (`Typologies_Julia.R`, `Typologies_Josette.R`) use published
#'   livestock-density thresholds of 0.5 and 1 LU/ha respectively, against
#'   utilised agricultural area (UAA: cropland, permanent pasture/shrubland
#'   and dehesa). `create_typologies_spain()` instead divides by the whole
#'   province area, with its own empirically-fit thresholds (0.25/0.3) --
#'   the two are not directly comparable as published.
#'
#'   This reclassifies the baseline typology with each literature threshold
#'   in turn (applied uniformly to `livestock_density_int`/`ext_lo`/
#'   `ext_hi`, since neither Julia's nor Josette's method draws Alice's
#'   intensive/extensive distinction within "specialized livestock"), on
#'   **both** area bases -- the whole-province `Livestock_density`
#'   `create_typologies_spain()` already computes, and a UAA-recomputed one
#'   -- so the two effects can be told apart: applying a literature
#'   threshold to the *wrong* (whole-province) denominator on its own is not
#'   the same test as applying it together with the UAA basis it was
#'   published against. Every other threshold (crop productivity, synthetic
#'   share, connected/disconnected) stays at its `.typology_thresholds()`
#'   value throughout, so only the livestock-density basis varies.
#'
#' @param n_prov_destiny Nitrogen flows tibble, passed to
#'   `create_typologies_spain()` when `baseline` is `NULL`. If `NULL`,
#'   loaded automatically (slow).
#' @param npp_ygpit Land-use area tibble with `Year`, `Province_name`,
#'   `LandUse` and `Area_ygpit_ha`. If `NULL`, read from the `npp_ygpit` pin.
#' @param baseline Pre-computed indicator table from
#'   `create_typologies_spain()`, carrying `year`, `province_name`,
#'   `LU_total`, `Typology_base` and the other indicator columns
#'   `.classify_typology_base()` acts on. If `NULL`, computed automatically
#'   (slow).
#'
#' @return A tibble with columns `source` (`"Julia"`/`"Josette"`),
#'   `uaa_threshold`, `area_basis` (`"whole_province"`/`"uaa"`),
#'   `agreement_pct` (share of province-years keeping the baseline's
#'   `Typology_base`), and `n_specialized_livestock` (province-years
#'   classified as either specialized-livestock category under that
#'   source/basis combination).
#' @export
#'
#' @examples
#' # A tiny fixture: two provinces/years, one clearly cropping-specialized
#' # (low density either way) and one where the UAA/whole-province area
#' # difference changes which side of a literature threshold it lands on.
#' baseline <- tibble::tribble(
#'   ~year, ~province_name, ~production_seminatural, ~production_crops,
#'   ~animal_ingestion, ~synthetic_share, ~crop_productivity, ~LU_total,
#'   ~Livestock_density, ~imported_feed_share, ~feed_from_seminatural_share,
#'   ~local_feed_share, ~Manure_share, ~Typology_base,
#'   2000, "A", 1, 100, 5, 0.8, 40, 500,
#'   0.1, 0.1, 0.1,
#'   0.1, 0.1, "Specialized cropping systems (intensive)",
#'   2000, "B", 1, 10, 50, 0.1, 40, 6000,
#'   0.4, 0.8, 0.1,
#'   0.1, 0.1, "Specialized livestock systems (extensive)"
#' )
#' npp_ygpit <- tibble::tribble(
#'   ~Year, ~Province_name, ~LandUse, ~Area_ygpit_ha,
#'   2000, "A", "Cropland", 8000,
#'   2000, "A", "Forest_high", 2000,
#'   2000, "B", "Cropland", 4000,
#'   2000, "B", "Pasture_Shrubland", 2000,
#'   2000, "B", "Forest_high", 9000
#' )
#' sensitivity <- run_typology_area_sensitivity(
#'   npp_ygpit = npp_ygpit,
#'   baseline = baseline
#' )
run_typology_area_sensitivity <- function(
  n_prov_destiny = NULL,
  npp_ygpit = NULL,
  baseline = NULL
) {
  baseline <- baseline %||%
    create_typologies_spain(
      n_prov_destiny = n_prov_destiny,
      make_map = FALSE
    )
  baseline <- dplyr::ungroup(baseline)
  npp_ygpit <- npp_ygpit %||% whep_read_file("npp_ygpit")

  baseline_uaa <- baseline |>
    dplyr::left_join(
      .calculate_uaa_area(npp_ygpit),
      by = c("year" = "Year", "province_name" = "Province_name")
    ) |>
    dplyr::mutate(
      Livestock_density = dplyr::if_else(
        is.na(Area_ha_uaa) | Area_ha_uaa == 0,
        NA_real_,
        LU_total / Area_ha_uaa
      )
    )

  area_bases <- list(whole_province = baseline, uaa = baseline_uaa)

  tidyr::expand_grid(
    .literature_livestock_sources(),
    area_basis = names(area_bases)
  ) |>
    purrr::pmap(function(source, uaa_threshold, area_basis) {
      th <- utils::modifyList(
        .typology_thresholds(),
        list(
          livestock_density_int = uaa_threshold,
          livestock_density_ext_lo = uaa_threshold,
          livestock_density_ext_hi = uaa_threshold
        )
      )
      result <- .classify_typology_base(area_bases[[area_basis]], th)
      .compute_uaa_agreement(
        baseline,
        result,
        source,
        uaa_threshold,
        area_basis
      )
    }) |>
    purrr::list_rbind()
}

# --- Private helpers ---------------------------------------------------------

#' @title Literature livestock-density thresholds to test ---------------------
#' @description Julia's and Josette's published UAA-based livestock-density
#' thresholds (see `run_typology_area_sensitivity()`'s description for why
#' these particular values, not the 0.4 `Typologies_Julia.R` itself codes).
#'
#' @return A tibble with `source` and `uaa_threshold`.
#' @keywords internal
#' @noRd
.literature_livestock_sources <- function() {
  tibble::tribble(
    ~source, ~uaa_threshold,
    "Julia", 0.5,
    "Josette", 1
  )
}

#' @title UAA (utilised agricultural area) per province-year ------------------
#' @description Cropland plus permanent pasture/shrubland plus dehesa,
#' matching the agricultural-area filter already used for kgN/ha indicators
#' (`typologies_kgN_ha.R`) -- unlike `create_typologies_spain()`'s own
#' `Area_ha`, which sums every `LandUse` including forest.
#'
#' @param npp_ygpit Output of `whep_read_file("npp_ygpit")`.
#'
#' @return A tibble with `Year`, `Province_name`, `Area_ha_uaa`.
#' @keywords internal
#' @noRd
.calculate_uaa_area <- function(npp_ygpit) {
  npp_ygpit |>
    dplyr::filter(LandUse %in% c("Cropland", "Pasture_Shrubland", "Dehesa")) |>
    dplyr::group_by(Year, Province_name) |>
    dplyr::summarise(
      Area_ha_uaa = sum(Area_ygpit_ha, na.rm = TRUE),
      .groups = "drop"
    )
}

#' @title Agreement and specialized-livestock count for one source/basis -----
#' @description Compares one literature-threshold reclassification (on
#' either area basis) against the baseline `Typology_base`, alongside how
#' many province-years land in either specialized-livestock category under
#' the tested combination -- the count `run_typology_sensitivity()` doesn't
#' need, since this comparison is specifically about that category shifting.
#'
#' @param baseline Original `create_typologies_spain()` output.
#' @param result `baseline` reclassified with the tested threshold, on
#' either area basis.
#' @param source `"Julia"` or `"Josette"`.
#' @param uaa_threshold The literature threshold tested.
#' @param area_basis `"whole_province"` or `"uaa"`.
#'
#' @return A one-row tibble with `source`, `uaa_threshold`, `area_basis`,
#' `agreement_pct` and `n_specialized_livestock`.
#' @keywords internal
#' @noRd
.compute_uaa_agreement <- function(
  baseline,
  result,
  source,
  uaa_threshold,
  area_basis
) {
  specialized <- c(
    "Specialized livestock systems (intensive)",
    "Specialized livestock systems (extensive)"
  )

  baseline |>
    dplyr::select(year, province_name, Typology_base) |>
    dplyr::left_join(
      dplyr::select(result, year, province_name, Typology_base),
      by = c("year", "province_name"),
      suffix = c("_base", "_uaa")
    ) |>
    dplyr::summarise(
      agreement_pct = mean(
        Typology_base_base == Typology_base_uaa,
        na.rm = TRUE
      ) *
        100,
      n_specialized_livestock = sum(Typology_base_uaa %in% specialized)
    ) |>
    dplyr::mutate(
      source = source,
      uaa_threshold = uaa_threshold,
      area_basis = area_basis
    ) |>
    dplyr::select(
      source,
      uaa_threshold,
      area_basis,
      agreement_pct,
      n_specialized_livestock
    )
}
