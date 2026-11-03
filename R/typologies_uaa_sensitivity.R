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
#' These are sensitivity scenarios, not reproductions of the complete
#' published decision trees: the other WHEP rules stay fixed. Threshold
#' sources are Le Noe et al. (2017), section 3.2.2, p.49,
#' \doi{10.1016/j.scitotenv.2017.02.040}, and Garnier et al.
#' (2023), Figure 2a, \doi{10.1016/j.jenvman.2023.117732}.
#' Both use agricultural area; neither validates the empirically selected
#' WHEP whole-province thresholds. The studies share authors and should
#' not be counted as independent validation of the WHEP calibration.
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
#' Inputs must resolve to dated polity identities. Duplicate aliases within a
#' source polity-year, missing/non-finite UAA, zero UAA and unresolved
#' territories abort with diagnostic rows; they never become classifications.
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
#'   2000, "Huesca", 1, 100, 5, 0.8, 40, 500,
#'   0.1, 0.1, 0.1,
#'   0.1, 0.1, "Specialized cropping systems (intensive)",
#'   2000, "Zaragoza", 1, 10, 50, 0.1, 40, 6000,
#'   0.4, 0.8, 0.1,
#'   0.1, 0.1, "Specialized livestock systems (extensive)"
#' )
#' npp_ygpit <- tibble::tribble(
#'   ~Year, ~Province_name, ~LandUse, ~Area_ygpit_ha,
#'   2000, "Huesca", "Cropland", 8000,
#'   2000, "Huesca", "Forest_high", 2000,
#'   2000, "Zaragoza", "Cropland", 4000,
#'   2000, "Zaragoza", "Pasture_Shrubland", 2000,
#'   2000, "Zaragoza", "Forest_high", 9000
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
  baseline <- .uaa_key_panel(dplyr::ungroup(baseline))
  npp_ygpit <- npp_ygpit %||% whep_read_file("npp_ygpit")

  baseline_uaa <- .attach_uaa_density(baseline, npp_ygpit)

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
#' @return A tibble with `Year`, `reporting_polity_code`, `Area_ha_uaa`.
#' @keywords internal
#' @noRd
.calculate_uaa_area <- function(npp_ygpit) {
  .uaa_require_columns(
    npp_ygpit,
    c("Year", "Province_name", "LandUse", "Area_ygpit_ha")
  )
  if (anyNA(npp_ygpit$LandUse) || any(!nzchar(npp_ygpit$LandUse))) {
    cli::cli_abort(
      "UAA source rows require explicit land-use categories.",
      class = "whep_uaa_missing_support"
    )
  }
  rows <- npp_ygpit |>
    dplyr::filter(LandUse %in% c("Cropland", "Pasture_Shrubland", "Dehesa")) |>
    .uaa_polity_rows() |>
    dplyr::rename(year = Year)
  if (
    !is.numeric(rows$Area_ygpit_ha) ||
      any(!is.finite(rows$Area_ygpit_ha) | rows$Area_ygpit_ha < 0)
  ) {
    cli::cli_abort(
      "UAA requires finite, non-negative supplied area values.",
      class = "whep_uaa_missing_support"
    )
  }
  aliases <- rows |>
    dplyr::summarise(
      uaa_source_labels = dplyr::n_distinct(Province_name),
      .by = c(year, reporting_polity_code)
    ) |>
    dplyr::filter(uaa_source_labels > 1L)
  if (nrow(aliases) > 0L) {
    cli::cli_abort(
      "Multiple source aliases supply the same polity-year UAA.",
      class = "whep_uaa_duplicate_identity",
      unresolved = aliases
    )
  }
  rows |>
    dplyr::summarise(
      Area_ha_uaa = sum(Area_ygpit_ha),
      .by = c(year, reporting_polity_code)
    ) |>
    dplyr::rename(Year = year)
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

  baseline <- .uaa_key_panel(baseline)
  result <- .uaa_key_panel(result)
  key <- c("year", "reporting_polity_code")
  missing <- dplyr::anti_join(baseline, result, by = key)
  extra <- dplyr::anti_join(result, baseline, by = key)
  if (
    nrow(missing) > 0L ||
      nrow(extra) > 0L ||
      anyNA(baseline$Typology_base) ||
      anyNA(result$Typology_base)
  ) {
    cli::cli_abort(
      "Agreement requires complete classifications on identical polity-years.",
      class = "whep_uaa_missing_support",
      unresolved = missing
    )
  }
  baseline |>
    dplyr::select(year, reporting_polity_code, Typology_base) |>
    dplyr::left_join(
      dplyr::select(result, year, reporting_polity_code, Typology_base),
      by = key,
      relationship = "one-to-one",
      suffix = c("_base", "_uaa")
    ) |>
    dplyr::summarise(
      agreement_pct = mean(Typology_base_base == Typology_base_uaa) * 100,
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


.uaa_require_columns <- function(rows, columns) {
  if (!all(rlang::has_name(rows, columns))) {
    cli::cli_abort(
      "UAA input requires columns {.val {columns}}.",
      class = "whep_uaa_missing_support"
    )
  }
}

# Source names are resolved only at ingestion. No unknown label is used as a
# substitute identity: the pre-1927 Canary source territories remain subject
# to whep-polities#741. Expose the unresolved observations instead of silently
# classifying a missing denominator or matching historical names.
.uaa_polity_rows <- function(rows) {
  .uaa_require_columns(rows, c("Year", "Province_name"))
  supplied <- if (rlang::has_name(rows, "reporting_polity_code")) {
    rows$reporting_polity_code
  }
  rows <- .grafs_province_keys(rows)
  invalid <- is.na(rows$reporting_polity_code)
  if (!is.null(supplied)) {
    invalid <- invalid |
      is.na(supplied) |
      supplied != rows$reporting_polity_code
  }
  unresolved <- rows[invalid, ]
  if (nrow(unresolved) > 0L) {
    if (!is.null(supplied)) {
      unresolved$supplied_polity_code <- supplied[invalid]
    }
    cli::cli_abort(
      "UAA sensitivity requires consistent, resolved dated polity identities.",
      class = "whep_uaa_unresolved_identity",
      unresolved = unresolved
    )
  }
  rows |> dplyr::select(-grafs_geography_key)
}

.uaa_key_panel <- function(rows) {
  .uaa_require_columns(rows, c("year", "province_name", "Typology_base"))
  if (
    nrow(rows) == 0L ||
      anyNA(rows$Typology_base) ||
      any(!nzchar(as.character(rows$Typology_base)))
  ) {
    cli::cli_abort(
      "Typology sensitivity requires a non-empty classified panel.",
      class = "whep_uaa_missing_support"
    )
  }
  rows <- rows |>
    dplyr::rename(Year = year, Province_name = province_name) |>
    .uaa_polity_rows() |>
    dplyr::rename(year = Year, province_name = Province_name)
  duplicates <- rows |>
    dplyr::count(year, reporting_polity_code) |>
    dplyr::filter(n > 1L)
  if (nrow(duplicates) > 0L) {
    cli::cli_abort(
      "Typology panel has repeated dated polity identities.",
      class = "whep_uaa_duplicate_identity",
      unresolved = duplicates
    )
  }
  rows
}

.attach_uaa_density <- function(baseline, npp_ygpit) {
  .uaa_require_columns(baseline, c("LU_total", "Livestock_density"))
  if (
    !is.numeric(baseline$LU_total) ||
      any(!is.finite(baseline$LU_total) | baseline$LU_total < 0) ||
      !is.numeric(baseline$Livestock_density) ||
      any(
        !is.finite(baseline$Livestock_density) |
          baseline$Livestock_density < 0
      )
  ) {
    cli::cli_abort(
      "Sensitivity requires finite, non-negative livestock indicators.",
      class = "whep_uaa_missing_support"
    )
  }
  .uaa_require_columns(npp_ygpit, "Year")
  if (
    !is.numeric(npp_ygpit$Year) ||
      any(!is.finite(npp_ygpit$Year)) ||
      any(npp_ygpit$Year != floor(npp_ygpit$Year))
  ) {
    cli::cli_abort(
      "UAA source years must be supplied finite integers.",
      class = "whep_grafs_geography"
    )
  }
  area <- npp_ygpit |>
    dplyr::filter(Year %in% baseline$year) |>
    .calculate_uaa_area()
  result <- baseline |>
    dplyr::left_join(
      area,
      by = c("year" = "Year", "reporting_polity_code"),
      relationship = "one-to-one"
    )
  unresolved <- result |>
    dplyr::filter(!is.finite(Area_ha_uaa) | Area_ha_uaa <= 0)
  if (nrow(unresolved) > 0L) {
    cli::cli_abort(
      "Every classified polity-year requires supplied positive UAA.",
      class = "whep_uaa_missing_support",
      unresolved = unresolved
    )
  }
  result |> dplyr::mutate(Livestock_density = LU_total / Area_ha_uaa)
}
