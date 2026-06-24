# Coefficient-scaffolding loaders for the livestock excretion -> manure-to-soil
# engine (modeled-v1). Each table maps the redistribute_feed `livestock_category`
# grain onto the IPCC/EMEP coefficient taxonomies so every downstream join is
# explicit (no silent default), and carries a `reliability` flag plus `notes`
# documenting any lumping or proxy assumption.

#' Load the species taxonomy bridge.
#'
#' Maps each `livestock_category` (the `redistribute_feed()` grain) onto the
#' coefficient taxonomies: `species_gen` (ash and MMS-distribution key),
#' `subcategory`, `bo_category` (N-retention / methane-potential key),
#' `excretion_category` (IPCC default-Nex key) and the gridding `species_group`.
#' Resolves the known mismatch where the string-matching `.get_bo_category()`
#' emits `"Swine - Market"`/`"Poultry - Broilers"` while the retention table is
#' keyed by `"Swine"`/`"Poultry"` (which silently coalesced to 0.07). Lumping and
#' proxy choices are flagged in `reliability` (`verified`/`derived`/`placeholder`)
#' and `notes`.
#' @noRd
.species_taxonomy_bridge <- function() {
  system.file(
    "extdata",
    "feed",
    "species_taxonomy_bridge.csv",
    package = "whep"
  ) |>
    data.table::fread(na.strings = "") |>
    tibble::as_tibble()
}

#' Feed nitrogen content per CBS feed item (kg N / kg DM).
#'
#' Two-hop crosswalk `item_cbs_code -> Name_biomass` (from `items_full`)
#' `-> Product_kgN_kgDM` (from `biomass_coefs`), so feed-item intake can be
#' converted to a nitrogen intake. Grass/substitute intake rows carry
#' `item_cbs_code = NA` and have no item key; they take the forage default from
#' `.forage_n_kgn_kgdm()` instead.
#' @noRd
.feed_n_content_lookup <- function(items, coefs) {
  coef_n <- tibble::as_tibble(coefs) |>
    dplyr::transmute(
      Name_biomass = .data$Name_biomass,
      feed_n_kgn_kgdm = .data$Product_kgN_kgDM
    ) |>
    dplyr::distinct(.data$Name_biomass, .keep_all = TRUE)

  tibble::as_tibble(items) |>
    dplyr::transmute(
      item_cbs_code = as.integer(.data$item_cbs_code),
      Name_biomass = .data$Name_biomass
    ) |>
    dplyr::filter(!is.na(.data$item_cbs_code)) |>
    dplyr::distinct(.data$item_cbs_code, .keep_all = TRUE) |>
    dplyr::left_join(coef_n, by = "Name_biomass")
}

#' Default nitrogen content of grazed forage (kg N / kg DM).
#'
#' Applied to grass/substitute intake rows (`item_cbs_code = NA`) that have no
#' feed-item N key. 0.02 kg N / kg DM (~12.5% crude protein) is a mid-range
#' grazed-forage value; provisional (CALIBRATE) pending a sourced per-feed-group
#' forage N table.
#' @noRd
.forage_n_kgn_kgdm <- function() {
  0.02
}

#' Manure-management nitrogen-loss fractions per (MMS, animal category).
#'
#' `frac_gas_ms` (NH3 + NOx volatilized during housing/storage) and
#' `frac_leach_ms` (leached/runoff) from IPCC 2019 Refinement Vol.4 Ch.10
#' Table 10.22 (base variants), keyed by `mms_type` (the 6
#' `regional_mms_distribution` systems) and the 5 IPCC animal categories
#' (`Dairy Cattle`, `Other Cattle`, `Swine`, `Poultry`, `Other animals`). These
#' fractions are not climate-dependent. Pasture/Range/Paddock storage loss is 0
#' (its losses enter the soil-deposition pathway, not management).
#' @noRd
.manure_loss_fractions <- function() {
  system.file(
    "extdata",
    "manure",
    "manure_loss_fractions.csv",
    package = "whep"
  ) |>
    data.table::fread(na.strings = c("NA", "")) |>
    tibble::as_tibble()
}

#' Ratio of dinitrogen (N2) to nitrous-oxide (N2O) N lost in manure storage.
#'
#' N2-N = ratio x N2O-N (from EF3). Default 3 (plausible range 1-10) from
#' IPCC 2019 Refinement Vol.4 Ch.10 Table 10.23 (Webb & Misselbrook 2004).
#' @noRd
.n2_to_n2o_ratio <- function() {
  3
}

#' IPCC manure-MCF climate zone from mean annual temperature (deg C).
#'
#' Returns one of `"Cool"`, `"Temperate"`, `"Warm"` (the `climate_mcf`
#' super-zones) using the verified 2006 IPCC GL Vol.4 Ch.3 decision-tree cuts
#' (adopted by the 2019 Refinement Ch.10 Fig 10A.1): Cool <= 10 deg C,
#' Temperate 10-18 deg C, Warm > 18 deg C. (The widely-guessed 15/25 cuts are
#' not in any IPCC source.) `NA` MAT returns `NA`.
#' @noRd
.climate_zone_from_mat <- function(mat_c) {
  dplyr::case_when(
    is.na(mat_c) ~ NA_character_,
    mat_c <= 10 ~ "Cool",
    mat_c <= 18 ~ "Temperate",
    TRUE ~ "Warm"
  )
}

#' Source registry for the manure-to-soil coefficient scaffolding.
#'
#' One row per documented source behind the scaffolded coefficient tables, with
#' a `reliability` flag in {verified, derived, consensus, placeholder}. The
#' modeled-v1 provenance backbone (afse data-validation framework).
#' @noRd
.manure_to_soil_sources <- function() {
  system.file(
    "extdata",
    "manure",
    "manure_to_soil_sources.csv",
    package = "whep"
  ) |>
    data.table::fread(na.strings = c("NA", "")) |>
    tibble::as_tibble()
}
