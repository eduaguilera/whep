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
