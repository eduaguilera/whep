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
