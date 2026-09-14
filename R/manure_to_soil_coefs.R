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
#' `-> product_n_kgdm` (from `bio_coefs`), so feed-item intake can be converted
#' to a nitrogen intake. Grass/substitute intake rows carry `item_cbs_code = NA`
#' and have no item key; they take the forage default from
#' `.forage_n_kgn_kgdm()` instead.
#' @noRd
.feed_n_content_lookup <- function(
  items = whep::items_full,
  coefs = whep::whep_coef_table("bio_coefs")
) {
  coef_n <- tibble::as_tibble(coefs) |>
    dplyr::transmute(
      name_biomass = .data$name_biomass,
      feed_n_kgn_kgdm = .data$product_n_kgdm
    ) |>
    dplyr::distinct(.data$name_biomass, .keep_all = TRUE)

  tibble::as_tibble(items) |>
    dplyr::transmute(
      item_cbs_code = as.integer(.data$item_cbs_code),
      name_biomass = .data$Name_biomass
    ) |>
    dplyr::filter(!is.na(.data$item_cbs_code)) |>
    dplyr::distinct(.data$item_cbs_code, .keep_all = TRUE) |>
    dplyr::left_join(coef_n, by = "name_biomass")
}

#' Manure C:N and humification per species x manure type.
#'
#' The `bio_coefs` table carries manure coefficients in its `Solid`, `Liquid`,
#' `Excreta` and `Urban` `category` rows (keyed by `name_biomass` = species),
#' with `residue_c_n_ratio` as the manure C:N and `residue_humified_c_kgc` as the
#' humification fraction. Used to derive manure carbon from manure nitrogen.
#' @noRd
.manure_cn_coefs <- function(coefs = whep::whep_coef_table("bio_coefs")) {
  tibble::as_tibble(coefs) |>
    dplyr::filter(
      .data$category %in% c("Solid", "Liquid", "Excreta", "Urban")
    ) |>
    dplyr::transmute(
      species = .data$name_biomass,
      manure_type = .data$category,
      cn_ratio = .data$residue_c_n_ratio,
      humified_c_kgc = .data$residue_humified_c_kgc
    )
}

#' Nitrogen content of grazed forage (kg N / kg DM).
#'
#' Applied to the grass/substitute intake rows that the allocator emits with
#' `item_cbs_code = NA` (the unlimited grassland sink), which therefore carry no
#' feed-item N key. Selectable, because the published grazed-forage values span
#' 0.017-0.022 kg N / kg DM and the choice moves excreted nitrogen roughly in
#' proportion to the grazed share of intake:
#'
#' * `"assumed_midrange"` (default) -- 0.02 kg N / kg DM (12.5% crude protein).
#'   **Assumed, unverified**: it is the value the package has always used and no
#'   source states it. It lies between the two GLEAM grass values below, which
#'   is why it is kept as the default rather than quietly replaced.
#' * `"gleam_grass_fresh"` / `"gleam_grass_hay"` / `"gleam_grass_mean"` --
#'   GLEAM `GRASSF` (22 g N / kg DM), `GRASSH` (17 g N / kg DM) and their mean,
#'   read from [gleam_feed_digestibility] (FAO GLEAM 3.0 Supplement S1,
#'   Tab. S.3.3, "Nutritional values for feed materials of ruminant species").
#' * `"biomass_coefs_grass"` -- `product_n_kgdm` of the `Grass` rows of the
#'   `bio_coefs` coefficient table (0.0174), i.e. the same N content WHEP
#'   already applies to grass arriving as a CBS feed item.
#'
#' None of these closes the gap in whep#1050. The grazed sink carries 18% of
#' global intake dry matter (1.30 of 7.15 Pg at 2020), so the whole range spans
#' 86.1 to 92.0 Tg of total excreted nitrogen against 89.6 Tg on the default;
#' matching FAOSTAT's sheep and goats would need 0.116 kg N / kg DM, a 73%
#' crude-protein forage that does not exist.
#' @noRd
.forage_n_kgn_kgdm <- function(method = "assumed_midrange") {
  method <- rlang::arg_match(method, .forage_n_methods())
  switch(
    method,
    assumed_midrange = 0.02,
    biomass_coefs_grass = .bio_coefs_grass_n(),
    .gleam_grass_n(method)
  )
}

.forage_n_methods <- function() {
  c(
    "assumed_midrange",
    "gleam_grass_fresh",
    "gleam_grass_hay",
    "gleam_grass_mean",
    "biomass_coefs_grass"
  )
}

# GLEAM 3.0 Supplement S1 Tab. S.3.3 grass N content, read from the shipped
# table rather than transcribed, so the coefficient stays traceable to its data.
.gleam_grass_n <- function(method) {
  tbl <- tibble::as_tibble(whep::gleam_feed_digestibility)
  vals <- purrr::map_dbl(c("GRASSF", "GRASSH"), function(mat) {
    v <- unique(tbl$n_content_g_kg[tbl$material == mat])
    if (length(v) != 1 || is.na(v)) {
      cli::cli_abort(
        "No single GLEAM nitrogen content for feed material {.val {mat}}."
      )
    }
    v / 1000
  })
  switch(
    method,
    gleam_grass_fresh = vals[[1]],
    gleam_grass_hay = vals[[2]],
    gleam_grass_mean = mean(vals)
  )
}

# The `Grass` rows of bio_coefs (item_prod_code 996 / 3001 / 3002) all carry the
# same product_n_kgdm; abort rather than silently pick one if they diverge.
.bio_coefs_grass_n <- function() {
  v <- whep::whep_coef_table("bio_coefs") |>
    dplyr::filter(.data$name_biomass == "Grass") |>
    dplyr::pull("product_n_kgdm") |>
    unique()
  if (length(v) != 1 || is.na(v)) {
    cli::cli_abort(
      "Expected one {.field product_n_kgdm} for {.val Grass} in bio_coefs."
    )
  }
  v
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
