# redistribute_feed-based feed-intake path (migration in progress).
#
# Plan: plans/2026-06-19-wire-redistribute-feed-migration. Three engines:
#   Engine 1 DEMAND   = a 3-method hybrid: IPCC Tier-2 energy for the 4 ruminant
#                       species it covers (Cattle, Buffalo, Sheep, Goats),
#                       Bouwman FCR for pigs/poultry (no Tier-2 energy model
#                       exists for monogastrics), Krausmann per-head for draft /
#                       other species.
#   Engine 2 MIX      = Bouwman dm_share (within-species feed_type split).
#   Engine 3 ALLOCATE = redistribute_feed() at 0.5-degree cell grain
#                       (territory = country, sub_territory = cell), with the
#                       grass ceiling/deficit cascade + the distance-decay
#                       feed-access buffer on the roughage trade.
#
# Built incrementally; until the engines land this dispatcher errors so the
# package default (grain = "national", demand_tier = "fcr") keeps working.

.build_redistribute_intake <- function(grain, demand_tier) {
  cli::cli_abort(c(
    "The {.val redistribute} feed-intake path is not yet implemented.",
    i = "Migration in progress: grain = {.val {grain}}, demand_tier =
      {.val {demand_tier}}.",
    i = "Use the default {.code grain = \"national\", demand_tier = \"fcr\"}
      for the current allocator."
  ))
}

# Curated live_anim_code -> livestock_category crosswalk: maps each live animal
# (animals_codes.csv) to its feed livestock_category (max_intake_share.csv) and
# marks which demand method it uses (`demand_source`: "ipcc" Tier-2 energy for
# the 4 ruminant species, "fcr" Bouwman for pigs/poultry, "krausmann" per-head
# for draft / other). `energy_species` is INFORMATIONAL ONLY: the energy model
# resolves the species itself from animals_codes via .get_general_species, so
# this column documents (it does not drive) which energy species each IPCC row
# uses. Built + adversarially verified against the three source code systems.
.livestock_crosswalk <- function() {
  system.file(
    "extdata",
    "feed",
    "livestock_category_crosswalk.csv",
    package = "whep"
  ) |>
    data.table::fread(na.strings = "") |>
    tibble::as_tibble()
}

# ---- Engine 1: DEMAND -------------------------------------------------------

# Total dry-matter feed demand per (year, area_code, livestock_category). Each
# live animal's demand uses the method the crosswalk assigns it; methods are
# alternatives, never silent fallbacks, and the chosen one is recorded in
# `method_demand`:
#   * demand_tier = "ipcc": IPCC Tier-2 energy (gross_energy -> DM) for the
#     ruminant species (the only ones IPCC's energy model covers); Bouwman FCR
#     for pigs and poultry; Krausmann per-head for draft / other species.
#   * demand_tier = "fcr": the legacy Bouwman / Krausmann magnitude for every
#     species (the regression baseline that reproduces today's output).
.build_feed_demand_total <- function(
  production,
  demand_tier,
  data = .feed_demand_data()
) {
  prod <- .normalise_feed_primary(production)
  if (nrow(prod) == 0) {
    return(.empty_feed_demand_total())
  }
  fcr <- .build_bouwman_fcr(data$conv_bouwman, sort(unique(prod$year)))
  totals <- .build_feed_demand(
    prod,
    data$items_prod_full,
    data$animals_codes,
    data$conv_krausmann,
    data$polities_cats,
    fcr
  ) |>
    dplyr::summarise(
      demand_dm_t = sum(demand_aft, na.rm = TRUE),
      .by = c(year, area_code, live_anim_code)
    ) |>
    .tag_legacy_method(data$crosswalk)

  if (demand_tier == "ipcc") {
    totals <- .overlay_energy_demand(totals, production, data$crosswalk)
  }
  .aggregate_demand_to_category(totals, data$crosswalk)
}

# Tag every legacy (`.build_feed_demand`) row with the method that actually
# produced it: that path is always the Bouwman / Krausmann magnitude, so the
# label is the "fcr"-tier label (`bouwman_fcr` or `krausmann_per_head`). The
# ipcc tier later overwrites the covered ruminant rows with energy demand and
# re-tags them; any uncovered ruminant keeps this FCR-fallback label, so the
# recorded method always reflects the number actually used.
.tag_legacy_method <- function(totals, crosswalk) {
  src <- crosswalk |>
    dplyr::transmute(
      live_anim_code = as.integer(live_anim_code),
      demand_source
    )
  totals |>
    dplyr::mutate(live_anim_code = as.integer(live_anim_code)) |>
    dplyr::inner_join(src, by = "live_anim_code") |>
    dplyr::transmute(
      year,
      area_code,
      live_anim_code,
      demand_dm_t,
      method_demand = .demand_method_label(demand_source, "fcr")
    )
}

# Package datasets the demand engine needs, grouped so the signature stays
# small and tests can inject fixtures.
.feed_demand_data <- function() {
  list(
    items_prod_full = whep::items_prod_full,
    animals_codes = whep::animals_codes,
    conv_krausmann = whep::conv_krausmann,
    conv_bouwman = whep::conv_bouwman,
    polities_cats = whep::polities_cats,
    crosswalk = .livestock_crosswalk()
  )
}

# Replace the ruminant codes' legacy FCR totals with IPCC Tier-2 energy totals,
# re-tagging those rows as energy-derived. Ruminant codes the energy model could
# not cover (e.g. a polity with ruminant product but no heads row) keep their
# legacy FCR total and its `bouwman_fcr` label, so their demand is never
# silently dropped and the recorded method stays honest.
.overlay_energy_demand <- function(totals, production, crosswalk) {
  ruminant_codes <- crosswalk$live_anim_code[crosswalk$demand_source == "ipcc"]
  energy <- .build_demand_energy(production, ruminant_codes) |>
    dplyr::mutate(method_demand = "ipcc_tier2_energy")
  covered <- intersect(ruminant_codes, unique(energy$live_anim_code))
  uncovered <- setdiff(
    intersect(ruminant_codes, totals$live_anim_code),
    covered
  )
  if (length(uncovered) > 0) {
    n <- length(uncovered)
    cli::cli_warn(c(
      "IPCC energy demand could not be computed for {n} ruminant
       live_anim_code{?s}: {.val {uncovered}}.",
      i = "Keeping the Bouwman FCR magnitude for {cli::qty(n)}{?it/them}."
    ))
  }
  totals |>
    dplyr::filter(!live_anim_code %in% covered) |>
    dplyr::bind_rows(
      dplyr::select(
        energy,
        year,
        area_code,
        live_anim_code,
        demand_dm_t,
        method_demand
      )
    )
}

# IPCC Tier-2 energy demand for ruminant species: gross_energy (MJ/day/head)
# converted to dry-matter tonnes/year via the diet gross-energy content and a
# 365-day year, summed over the GLEAM cohorts of each animal.
.build_demand_energy <- function(production, ruminant_codes) {
  heads <- production |>
    dplyr::filter(unit == "heads", item_cbs_code %in% ruminant_codes)
  if (nrow(heads) == 0) {
    return(.empty_demand_codes())
  }
  production |>
    dplyr::filter(
      (unit == "heads" & item_cbs_code %in% ruminant_codes) | unit == "t_head"
    ) |>
    .ensure_diet_quality() |>
    prepare_livestock_emissions(expand_cohorts = TRUE) |>
    estimate_energy_demand() |>
    .energy_to_dm()
}

# Demand assumes a default IPCC "Medium" diet (DE 65%) where diet quality is not
# supplied; this documented assumption feeds both DE% and the gross-energy
# density used in the dry-matter conversion.
.ensure_diet_quality <- function(data) {
  if (!rlang::has_name(data, "diet_quality")) {
    data$diet_quality <- "Medium"
  } else {
    data$diet_quality <- dplyr::coalesce(data$diet_quality, "Medium")
  }
  data
}

# gross_energy (MJ/day/head) -> dry-matter tonnes/year, summed over cohorts.
.energy_to_dm <- function(energy) {
  ge_content <- feed_characteristics |>
    dplyr::select(diet_quality, ge_content_mj_kg_dm) |>
    dplyr::distinct()
  days <- livestock_constants$days_in_year
  out <- energy |>
    dplyr::left_join(ge_content, by = "diet_quality") |>
    dplyr::mutate(
      dm_t = cohort_heads * (gross_energy / ge_content_mj_kg_dm) * days / 1000
    )
  if (any(is.na(out$dm_t) & out$cohort_heads > 0)) {
    cli::cli_warn(
      "Some ruminant cohorts have undefined dry-matter demand and are excluded:
       missing gross energy (check GLEAM cohort weights) or an unrecognised
       diet_quality with no gross-energy density."
    )
  }
  out |>
    dplyr::summarise(
      demand_dm_t = sum(dm_t, na.rm = TRUE),
      .by = c(year, area_code, item_cbs_code)
    ) |>
    dplyr::transmute(
      year,
      area_code = as.integer(area_code),
      live_anim_code = as.integer(item_cbs_code),
      demand_dm_t
    )
}

# Aggregate per-animal totals (already carrying the method that produced each
# row) to livestock_category, recording the method(s) the category's demand was
# actually built from.
.aggregate_demand_to_category <- function(totals, crosswalk) {
  cat_map <- crosswalk |>
    dplyr::transmute(
      live_anim_code = as.integer(live_anim_code),
      livestock_category
    )
  totals |>
    dplyr::mutate(live_anim_code = as.integer(live_anim_code)) |>
    dplyr::inner_join(cat_map, by = "live_anim_code") |>
    dplyr::summarise(
      demand_dm_t = sum(demand_dm_t, na.rm = TRUE),
      method_demand = .combine_demand_methods(method_demand),
      .by = c(year, area_code, livestock_category)
    )
}

# Method label per (demand_source, demand_tier): energy only for ruminants under
# the ipcc tier; Krausmann for draft; Bouwman FCR otherwise.
.demand_method_label <- function(demand_source, demand_tier) {
  dplyr::case_when(
    demand_source == "ipcc" & demand_tier == "ipcc" ~ "ipcc_tier2_energy",
    demand_source == "krausmann" ~ "krausmann_per_head",
    TRUE ~ "bouwman_fcr"
  )
}

# The method(s) a category's demand was built from. A single method for pure
# categories; a "+"-joined set for a mixed category (e.g. Cattle_meat when one
# ruminant came from energy and another fell back to FCR), so the provenance is
# never hidden.
.combine_demand_methods <- function(x) {
  paste(sort(unique(x)), collapse = "+")
}

.empty_feed_demand_total <- function() {
  tibble::tibble(
    year = integer(),
    area_code = integer(),
    livestock_category = character(),
    demand_dm_t = numeric(),
    method_demand = character()
  )
}

.empty_demand_codes <- function() {
  tibble::tibble(
    year = integer(),
    area_code = integer(),
    live_anim_code = integer(),
    demand_dm_t = numeric()
  )
}
