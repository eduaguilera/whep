#' Calculate all livestock emissions.
#'
#' @description
#' Main dispatcher that runs the full IPCC 2019 livestock
#' emissions pipeline: energy demand (Tier 2), enteric CH4,
#' manure CH4, and manure N2O.
#'
#' Selects tier automatically: Tier 2 when cohort-level data
#' (weight, diet) are available; Tier 1 otherwise.
#'
#' WHEP's Tier 2 energy balance covers cattle, buffalo, sheep and goats, the
#' species [ipcc_tier2_energy_coefs] ships coefficients for. The IPCC 2019
#' Refinement (Vol. 4 Ch. 10, Table 10.9 (Updated)) suggests Tier 1 for
#' camels, horses, mules and asses and swine, and develops no enteric method
#' for poultry. At Tier 2 those species take Tier 1 by default, with a
#' message and `"IPCC_2019_Tier1"` in each row's `method_*` columns, so a
#' Tier 2 run covers the same animals as a Tier 1 run.
#' `options$tier2_uncovered` selects `"leave_na"` or `"abort"` instead.
#'
#' @param data Dataframe with at minimum `species` and `heads`.
#'   For Tier 2, also needs `cohort`, `weight` (or `iso3`),
#'   `diet_quality`, and production columns.
#' @param tier Integer 1 or 2. If `NULL` (default), auto-selects
#'   based on data completeness.
#' @inheritParams manure_engine_options
#'
#' @return Dataframe with all emission columns, method tracking,
#'   and original data columns preserved.
#' @export
#'
#' @examples
#' tibble::tibble(
#'   species = "Dairy Cattle",
#'   cohort = "Adult Female",
#'   heads = 1000,
#'   weight = 600,
#'   diet_quality = "High",
#'   milk_yield_kg_day = 20
#' ) |>
#'   calculate_livestock_emissions() |>
#'   dplyr::select(species, cohort, heads,
#'     enteric_ch4_tier2, manure_ch4_tier2,
#'     manure_n2o_total)
calculate_livestock_emissions <- function(data, tier = NULL, options = list()) {
  tier <- .resolve_tier(data, tier)
  data <- .as_livestock_tibble(data)

  if (tier == 2) {
    .run_tier2(data, options)
  } else {
    .run_tier1(data, options)
  }
}

#' Calculate enteric methane emissions.
#'
#' @description
#' Wrapper that selects Tier 1 or 2 for enteric CH4
#' based on data availability.
#'
#' @param data Dataframe with `species`, `heads`.
#'   For Tier 2, also needs `cohort`, `weight`, and
#'   `diet_quality`. For Tier 1, `iso3` is used to
#'   select regional emission factors.
#' @param tier Integer 1 or 2. If `NULL` (default),
#'   auto-selects based on data completeness.
#' @inheritParams manure_engine_options
#'
#' @return Dataframe with all input columns preserved, plus:
#'   - `method_enteric`: tracking label
#'     (`"IPCC_2019_Tier1"` or `"IPCC_2019_Tier2"`). At Tier 2 a species with
#'     no Tier 2 method carries `"IPCC_2019_Tier1"` and its Tier 1 value (see
#'     `tier2_uncovered` under `options`).
#'   - Tier 1: `enteric_ef_kgch4` (emission factor),
#'     `enteric_ch4_tier1` (total kg CH4).
#'   - Tier 2: `gross_energy`, `ym_factor`,
#'     `enteric_ch4_per_head` (kg CH4/head/yr),
#'     `enteric_ch4_tier2` (total kg CH4).
#' @export
#'
#' @examples
#' tibble::tibble(
#'   species = "Cattle", heads = 1000, iso3 = "DEU"
#' ) |>
#'   calculate_enteric_ch4(tier = 1)
calculate_enteric_ch4 <- function(data, tier = NULL, options = list()) {
  tier <- .resolve_tier(data, tier)
  data <- .as_livestock_tibble(data)
  if (tier == 2) {
    .with_tier2_coverage(
      data,
      options,
      .enteric_tier2_chain,
      .enteric_tier1_chain
    )
  } else {
    .calc_enteric_ch4_tier1(data)
  }
}

#' Calculate manure emissions (CH4 + N2O).
#'
#' @description
#' Wrapper that selects Tier 1 or 2 for manure CH4 and
#' computes N2O (Tier 2 only; skipped for Tier 1).
#'
#' @param data Dataframe with `species`, `heads`.
#'   For Tier 2, also needs `cohort`, `weight`, and
#'   `diet_quality`. For Tier 1, `iso3` is used to
#'   select regional emission factors.
#' @param tier Integer 1 or 2. If `NULL` (default),
#'   auto-selects based on data completeness.
#' @inheritParams manure_engine_options
#'
#' @return Dataframe with all input columns preserved, plus:
#'   - `method_manure_ch4`: tracking label.
#'   - `method_mms`: which half of [regional_mms_distribution] was read and
#'     how it was keyed, `"<shares>/<keying>"` (e.g.
#'     `"gleam_2_0/region_specific"`).
#'   - Tier 1: `manure_ef_kgch4`, `manure_ch4_tier1`.
#'   - Tier 2: `volatile_solids`, `methane_potential`,
#'     `weighted_mcf`, `manure_ch4_per_head`,
#'     `manure_ch4_tier2`.
#'   - N2O (both tiers): `method_manure_n2o`,
#'     `n_excretion`, `manure_n2o_direct`,
#'     `manure_n2o_indirect`, `manure_n2o_total`. Tier 1 uses default per-head
#'     excretion rates; Tier 2 uses the energy/nitrogen balance.
#' @export
#'
#' @examples
#' tibble::tibble(
#'   species = "Cattle", heads = 1000, iso3 = "DEU"
#' ) |>
#'   calculate_manure_emissions(tier = 1)
calculate_manure_emissions <- function(data, tier = NULL, options = list()) {
  tier <- .resolve_tier(data, tier)
  data <- .as_livestock_tibble(data)
  if (tier == 2) {
    .with_tier2_coverage(
      data,
      options,
      .manure_tier2_chain,
      .manure_tier1_chain
    )
  } else {
    .manure_tier1_chain(data, options)
  }
}

# Private helpers ----

#' Resolve tier from data or user specification.
#' @noRd
.resolve_tier <- function(data, tier) {
  if (!is.null(tier)) {
    if (!tier %in% c(1L, 2L, 1, 2)) {
      cli::cli_abort("{.arg tier} must be 1 or 2.")
    }
    return(as.integer(tier))
  }

  has_tier2 <- rlang::has_name(data, "cohort") &&
    (rlang::has_name(data, "weight") ||
      rlang::has_name(data, "iso3"))

  if (has_tier2) 2L else 1L
}

#' Run full Tier 2 pipeline.
#' @noRd
.run_tier2 <- function(data, options = list()) {
  .with_tier2_coverage(data, options, .run_tier2_chain, .run_tier1)
}

#' The Tier 2 energy-balance chain, for the species it covers.
#' @noRd
.run_tier2_chain <- function(data, options = list()) {
  data |>
    estimate_energy_demand() |>
    .calc_enteric_ch4_tier2() |>
    .calc_manure_ch4_tier2(options) |>
    .calc_manure_n2o(options)
}

.enteric_tier2_chain <- function(data, options = list()) {
  data |>
    estimate_energy_demand() |>
    .calc_enteric_ch4_tier2()
}

.enteric_tier1_chain <- function(data, options = list()) {
  .calc_enteric_ch4_tier1(data)
}

.manure_tier2_chain <- function(data, options = list()) {
  data |>
    estimate_energy_demand() |>
    .calc_manure_ch4_tier2(options) |>
    .calc_manure_n2o(options)
}

.manure_tier1_chain <- function(data, options = list()) {
  data |>
    .calc_manure_ch4_tier1() |>
    .calc_manure_n2o_tier1(options)
}

# Tier 2 coverage (whep#1028) ----

#' Run a Tier 2 chain on the species it covers, and the rest as chosen.
#'
#' The Tier 2 energy balance needs the IPCC 2019 maintenance and activity
#' coefficients of Tables 10.4 and 10.5, which `ipcc_tier2_energy_coefs` ships for cattle,
#' buffalo, sheep and goats only. Every other species used to come out of the
#' chain as `NA` without a word, so a "Tier 2" run quietly covered fewer
#' animals than Tier 1. The 2019 Refinement (Vol. 4 Ch. 10, Table 10.9
#' (Updated)) itself suggests Tier 1 for camels, horses, mules and asses and
#' swine, and develops no enteric method for poultry; its Tier 2 manure
#' equations for swine and poultry need a country-specific dry-matter intake
#' (Equation 10.32A) that WHEP does not hold. The `tier2_uncovered` option
#' says what happens to those species, and no choice is silent: each one
#' speaks, and Tier 1 rows keep their `"IPCC_2019_Tier1"` method stamps.
#' @noRd
.with_tier2_coverage <- function(data, options, tier2_fn, tier1_fn) {
  handling <- .manure_options(options)$tier2_uncovered
  covered <- .has_tier2_method(data$species)
  if (all(covered)) {
    return(tier2_fn(data, options))
  }
  .signal_tier2_uncovered(data[!covered, ], handling)
  if (handling == "leave_na") {
    return(tier2_fn(data, options))
  }
  data <- dplyr::mutate(data, .tier2_row = dplyr::row_number())
  tier1_rows <- data[!covered, ] |>
    tier1_fn(options) |>
    .tier1_into_tier2_columns()
  tier2_rows <- if (any(covered)) tier2_fn(data[covered, ], options)
  dplyr::bind_rows(tier2_rows, tier1_rows) |>
    dplyr::arrange(.data$.tier2_row) |>
    dplyr::select(-".tier2_row")
}

#' Whether WHEP has a Tier 2 method for each species label.
#' @noRd
.has_tier2_method <- function(species) {
  .get_general_species(species) %in% ipcc_tier2_energy_coefs$category
}

#' Move a Tier 1 result into the columns a Tier 2 consumer reads.
#'
#' The `method_*` columns the Tier 1 helpers stamp (`"IPCC_2019_Tier1"`) stay
#' on the row, so which tier produced each value is recorded per row.
#' @noRd
.tier1_into_tier2_columns <- function(data) {
  moves <- c(
    enteric_ch4_tier2 = "enteric_ch4_tier1",
    enteric_ch4_per_head = "enteric_ef_kgch4",
    manure_ch4_tier2 = "manure_ch4_tier1",
    manure_ch4_per_head = "manure_ef_kgch4"
  )
  moves <- moves[moves %in% names(data)]
  data[names(moves)] <- data[unname(moves)]
  dplyr::select(
    data,
    -dplyr::any_of(c("enteric_ch4_tier1", "manure_ch4_tier1"))
  )
}

#' Say which species the Tier 2 method does not cover, and what happens.
#' @noRd
.signal_tier2_uncovered <- function(rows, handling) {
  herd <- rows |>
    dplyr::mutate(
      species_gen = .get_general_species(.data$species),
      .n = .animal_count(rows)
    ) |>
    dplyr::summarise(heads = sum(.data$.n, na.rm = TRUE), .by = "species_gen")
  species <- herd$species_gen
  heads <- format(round(herd$heads), big.mark = ",", scientific = FALSE)
  bullets <- c(
    "WHEP has no IPCC Tier 2 method for {cli::qty(length(species))}
     species {.val {species}}.",
    i = "Head count affected, in the same order: {heads}.",
    i = "IPCC 2019 Table 10.9 (Updated) suggests Tier 1 for them; set
         {.code options$tier2_uncovered} to choose what happens."
  )
  switch(
    handling,
    tier1 = cli::cli_inform(
      c(bullets, v = "Using Tier 1 for them, stamped in {.field method_*}."),
      class = "whep_tier2_uncovered"
    ),
    leave_na = cli::cli_warn(
      c(bullets, "!" = "Leaving their emissions {.val NA}."),
      class = "whep_tier2_uncovered"
    ),
    abort = cli::cli_abort(bullets, class = "whep_tier2_uncovered")
  )
}

#' Run full Tier 1 pipeline.
#' @noRd
.run_tier1 <- function(data, options = list()) {
  data |>
    .calc_enteric_ch4_tier1() |>
    .calc_manure_ch4_tier1() |>
    .calc_manure_n2o_tier1(options)
}
