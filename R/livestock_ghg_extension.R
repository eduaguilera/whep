#' Build the livestock greenhouse-gas emissions extension.
#'
#' @description
#' Aggregate per-animal IPCC livestock emissions into a footprint extension
#' keyed by `(year, area_code, item_cbs_code)`, expressed in kilograms of
#' carbon-dioxide equivalent (CO2e). This bridges the cohort-level emissions
#' pipeline ([calculate_livestock_emissions()]) to the input-output grain used
#' by [build_io_model()] and [compute_footprint()], exactly like
#' [build_grassland_land_extension()] does for land.
#'
#' Live-animal head counts come from [get_primary_production()], are bridged to
#' IPCC species with [prepare_livestock_emissions()], and the resulting enteric
#' and manure emissions are converted to CO2e and summed back to the
#' live-animal commodity sector (`item_cbs_code`, e.g. 961 for non-dairy
#' cattle), which is itself a sector in [build_io_model()].
#'
#' Two IPCC tiers are available, selected with `tier`:
#' - `1` (default): Tier 1 regional emission factors (IPCC 2019). It needs only
#'   species, country and head counts, so it is complete for every country in
#'   [get_primary_production()]. It covers enteric and manure **methane** and
#'   manure **N2O** (direct and indirect, from default per-head nitrogen
#'   excretion rates).
#' - `2`: Tier 2 cohort energy balance (IPCC 2019). It derives enteric CH4 and
#'   manure N2O from a per-animal energy and nitrogen balance, for finer
#'   resolution, but requires cohort weight and diet inputs. Animals whose
#'   emissions cannot be resolved (missing diet or energy data) are dropped with
#'   a warning rather than entering the footprint as `NA`. Its per-head enteric
#'   and manure emissions now sit in the same range as the Tier 1 regional
#'   factors. Tier 1 remains the default because it is complete for every
#'   country in [get_primary_production()], whereas Tier 2 needs cohort and
#'   diet inputs.
#'
#' The CO2e conversion uses 100-year global warming potentials selected with
#' `gwp`:
#' - `"ar6"` (default): IPCC AR6 (2021) Table 7.15, biogenic CH4 = 27,
#'   N2O = 273.
#' - `"ar5"`: IPCC AR5 (2013), CH4 = 28, N2O = 265 (no climate-carbon
#'   feedback).
#' - `"ar4"`: IPCC AR4 (2007), CH4 = 25, N2O = 298.
#'
#' @param tier IPCC tier, `1` (default) or `2`.
#' @param gwp 100-year global warming potential standard, `"ar6"` (default),
#'   `"ar5"` or `"ar4"`.
#' @param method_diet How Tier 2 resolves each herd's `diet_quality`, which
#'   sets DE% and so gross energy, enteric CH4, volatile solids and nitrogen
#'   excretion at once. `"per_cell_feed"` (default) derives it from the feed
#'   mix of the cell the herd is in; `"national_feed"` from the country's own
#'   mix; `"uniform_medium"` assumes the IPCC `"Medium"` diet for every herd.
#'   The gridded rung is the default because WHEP resolves a diet per cell and
#'   a diet varies within a country, so a national mix is a coarsening and an
#'   assumed Medium is coarser still. Both remain selectable. The assumption is
#'   never chosen implicitly, and the rung used is recorded in `method_ghg`.
#'   Ignored at Tier 1, whose emission factors carry no diet dimension.
#' @param data Optional named list of pre-loaded inputs to avoid remote reads:
#'   `primary_prod` (the [get_primary_production()] output) and, for Tier 2
#'   with either feed-derived diet, `feed_intake` (the
#'   [get_feed_intake()] output). `primary_prod` falls back to its reader when
#'   absent; `feed_intake` does not, because [get_feed_intake()] rebuilds the
#'   whole feed allocation and would silently turn this extension into an
#'   hours-long build. Supply it, or choose `method_diet = "uniform_medium"`.
#' @param example If `TRUE`, return a small fixture instead of reading remote
#'   data. Defaults to `FALSE`.
#'
#' @return A tibble with columns `year`, `area_code`, `item_cbs_code`,
#'   `impact_u` (livestock emissions in kilograms CO2e) and `method_ghg` (the
#'   chosen tier and GWP standard, e.g. `"IPCC_2019_Tier1_AR6"`), plus the
#'   polity columns below.
#'
#' @inheritSection whep_polity_columns Polity columns
#'
#' @export
#'
#' @examples
#' build_livestock_ghg_extension(example = TRUE)
build_livestock_ghg_extension <- function(
  tier = 1,
  gwp = c("ar6", "ar5", "ar4"),
  method_diet = c("per_cell_feed", "national_feed", "uniform_medium"),
  data = list(),
  example = FALSE
) {
  tier <- .check_ghg_tier(tier)
  gwp <- match.arg(gwp)
  method_diet <- rlang::arg_match(method_diet)
  if (isTRUE(example)) {
    return(.example_ghg_extension())
  }

  primary_prod <- if (is.null(data$primary_prod)) {
    get_primary_production()
  } else {
    data$primary_prod
  }

  primary_prod |>
    .livestock_emissions_by_sector(tier, method_diet, data$feed_intake) |>
    .ghg_co2e_extension(tier, gwp, method_diet) |>
    .add_reporting_polity_columns()
}

# Run the cohort emissions pipeline, expanding to cohorts only for Tier 2 so
# Tier 1 stays at the lighter species grain. The IO-grain keys (year,
# area_code, item_cbs_code) are carried through unchanged.
#
# Tier 2 needs a diet before it can solve its energy balance, and it no longer
# invents one: the requested rung of the diet ladder is applied here, on the
# same national grain the extension reports on. Tier 1 needs none.
.livestock_emissions_by_sector <- function(
  primary_prod,
  tier,
  method_diet,
  feed_intake
) {
  if (tier != 2L) {
    return(calculate_livestock_emissions(
      prepare_livestock_emissions(primary_prod),
      tier = tier
    ))
  }
  if (method_diet != "uniform_medium" && is.null(feed_intake)) {
    cli::cli_abort(c(
      "Tier 2 with {.arg method_diet} {.val {method_diet}} needs a
       feed-intake table.",
      i = "Pass it as {.code data$feed_intake}, e.g. from
           {.fun get_feed_intake} -- which rebuilds the whole feed allocation
           and is not read for you, because that would turn this extension
           into an hours-long build without saying so.",
      i = "Or select {.val uniform_medium} to assume the IPCC {.val Medium}
           diet explicitly."
    ))
  }
  primary_prod |>
    prepare_livestock_emissions(expand_cohorts = TRUE) |>
    .resolve_diet_quality(method_diet, feed_intake) |>
    calculate_livestock_emissions(tier = tier)
}

# Convert enteric + manure CH4 (and Tier 2 manure N2O) to CO2e with the chosen
# GWP100 factors, then sum to (year, area_code, item_cbs_code).
.ghg_co2e_extension <- function(emissions, tier, gwp, method_diet = NULL) {
  .check_emission_keys(emissions)
  factors <- .ghg_gwp_factors(gwp)
  ch4 <- .sum_emission_cols(
    emissions,
    paste0(c("enteric_ch4_tier", "manure_ch4_tier"), tier)
  )
  n2o <- .sum_emission_cols(emissions, "manure_n2o_total")
  co2e <- ch4 * factors[["ch4"]] + n2o * factors[["n2o"]]
  .warn_dropped_ghg(co2e, emissions, tier)

  emissions |>
    dplyr::mutate(co2e_kg = co2e) |>
    dplyr::filter(!is.na(.data$co2e_kg)) |>
    dplyr::summarise(
      impact_u = sum(.data$co2e_kg, na.rm = TRUE),
      .by = c(year, area_code, item_cbs_code)
    ) |>
    dplyr::mutate(
      year = as.integer(.data$year),
      area_code = as.integer(.data$area_code),
      item_cbs_code = as.integer(.data$item_cbs_code),
      method_ghg = .ghg_method_label(tier, gwp, method_diet)
    ) |>
    dplyr::filter(.data$impact_u > 0) |>
    dplyr::select(year, area_code, item_cbs_code, impact_u, method_ghg)
}

# Row-wise sum of the requested emission columns. An absent column now aborts:
# it used to contribute zero, so a renamed or dropped emission column would
# quietly remove a whole gas from the footprint while every total still
# reconciled. Both tiers produce all three columns (Tier 1 manure N2O writes
# into the same `manure_n2o_total`), so nothing legitimately arrives without
# them. An NA within a present column still propagates, so unresolved rows can
# be detected and dropped rather than silently zeroed.
.sum_emission_cols <- function(emissions, cols) {
  missing <- setdiff(cols, names(emissions))
  if (length(missing) > 0L) {
    cli::cli_abort(c(
      "Livestock emissions are missing column{?s}: {.field {missing}}.",
      i = "An absent emission column is a missing gas, not a zero one."
    ))
  }
  Reduce(`+`, lapply(cols, function(col) emissions[[col]]))
}

# IPCC 100-year global warming potentials (kg CO2e per kg gas).
.ghg_gwp_factors <- function(gwp) {
  switch(
    gwp,
    ar6 = c(ch4 = 27, n2o = 273),
    ar5 = c(ch4 = 28, n2o = 265),
    ar4 = c(ch4 = 25, n2o = 298)
  )
}

# Tier 1 emission factors carry no diet dimension, so only the Tier 2 label
# records which rung of the diet ladder produced the numbers.
.ghg_method_label <- function(tier, gwp, method_diet = NULL) {
  label <- paste0("IPCC_2019_Tier", tier, "_", toupper(gwp))
  if (tier != 2L || is.null(method_diet)) {
    return(label)
  }
  paste0(label, "_diet_", method_diet)
}

.warn_dropped_ghg <- function(co2e, emissions, tier) {
  n_na <- sum(is.na(co2e))
  if (n_na == 0L) {
    return(invisible())
  }

  systematic_species <- .species_with_no_tier_coefs(co2e, emissions)
  n_systematic <- sum(
    is.na(co2e) & emissions$species_gen %in% systematic_species
  )
  n_partial <- n_na - n_systematic

  if (length(systematic_species) > 0L) {
    demonstrative <- if (length(systematic_species) == 1L) "this" else "these"
    cli::cli_warn(c(
      "!" = "Dropping {n_systematic} livestock row{?s} for
        {.val {systematic_species}}: no Tier {tier} coefficients exist for
        {demonstrative} species.",
      "i" = "This is a systematic gap (the whole species has no matching
        coefficient row), not a missing-data one -- Tier 1 (the package
        default) still covers {demonstrative} species."
    ))
  }

  if (n_partial > 0L) {
    cli::cli_warn(c(
      "!" = "Dropping {n_partial} livestock row{?s} with unresolved emissions.",
      "i" = "These rows lack the cohort or diet inputs the Tier 2 energy
        balance needs."
    ))
  }
}

# Species where EVERY row lacking co2e is missing (as opposed to some rows of
# a species resolving fine and others not, which points to a per-row data gap
# rather than a species entirely absent from the tier's coefficient tables).
.species_with_no_tier_coefs <- function(co2e, emissions) {
  if (!rlang::has_name(emissions, "species_gen")) {
    return(character())
  }
  emissions |>
    dplyr::mutate(.na = is.na(co2e)) |>
    dplyr::summarise(.all_na = all(.na), .by = species_gen) |>
    dplyr::filter(.all_na) |>
    dplyr::pull(species_gen)
}

.check_ghg_tier <- function(tier) {
  if (
    !is.numeric(tier) ||
      length(tier) != 1L ||
      is.na(tier) ||
      !tier %in% c(1, 2)
  ) {
    cli::cli_abort("{.arg tier} must be 1 or 2.")
  }
  as.integer(tier)
}

.check_emission_keys <- function(emissions) {
  keys <- c("year", "area_code", "item_cbs_code")
  missing <- keys[!keys %in% names(emissions)]
  if (length(missing) > 0L) {
    cli::cli_abort(
      "Livestock emissions input is missing required column{?s}:
      {.field {missing}}."
    )
  }
}
