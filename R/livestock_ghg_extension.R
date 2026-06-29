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
#'   a warning rather than entering the footprint as `NA`. **Tier 2 is not yet
#'   calibrated** -- its enteric CH4 currently runs well above Tier 1 -- so
#'   Tier 1 is the recommended default until the Tier 2 energy model is
#'   validated.
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
#' @param data Optional named list of pre-loaded inputs to avoid remote reads:
#'   `primary_prod` (the [get_primary_production()] output). It falls back to
#'   its reader when absent.
#' @param example If `TRUE`, return a small fixture instead of reading remote
#'   data. Defaults to `FALSE`.
#'
#' @return A tibble with columns `year`, `area_code`, `item_cbs_code`,
#'   `impact_u` (livestock emissions in kilograms CO2e) and `method_ghg` (the
#'   chosen tier and GWP standard, e.g. `"IPCC_2019_Tier1_AR6"`).
#'
#' @export
#'
#' @examples
#' build_livestock_ghg_extension(example = TRUE)
build_livestock_ghg_extension <- function(
  tier = 1,
  gwp = c("ar6", "ar5", "ar4"),
  data = list(),
  example = FALSE
) {
  tier <- .check_ghg_tier(tier)
  gwp <- match.arg(gwp)
  if (isTRUE(example)) {
    return(.example_ghg_extension())
  }

  primary_prod <- if (is.null(data$primary_prod)) {
    get_primary_production()
  } else {
    data$primary_prod
  }

  primary_prod |>
    .livestock_emissions_by_sector(tier) |>
    .ghg_co2e_extension(tier, gwp)
}

# Run the cohort emissions pipeline, expanding to cohorts only for Tier 2 so
# Tier 1 stays at the lighter species grain. The IO-grain keys (year,
# area_code, item_cbs_code) are carried through unchanged.
.livestock_emissions_by_sector <- function(primary_prod, tier) {
  prepared <- if (tier == 2L) {
    prepare_livestock_emissions(primary_prod, expand_cohorts = TRUE)
  } else {
    prepare_livestock_emissions(primary_prod)
  }
  calculate_livestock_emissions(prepared, tier = tier)
}

# Convert enteric + manure CH4 (and Tier 2 manure N2O) to CO2e with the chosen
# GWP100 factors, then sum to (year, area_code, item_cbs_code).
.ghg_co2e_extension <- function(emissions, tier, gwp) {
  .check_emission_keys(emissions)
  factors <- .ghg_gwp_factors(gwp)
  ch4 <- .sum_emission_cols(
    emissions,
    paste0(c("enteric_ch4_tier", "manure_ch4_tier"), tier)
  )
  n2o <- .sum_emission_cols(emissions, "manure_n2o_total")
  co2e <- ch4 * factors[["ch4"]] + n2o * factors[["n2o"]]
  .warn_dropped_ghg(co2e)

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
      method_ghg = .ghg_method_label(tier, gwp)
    ) |>
    dplyr::filter(.data$impact_u > 0) |>
    dplyr::select(year, area_code, item_cbs_code, impact_u, method_ghg)
}

# Row-wise sum of the requested emission columns. Absent columns (e.g. manure
# N2O at Tier 1) contribute zero; an NA within a present column propagates so
# unresolved rows can be detected and dropped rather than silently zeroed.
.sum_emission_cols <- function(emissions, cols) {
  present <- cols[cols %in% names(emissions)]
  if (length(present) == 0L) {
    return(rep(0, nrow(emissions)))
  }
  Reduce(`+`, lapply(present, function(col) emissions[[col]]))
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

.ghg_method_label <- function(tier, gwp) {
  paste0("IPCC_2019_Tier", tier, "_", toupper(gwp))
}

.warn_dropped_ghg <- function(co2e) {
  n_na <- sum(is.na(co2e))
  if (n_na > 0L) {
    cli::cli_warn(c(
      "!" = "Dropping {n_na} livestock row{?s} with unresolved emissions.",
      "i" = "These rows lack the cohort or diet inputs the Tier 2 energy
        balance needs."
    ))
  }
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
