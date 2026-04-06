#' Calculate all livestock emissions.
#'
#' @description
#' Main dispatcher that runs the full IPCC 2019 livestock
#' emissions pipeline: energy demand (Tier 2 GE), enteric CH4,
#' manure CH4, and manure N2O.
#'
#' Selects tier automatically: Tier 2 when cohort-level data
#' (weight, diet) are available; Tier 1 otherwise.
#'
#' @param data Dataframe with at minimum `species` and `heads`.
#'   For Tier 2, also needs `cohort`, `weight` (or `iso3`),
#'   `diet_quality`, and production columns.
#' @param tier Integer 1 or 2. If `NULL` (default), auto-selects
#'   based on data completeness.
#'
#' @return Dataframe with all emission columns, method tracking,
#'   and original data columns preserved.
#' @export
#'
#' @examples
#' \dontrun{
#'   tibble::tibble(
#'     species = "Dairy Cattle",
#'     cohort = "Adult Female",
#'     heads = 1000,
#'     weight = 600,
#'     diet_quality = "High",
#'     milk_yield_kg_day = 20
#'   ) |>
#'     calculate_livestock_emissions()
#' }
calculate_livestock_emissions <- function(data,
                                          tier = NULL) {
  tier <- .resolve_tier(data, tier)

  if (tier == 2) {
    .run_tier2(data)
  } else {
    .run_tier1(data)
  }
}

#' Calculate enteric methane emissions.
#'
#' @description
#' Wrapper that selects Tier 1 or 2 for enteric CH4
#' based on data availability.
#'
#' @param data Dataframe with `species`, `heads`.
#' @param tier Integer 1 or 2.
#'
#' @return Dataframe with enteric CH4 results.
#' @export
calculate_enteric_ch4 <- function(data, tier = NULL) {
  tier <- .resolve_tier(data, tier)
  if (tier == 2) {
    data |>
      estimate_energy_demand() |>
      .calc_enteric_ch4_tier2()
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
#' @param tier Integer 1 or 2.
#'
#' @return Dataframe with manure CH4 and N2O results.
#' @export
calculate_manure_emissions <- function(data, tier = NULL) {
  tier <- .resolve_tier(data, tier)
  if (tier == 2) {
    data <- data |>
      estimate_energy_demand() |>
      .calc_manure_ch4_tier2() |>
      .calc_manure_n2o()
  } else {
    data <- .calc_manure_ch4_tier1(data)
  }
  data
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
.run_tier2 <- function(data) {
  data |>
    estimate_energy_demand() |>
    .calc_enteric_ch4_tier2() |>
    .calc_manure_ch4_tier2() |>
    .calc_manure_n2o()
}

#' Run full Tier 1 pipeline.
#' @noRd
.run_tier1 <- function(data) {
  data |>
    .calc_enteric_ch4_tier1() |>
    .calc_manure_ch4_tier1()
}
