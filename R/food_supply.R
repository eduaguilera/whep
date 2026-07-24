# Per-capita food supply for the SJOS-N nourishment "just" axis (Module 3,
# Task 3.1). Protein is the SJOS-N nourishment axis; dietary energy is carried
# as a secondary cross-check only. The default "whep_native" method builds the
# supply from the WHEP commodity-balance food element (tonnes) and the
# whep::biomass_coefs nutrition coefficients, divided by national population.
# The "faostat_fbs" method instead consumes FAOSTAT Food Balance Sheet
# per-capita supply directly (the nourishment cross-check / sensitivity of
# locked plan decision 7).
#
# Integration note: re-enabling FAOSTAT's per-capita FBS nutritional elements,
# which R/read_raw_inputs.R currently drops at extraction (the cb_elements
# selection), so that data$fbs_supply can be assembled from the pins rather than
# injected, is the "faostat_fbs" wiring step and is out of scope for this
# fixture-tested function.

#' Build per-capita food supply for the nourishment axis.
#'
#' @description
#' Assembles per-capita protein and dietary-energy supply, the state variable
#' for the SJOS-N nourishment ("just") axis. Protein is the SJOS-N nourishment
#' axis; dietary energy is a secondary cross-check. The default `"whep_native"`
#' method multiplies the WHEP commodity-balance food element (tonnes fresh
#' matter, per `year`, `area_code`, `item_cbs_code`) by the per-item nutrition
#' coefficients in `whep::biomass_coefs` and divides by national population.
#' Protein per kilogram fresh matter follows the coalesce chain `Edible_N_kgFM`,
#' then `N_kgN_kgFM`, then `Product_kgN_kgDM * Product_kgDM_kgFM` (kg N per kg
#' fresh matter), times 6.25 (nitrogen-to-protein factor). Energy per kilogram
#' fresh matter follows `GE_product_edible_portion_MJ_kgFM`, then
#' `GE_product_MJ_kgFM` (MJ per kg fresh matter), converted to kilocalories via
#' `MJ / 0.004184`. The energy term is GROSS (combustion) energy, not Atwater
#' metabolisable energy, and so is only a secondary cross-check for SJOS-N;
#' Atwater factors could refine it (O-B). Food items with no protein
#' coefficient after the coalesce chain are excluded with a warning naming the
#' count and a few examples (the residual gap-fill, O-B), never silently
#' dropped. The `"faostat_fbs"` method returns the injected FAOSTAT Food
#' Balance Sheet per-capita supply unchanged, as a cross-check / sensitivity.
#'
#' @param method Supply source: `"whep_native"` (default, commodity-balance
#'   food tonnes times `whep::biomass_coefs` divided by population) or
#'   `"faostat_fbs"` (the injected FAOSTAT FBS per-capita supply).
#' @param data Named list of injected inputs. For `"whep_native"`:
#'   `cbs_food` (`year`, `area_code`, `item_cbs_code`, `food_t`) and
#'   `population` (`year`, `area_code`, `population`) are required, and
#'   `biomass_coefs` / `items_full` override the packaged
#'   `whep::biomass_coefs` / `whep::items_full`. For `"faostat_fbs"`:
#'   `fbs_supply` (`year`, `area_code`, `protein_g_cap_day`,
#'   `energy_kcal_cap_day`, `population`) is required.
#' @param example If `TRUE`, return a small fixture instead of computing.
#'   Defaults to `FALSE`.
#' @return A tibble keyed by `year`, `area_code` with `protein_g_cap_day`,
#'   `energy_kcal_cap_day` and `population`.
#' @export
#' @examples
#' build_food_supply(example = TRUE)
build_food_supply <- function(
  method = c("whep_native", "faostat_fbs"),
  data = list(),
  example = FALSE
) {
  if (isTRUE(example)) {
    return(.example_build_food_supply())
  }
  method <- rlang::arg_match(method)
  if (method == "faostat_fbs") {
    return(.food_supply_fbs(data))
  }
  .food_supply_whep_native(data)
}

# ---- Private helpers -------------------------------------------------------

# faostat_fbs: pass the injected FAOSTAT FBS per-capita supply through, keeping
# only the contract columns.
.food_supply_fbs <- function(data) {
  cols <- c(
    "year",
    "area_code",
    "protein_g_cap_day",
    "energy_kcal_cap_day",
    "population"
  )
  .check_columns(data$fbs_supply, cols, "data$fbs_supply")
  dplyr::select(data$fbs_supply, dplyr::all_of(cols))
}

# whep_native: commodity-balance food tonnes times the per-item nutrition
# coefficients, aggregated per country-year and divided by national population.
.food_supply_whep_native <- function(data) {
  cbs_food <- data$cbs_food
  population <- data$population
  coefs <- data$biomass_coefs %||% whep::biomass_coefs
  items <- data$items_full %||% whep::items_full
  .check_columns(
    cbs_food,
    c("year", "area_code", "item_cbs_code", "food_t"),
    "data$cbs_food"
  )
  .check_columns(
    population,
    c("year", "area_code", "population"),
    "data$population"
  )
  cbs_food |>
    .food_join_nutrition(.food_nutrition_lookup(items, coefs)) |>
    .food_aggregate() |>
    .food_per_capita(population)
}

# Per-item nutrition coefficients keyed by item_cbs_code. Bridge item_cbs_code
# to Name_biomass (items_full) then to biomass_coefs, deriving protein and
# gross-energy content per kilogram fresh matter with the documented coalesce
# chains. Protein is nitrogen times 6.25.
.food_nutrition_lookup <- function(items, coefs) {
  bridge <- dplyr::distinct(items, .data$item_cbs_code, .data$Name_biomass)
  nutrition <- dplyr::transmute(
    coefs,
    Name_biomass = .data$Name_biomass,
    protein_frac_kgfm = dplyr::coalesce(
      .data$Edible_N_kgFM,
      .data$N_kgN_kgFM,
      .data$Product_kgN_kgDM * .data$Product_kgDM_kgFM
    ) *
      6.25,
    energy_mj_kgfm = dplyr::coalesce(
      .data$GE_product_edible_portion_MJ_kgFM,
      .data$GE_product_MJ_kgFM
    )
  ) |>
    # One coefficient row per biomass name: biomass_coefs carries duplicate
    # Name_biomass rows (e.g. livestock cohorts), and without this a name with
    # >1 row would fan out and double-count food_t downstream.
    dplyr::distinct(.data$Name_biomass, .keep_all = TRUE)
  dplyr::left_join(bridge, nutrition, by = "Name_biomass")
}

# Attach the nutrition coefficients to the food tonnes and drop (with a
# warning) items that carry no protein coefficient after the coalesce chain.
.food_join_nutrition <- function(cbs_food, nutrition) {
  joined <- dplyr::left_join(cbs_food, nutrition, by = "item_cbs_code")
  .food_warn_unmatched(joined)
  dplyr::filter(joined, !is.na(.data$protein_frac_kgfm))
}

# Warn (never silently drop) about food items with no protein coefficient,
# naming the count and a few example item codes.
.food_warn_unmatched <- function(joined) {
  unmatched <- joined |>
    dplyr::filter(is.na(.data$protein_frac_kgfm)) |>
    dplyr::distinct(.data$item_cbs_code)
  n_unmatched <- nrow(unmatched)
  if (n_unmatched == 0L) {
    return(invisible())
  }
  examples <- unmatched$item_cbs_code[seq_len(min(3L, n_unmatched))]
  cli::cli_warn(c(
    "!" = "Excluding {n_unmatched} food item{?s} with no protein
           coefficient after the coalesce chain.",
    "i" = "Example item codes: {examples}."
  ))
}

# Country-year totals: protein tonnes (food tonnes times the protein mass
# fraction) and gross energy megajoules (food tonnes times 1000 kg per tonne
# times the energy density). A missing energy density contributes zero energy
# (na.rm) while the item still counts for protein.
.food_aggregate <- function(joined) {
  dplyr::summarise(
    joined,
    protein_t = sum(.data$food_t * .data$protein_frac_kgfm, na.rm = TRUE),
    energy_mj = sum(
      .data$food_t * 1000 * .data$energy_mj_kgfm,
      na.rm = TRUE
    ),
    .by = c("year", "area_code")
  )
}

# Divide the country-year totals by population and 365 days. Protein tonnes to
# grams is 1e6; gross-energy megajoules to kilocalories is division by 0.004184
# (1 kcal = 0.004184 MJ). Country-years with no population are dropped.
.food_per_capita <- function(agg, population) {
  agg |>
    dplyr::inner_join(population, by = c("year", "area_code")) |>
    dplyr::transmute(
      year = .data$year,
      area_code = .data$area_code,
      protein_g_cap_day = .data$protein_t * 1e6 / .data$population / 365,
      energy_kcal_cap_day = .data$energy_mj /
        0.004184 /
        .data$population /
        365,
      population = .data$population
    )
}
