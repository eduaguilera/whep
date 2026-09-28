# Per-capita food supply for the SJOS-N nourishment "just" axis (Module 3,
# Task 3.1). Protein is the SJOS-N nourishment axis; dietary energy is carried
# as a secondary cross-check only. The default "whep_native" method builds the
# supply from the WHEP commodity-balance food element (tonnes) and the
# whep::biomass_coefs nutrition coefficients, divided by national population.
# The "faostat_fbs" method instead consumes FAOSTAT Food Balance Sheet
# per-capita supply directly (the nourishment cross-check / sensitivity of
# locked plan decision 7).
#
# Wiring (#413): with no `data$fbs_supply` injected, "faostat_fbs" reads the
# per-capita elements from the same two FBS pins `build_commodity_balances()`
# and `read_fbs_population()` read -- item 2901 Grand Total, elements 664
# (kcal/capita/day) and 674 (protein g/capita/day), and item 2501 / element 511
# population. `R/read_raw_inputs.R`'s `cb_elements` allow-list still drops them
# from the CBS extraction; this reader goes to the raw pins instead, so the CBS
# build is untouched. The FAOSTAT-area -> bucket resolution, the aggregate
# exclusion (area 351 China, #939) and the new-over-old vintage rule are those
# of `read_fbs_population()`, not a new mapping.

#' Build per-capita food supply for the nourishment axis.
#'
#' @description
#' Assembles per-capita protein and dietary-energy supply, the state variable
#' for the SJOS-N nourishment ("just") axis. Protein is the SJOS-N nourishment
#' axis; dietary energy is a secondary cross-check. The default `"whep_native"`
#' method multiplies the WHEP commodity-balance food element (tonnes fresh
#' matter, per `year`, `area_code`, `item_cbs_code`) by the per-item nutrition
#' coefficients in `whep::biomass_coefs` and divides by national population.
#' Protein per kilogram fresh matter is nitrogen times 6.25
#' (nitrogen-to-protein factor), on the basis selected by `protein_basis`.
#' The nitrogen density is `N_kgN_kgFM` where available, otherwise
#' `Product_kgN_kgDM * Product_kgDM_kgFM`. `Edible_N_kgFM` is not read: it is
#' empty in every coefficient row, upstream as well as in the packaged data, so
#' the edible basis is derived from `Edible_portion` instead of stored
#' redundantly. `N_kgN_kgFM` must be a **food-composition** density for this to
#' mean anything, because `food_t` for a processed FBS item is the primary
#' equivalent of the products actually eaten: a whole-grain nitrogen density on
#' that mass counts milling offal as food. Five cereal rows carry the
#' agronomic value instead, and others a bare literal; both sets are named in
#' [biomass_coefs]. `Wheat` was the largest, at 1.27x FAOSTAT FBS world
#' protein until #796, and now ships on the **flour basis**, 93 g of protein
#' per kg, sourced and measured in [biomass_coefs]. Energy per kilogram
#' fresh matter follows `GE_product_edible_portion_MJ_kgFM`, then
#' `GE_product_MJ_kgFM` (MJ per kg fresh matter), converted to kilocalories via
#' `MJ / 0.004184`. The energy term is GROSS (combustion) energy, not Atwater
#' metabolisable energy, and so is only a secondary cross-check for SJOS-N;
#' Atwater factors could refine it (O-B). Food items with no protein
#' coefficient after the coalesce chain are excluded with a warning naming the
#' count and a few examples (the residual gap-fill, O-B), never silently
#' dropped.
#'
#' The `"faostat_fbs"` method is FAOSTAT's own per-capita supply, the
#' independent benchmark for the default. With `data$fbs_supply` injected it is
#' returned unchanged. Otherwise it is read from the `faostat-fbs-old`
#' (1961-2013) and `faostat-fbs-new` (2010-2023) pins: protein and dietary
#' energy per capita per day from item 2901 "Grand Total" (elements 674 and
#' 664), which FAOSTAT reports directly, and population from item 2501
#' (element 511, thousands, converted to persons). FAOSTAT's energy element is
#' dietary energy as FAOSTAT derives it from food composition, not the gross
#' (combustion) energy of `"whep_native"`, so the two methods' energy columns
#' are not like for like. FAOSTAT areas are
#' resolved onto `area_code` exactly as [read_fbs_population()] resolves them:
#' year by year, dropping any area that resolves to no polity (the regional
#' aggregates and area 351 "China", the aggregate over areas 41, 96, 128 and
#' 214, #939); where more than one FAOSTAT area lands in a bucket-year the
#' per-capita values are population-weighted. `faostat-fbs-new` wins an
#' overlapping `(year, area_code)`. The old pin follows FAOSTAT's pre-2014
#' FBS methodology and the new one the revised methodology, so a series
#' crossing 2010 changes vintage there.
#'
#' An area with food but no `population` row has no denominator, so it is
#' absent from the output rather than wrong in it. Those areas are **named at
#' runtime** in a warning, with the share of food protein that leaves with
#' them, because which areas they are moves with every refresh of the
#' `gdp-population` pin and of the food input: this sentence carried a count of
#' 15 that was already 16 by the time #644 measured it. Read the warning, not a
#' number in the documentation.
#'
#' For orientation only, the areas with food protein and no population row in
#' any year, measured against the `faostat-fbs-new` pin over the population
#' pin's 1850-2021 span, are 13, headed by the `China` aggregate (351), `Sudan`
#' (276) and `South Sudan` (277), then Comoros, New Caledonia, Bhutan and the
#' small island states. Two of those are known open issues rather than data
#' gaps: 351 is the aggregate whose members carry population separately, and
#' 151 `Netherlands Antilles` is #787.
#' `options(whep.warn_missing_population = FALSE)` silences the warning.
#'
#' @param method Supply source: `"whep_native"` (default, commodity-balance
#'   food tonnes times `whep::biomass_coefs` divided by population) or
#'   `"faostat_fbs"` (FAOSTAT FBS per-capita supply, injected or read from
#'   the FBS pins).
#' @param data Named list of injected inputs. For `"whep_native"`:
#'   `cbs_food` (`year`, `area_code`, `item_cbs_code`, `food_t`) and
#'   `population` (`year`, `area_code`, `population`) are required, and
#'   `biomass_coefs` / `items_full` override the packaged
#'   `whep::biomass_coefs` / `whep::items_full`. For `"faostat_fbs"`:
#'   `fbs_supply` (`year`, `area_code`, `protein_g_cap_day`,
#'   `energy_kcal_cap_day`, `population`) is returned as given if supplied;
#'   otherwise `fbs_old` and/or `fbs_new`, the raw pins in their own long
#'   FAOSTAT layout (`Area Code`, `Item Code`, `Element Code`, `Year`,
#'   `Value`), replace the [whep_read_file()] read of whichever is absent.
#' @param protein_basis How the inedible fraction is treated when converting
#'   nitrogen density to protein, for `"whep_native"` only:
#'   `"edible_portion"` (default) scales the nitrogen density by
#'   `Edible_portion`, which is correct when `food_t` is commodity mass while
#'   the density applies to the edible part, and agrees best with FAOSTAT FBS;
#'   `"whole_commodity"` applies no edible scaling, the behaviour before this
#'   argument existed, kept for continuity and sensitivity analysis;
#'   `"product_nitrogen"` uses the agronomic `Product_kgN_kgDM` for both the
#'   edible and inedible fractions, scaled by `Edible_portion`, ignoring
#'   `N_kgN_kgFM`. A missing `Edible_portion` counts as 1.
#' @param example If `TRUE`, return a small fixture instead of computing.
#'   Defaults to `FALSE`.
#' @return A tibble keyed by `year`, `area_code` with `protein_g_cap_day`,
#'   `energy_kcal_cap_day`, `population`, `method_food_supply` and
#'   `method_protein_basis` (`NA` for `"faostat_fbs"`), plus the polity columns
#'   below.
#' @inheritSection whep_polity_columns Polity columns
#' @export
#' @examples
#' build_food_supply(example = TRUE)
build_food_supply <- function(
  method = c("whep_native", "faostat_fbs"),
  data = list(),
  protein_basis = c("edible_portion", "whole_commodity", "product_nitrogen"),
  example = FALSE
) {
  if (isTRUE(example)) {
    return(.example_build_food_supply())
  }
  method <- rlang::arg_match(method)
  protein_basis <- rlang::arg_match(protein_basis)
  out <- if (method == "faostat_fbs") {
    .food_supply_fbs(data)
  } else {
    .food_supply_whep_native(data, protein_basis)
  }
  dplyr::mutate(
    out,
    method_food_supply = method,
    method_protein_basis = if (method == "faostat_fbs") {
      NA_character_
    } else {
      protein_basis
    }
  ) |>
    .add_reporting_polity_columns()
}

# ---- Private helpers -------------------------------------------------------

# faostat_fbs: an injected `data$fbs_supply` passes through, keeping only the
# contract columns; otherwise the supply is read from the two FBS pins (#413).
.food_supply_fbs <- function(data) {
  cols <- .food_supply_cols()
  if (is.null(data$fbs_supply)) {
    return(.read_fbs_supply(data))
  }
  .check_columns(data$fbs_supply, cols, "data$fbs_supply")
  dplyr::select(data$fbs_supply, dplyr::all_of(cols))
}

.food_supply_cols <- function() {
  c(
    "year",
    "area_code",
    "protein_g_cap_day",
    "energy_kcal_cap_day",
    "population"
  )
}

# Both vintages, each parsed and bucketed on its own, then the newer pin's row
# kept for an overlapping (year, area_code) -- `read_fbs_population()`'s rule.
.read_fbs_supply <- function(data) {
  old <- data$fbs_old %||% whep_read_file("faostat-fbs-old")
  new <- data$fbs_new %||% whep_read_file("faostat-fbs-new")
  dplyr::bind_rows(
    .fbs_supply_parse(old, "FAOSTAT FBS old", 2L),
    .fbs_supply_parse(new, "FAOSTAT FBS new", 1L)
  ) |>
    .fbs_pop_prefer_new() |>
    dplyr::arrange(.data$year, .data$area_code) |>
    dplyr::select(dplyr::all_of(.food_supply_cols()))
}

# One pin reduced to Grand Total protein and energy per capita plus population,
# one row per FAOSTAT area and year. Only the five columns named are read, so a
# pin's other columns (the new pin's logical `Note`, #1178) cannot matter.
.fbs_supply_parse <- function(raw, label, rank) {
  needed <- c("Area Code", "Item Code", "Element Code", "Year", "Value")
  .check_columns(raw, needed, "the FAOSTAT FBS table")
  elements <- .fbs_supply_elements()
  tibble::as_tibble(raw) |>
    dplyr::transmute(
      year = as.integer(.data[["Year"]]),
      area_code = as.integer(.data[["Area Code"]]),
      key = paste(
        as.integer(.data[["Item Code"]]),
        as.integer(.data[["Element Code"]])
      ),
      value = as.numeric(.data[["Value"]])
    ) |>
    dplyr::filter(.data$key %in% names(elements)) |>
    dplyr::mutate(variable = unname(elements[.data$key])) |>
    .fbs_supply_require(label) |>
    tidyr::pivot_wider(
      id_cols = c("year", "area_code"),
      names_from = "variable",
      values_from = "value",
      values_fn = dplyr::first
    ) |>
    ensure_columns(.fbs_supply_prototype()) |>
    dplyr::mutate(population = .data$population_thousands * 1000) |>
    .fbs_supply_bucket(rank)
}

# item-element keys -> output column. Item 2901 is FAOSTAT's Grand Total, the
# per-capita supply over every leaf commodity, reported directly; summing
# leaves reproduces it (median relative difference 2e-4 on protein, #413) but
# needs every aggregate item excluded first. Population is in thousands in
# both pins ("1000 persons" old, "1000 No" new).
.fbs_supply_elements <- function() {
  c(
    "2901 674" = "protein_g_cap_day",
    "2901 664" = "energy_kcal_cap_day",
    "2501 511" = "population_thousands"
  )
}

.fbs_supply_prototype <- function() {
  tibble::tibble(
    year = integer(),
    area_code = integer(),
    protein_g_cap_day = numeric(),
    energy_kcal_cap_day = numeric(),
    population_thousands = numeric()
  )
}

# An FBS pin that holds rows but none of the Grand Total protein or population
# elements is a changed pin layout, not a year with no food: abort rather than
# return an empty benchmark that reads as "no data".
.fbs_supply_require <- function(long, label) {
  missing <- setdiff(
    c("protein_g_cap_day", "population_thousands"),
    unique(long$variable)
  )
  if (length(missing) > 0L) {
    cli::cli_abort(c(
      "{.val {label}} carries no {.field {missing}} element.",
      i = "Expected item 2901 / element 674 (protein) and item 2501 /
           element 511 (population) in the FAOSTAT FBS layout."
    ))
  }
  long
}

# FAOSTAT area -> polity bucket, year by year, keeping only areas that resolve
# to a polity (the #939 rule `.fbs_pop_bucket()` applies to population). An
# area-year with no positive population or no protein value cannot be
# expressed per capita and is dropped. A bucket-year holding several FAOSTAT
# areas gets population-weighted per-capita values, never a sum of two
# per-capita figures.
.fbs_supply_bucket <- function(parsed, rank) {
  resolved <- add_polity_code(
    parsed,
    code_column = "area_code",
    year_column = "year"
  )
  .fbs_pop_report_aggregates(resolved)
  resolved |>
    dplyr::filter(
      !is.na(.data$year),
      !is.na(.data$polity_area_code),
      !is.na(.data$polity_code),
      is.finite(.data$population),
      .data$population > 0,
      !is.na(.data$protein_g_cap_day)
    ) |>
    dplyr::summarise(
      protein_g_cap_day = stats::weighted.mean(
        .data$protein_g_cap_day,
        .data$population
      ),
      energy_kcal_cap_day = stats::weighted.mean(
        .data$energy_kcal_cap_day,
        .data$population
      ),
      population = sum(.data$population),
      .by = c("year", "polity_area_code")
    ) |>
    dplyr::transmute(
      year = .data$year,
      area_code = as.integer(.data$polity_area_code),
      protein_g_cap_day = .data$protein_g_cap_day,
      energy_kcal_cap_day = .data$energy_kcal_cap_day,
      population = .data$population,
      source_rank = .env$rank
    )
}

# whep_native: commodity-balance food tonnes times the per-item nutrition
# coefficients, aggregated per country-year and divided by national population.
.food_supply_whep_native <- function(data, protein_basis) {
  cbs_food <- data$cbs_food
  population <- data[["population"]]
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
    .food_join_nutrition(
      .food_nutrition_lookup(items, coefs, protein_basis)
    ) |>
    .food_aggregate() |>
    .food_per_capita(population)
}

# Per-item nutrition coefficients keyed by item_cbs_code. Bridge item_cbs_code
# to Name_biomass (items_full) then to biomass_coefs, deriving protein and
# gross-energy content per kilogram fresh matter.
.food_nutrition_lookup <- function(items, coefs, protein_basis) {
  .check_columns(coefs, .food_coef_cols(), "data$biomass_coefs")
  # ANIMAL PRODUCTS is a section header that leaked into the coefficient table:
  # it carries Edible_portion 4.0 and 3 kg of nitrogen per kg of fresh matter,
  # which as food would be 18.75 kg of protein per kg. No item_cbs_code bridges
  # to it today (asserted in test_biomass_coefs_hygiene.R), so dropping it
  # changes nothing now and stops it going live if a future items_full does.
  coefs <- dplyr::filter(coefs, .data$Name_biomass != "ANIMAL PRODUCTS")
  bridge <- dplyr::distinct(items, .data$item_cbs_code, .data$Name_biomass)
  nutrition <- dplyr::transmute(
    coefs,
    Name_biomass = .data$Name_biomass,
    protein_frac_kgfm = .food_protein_frac(
      nitrogen_edible = .data$N_kgN_kgFM,
      nitrogen_product = .data$Product_kgN_kgDM * .data$Product_kgDM_kgFM,
      edible_portion = .data$Edible_portion,
      protein_basis = protein_basis
    ),
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

.food_coef_cols <- function() {
  c(
    "Name_biomass",
    "N_kgN_kgFM",
    "Product_kgN_kgDM",
    "Product_kgDM_kgFM",
    "Edible_portion",
    "GE_product_edible_portion_MJ_kgFM",
    "GE_product_MJ_kgFM"
  )
}

# Protein mass fraction per kilogram fresh matter, nitrogen times 6.25.
#
# `Edible_N_kgFM` is deliberately NOT read. It is empty in every one of the 421
# coefficient rows, upstream in afsetools as well as in the packaged data
# (#361), so the edible basis is derived here from the populated columns rather
# than stored redundantly in the coefficient table.
#
# The three bases differ in how the inedible fraction is treated:
#   edible_portion    nitrogen density (edible where available, else product)
#                     scaled by the edible fraction of fresh matter. Correct
#                     when `food_t` is commodity mass but the density applies to
#                     the edible part only. Best agreement with FAOSTAT FBS.
#   whole_commodity   no edible scaling; the pre-#361 behaviour, kept selectable
#                     for continuity and sensitivity analysis.
#   product_nitrogen  agronomic product nitrogen for both fractions, scaled by
#                     the edible fraction, ignoring `N_kgN_kgFM`.
#
# A missing `Edible_portion` is treated as 1 (no inedible fraction) in the two
# scaling bases, so an unpopulated row degrades to the whole-commodity value
# rather than to NA.
.food_protein_frac <- function(
  nitrogen_edible,
  nitrogen_product,
  edible_portion,
  protein_basis
) {
  edible_fraction <- dplyr::coalesce(edible_portion, 1)
  nitrogen <- switch(
    protein_basis,
    edible_portion = dplyr::coalesce(nitrogen_edible, nitrogen_product) *
      edible_fraction,
    whole_commodity = dplyr::coalesce(nitrogen_edible, nitrogen_product),
    product_nitrogen = nitrogen_product * edible_fraction
  )
  nitrogen * 6.25
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
# (1 kcal = 0.004184 MJ). Country-years with no population are dropped -- and
# named, because the inner join makes their food vanish from the output rather
# than appear wrong in it (#543).
.food_per_capita <- function(agg, population) {
  .warn_missing_population(agg, population, "protein_t", "food protein")
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
