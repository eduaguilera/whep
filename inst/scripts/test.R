# Interactive test script for new exported functions in this PR.
# Run: Rscript inst/scripts/test_new_exports.R
# Or source interactively to inspect results.

if (file.exists("renv/activate.R")) {
  source("renv/activate.R")
}
devtools::load_all(quiet = TRUE)

# -- build_* functions (use example = TRUE, no remote data) --

cat("\n=== build_primary_production(example = TRUE) ===\n")
prod <- build_primary_production(example = TRUE)
stopifnot(tibble::is_tibble(prod), nrow(prod) > 0)
print(prod)

cat("\n=== build_commodity_balances(example = TRUE) ===\n")
cbs <- build_commodity_balances(example = TRUE)
stopifnot(tibble::is_tibble(cbs), nrow(cbs) > 0)
print(cbs)

cat("\n=== build_processing_coefs(example = TRUE) ===\n")
proc <- build_processing_coefs(cbs, example = TRUE)
stopifnot(tibble::is_tibble(proc), nrow(proc) > 0)
print(proc)

# -- Livestock pipeline: Tier 1 (minimal input) --

cat("\n=== prepare_livestock_emissions() ===\n")
# Use item_cbs_code values (not Item_Code):
# 961 = Cattle non-dairy, 946 = Buffaloes,
# 976 = Sheep, 1016 = Goats, 1096 = Horses
prep_input <- tibble::tibble(
  item_cbs_code = c(961L, 946L, 976L, 1016L, 1096L),
  unit = "heads",
  value = c(5000, 3000, 8000, 12000, 500),
  area_code = 79L
)
prep <- prepare_livestock_emissions(prep_input)
stopifnot(tibble::is_tibble(prep), nrow(prep) > 0)
stopifnot("species" %in% names(prep), "heads" %in% names(prep))
print(prep)

cat("\n=== calculate_enteric_ch4(tier = 1) ===\n")
tier1_input <- tibble::tibble(
  species = c("Cattle", "Sheep", "Goats", "Swine"),
  heads = c(1000, 5000, 3000, 10000),
  iso3 = "DEU"
)
enteric <- calculate_enteric_ch4(tier1_input, tier = 1)
stopifnot(tibble::is_tibble(enteric), nrow(enteric) > 0)
print(enteric)

cat("\n=== calculate_manure_emissions(tier = 1) ===\n")
manure <- calculate_manure_emissions(tier1_input, tier = 1)
stopifnot(tibble::is_tibble(manure), nrow(manure) > 0)
print(manure)

# -- Livestock pipeline: Tier 2 (cohort-level input) --

cat("\n=== estimate_energy_demand() ===\n")
tier2_input <- tibble::tibble(
  species = "Dairy Cattle",
  cohort = "Adult Female",
  weight = 600,
  milk_yield_kg_day = 20,
  fat_percent = 4.0,
  diet_quality = "High",
  heads = 1000
)
energy <- estimate_energy_demand(tier2_input)
stopifnot(tibble::is_tibble(energy), nrow(energy) > 0)
stopifnot("gross_energy" %in% names(energy))
print(energy)

cat("\n=== calculate_livestock_emissions() (Tier 2) ===\n")
full <- calculate_livestock_emissions(tier2_input)
stopifnot(tibble::is_tibble(full), nrow(full) > 0)
print(
  full |>
    dplyr::select(
      species, cohort, heads,
      dplyr::matches("enteric|manure|n2o")
    )
)

cat("\n=== calculate_cohorts_systems() ===\n")
cohort_input <- tibble::tibble(
  species = c("Cattle", "Sheep", "Pigs"),
  heads = c(1000, 5000, 8000)
)
cohorts <- calculate_cohorts_systems(cohort_input)
stopifnot(tibble::is_tibble(cohorts), nrow(cohorts) > nrow(cohort_input))
stopifnot("cohort" %in% names(cohorts))
print(cohorts)

cat("\n=== calculate_uncertainty_bounds() ===\n")
unc <- calculate_uncertainty_bounds(full)
stopifnot(tibble::is_tibble(unc), nrow(unc) > 0)
stopifnot(any(grepl("_lower$|_upper$", names(unc))))
print(
  unc |> dplyr::select(species, dplyr::matches("lower|upper"))
)

cat("\n=== All new exported functions ran successfully ===\n")
