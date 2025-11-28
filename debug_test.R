# Debug script
devtools::load_all(".")

# Load data
if (file.exists("data/livestock_coefs.rda")) {
  load("data/livestock_coefs.rda")
} else {
  stop("Data not found")
}

# Create test data
test_data <- tibble::tibble(
  year = 2020,
  geog_id = "USA",
  iso3 = "USA",
  species = "Dairy Cattle",
  heads = 1000
)

print("Running Tier 1...")
res_t1 <- calculate_livestock_emissions(test_data, method = "tier1")
print(res_t1)

print("Running Tier 2...")
res_t2 <- calculate_livestock_emissions(test_data, method = "tier2")
print(res_t2)

print("Checking Energy Demand...")
data_energy <- estimate_energy_demand(test_data |> dplyr::mutate(cohort = "Adult Female"))
print(head(data_energy))

print("Checking Dynamic Calculations...")
# High Quality Diet
data_high <- test_data |> 
  dplyr::mutate(cohort = "Adult Female", diet_quality = "High") |>
  estimate_energy_demand()
  
# Low Quality Diet
data_low <- test_data |> 
  dplyr::mutate(cohort = "Adult Female", diet_quality = "Low") |>
  estimate_energy_demand()
  
print(paste("High DE:", data_high$DE_percent))
print(paste("Low DE:", data_low$DE_percent))
print(paste("High REM:", data_high$REM))
print(paste("Low REM:", data_low$REM))

res_high <- calculate_enteric_ch4(data_high, method = "tier2")
res_low <- calculate_enteric_ch4(data_low, method = "tier2")

print(paste("High Ym:", res_high$Ym))
print(paste("Low Ym:", res_low$Ym))
print(paste("High EF:", res_high$ef_enteric_tier2))
print(paste("Low EF:", res_low$ef_enteric_tier2))

if (res_high$ef_enteric_tier2 != res_low$ef_enteric_tier2) {
  print("SUCCESS: Dynamic calculations produce different results.")
} else {
  print("FAILURE: Dynamic calculations produce same results.")
}
