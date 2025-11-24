
test_that("Calculated emissions are within reasonable GLEAM/IPCC ranges", {
  
  # Load data
  if (file.exists("../../data/livestock_coefs.rda")) {
    load("../../data/livestock_coefs.rda")
  } else if (file.exists("data/livestock_coefs.rda")) {
    load("data/livestock_coefs.rda")
  } else {
    skip("Coefficient data not found")
  }
  
  # Create a synthetic "Average Global Cow"
  data <- data.frame(
    year = 2010,
    geog_id = "GLO",
    species = "Cattle",
    heads = 1000
  )
  
  # Run Tier 2 calculation
  # We need to manually assemble the call or use the wrapper if it exposes the internal data
  # The wrapper loads data from package env, but we loaded it here.
  # Let's call the internal functions with the loaded data.
  
  # 1. Cohorts
  d_cohorts <- calculate_cohorts_systems(data, gleam_default_cohort_shares, gleam_default_system_shares)
  
  # 2. Energy
  d_energy <- estimate_energy_demand(d_cohorts, ipcc_tier2_energy, livestock_weights, feed_params)
  
  # 3. Emissions
  res <- d_energy |>
    calculate_enteric_ch4(method = "tier2", tier2_coefs = ipcc_tier2_methane, constants = livestock_constants) |>
    calculate_manure_emissions(method = "tier2", tier2_coefs = ipcc_tier2_methane, manure_params = manure_params, feed_data = feed_params, constants = livestock_constants)
  
  # Calculate implied EF (kg CH4/head/year)
  total_enteric <- sum(res$enteric_ch4_tonnes) * 1000
  implied_ef_enteric <- total_enteric / 1000 # heads
  
  # GLEAM/IPCC Global Average for Cattle is roughly 50-70 kg CH4/head/year
  # Our defaults might be slightly different but should be in this order of magnitude.
  expect_gt(implied_ef_enteric, 40)
  expect_lt(implied_ef_enteric, 80)
  
  # Manure CH4
  total_manure_ch4 <- sum(res$manure_ch4_tonnes) * 1000
  implied_ef_manure <- total_manure_ch4 / 1000
  
  # Global average is lower, maybe 1-10 kg depending on system
  expect_gt(implied_ef_manure, 0.5)
  expect_lt(implied_ef_manure, 40)
  
})
