test_that("Livestock emissions calculation works without hardcoded values", {
  
  # Load data
  # Load data
  if (file.exists("data/livestock_coefs.rda")) {
    load("data/livestock_coefs.rda")
  } else {
    load(system.file("data/livestock_coefs.rda", package = "whep"))
  }
  
  # Create test data
  test_data <- tibble::tibble(
    year = 2020,
    geog_id = "USA",
    iso3 = "USA",
    species = "Dairy Cattle",
    heads = 1000
  )
  
  # Run Tier 1
  res_t1 <- calculate_livestock_emissions(test_data, method = "tier1")
  
  expect_true(nrow(res_t1) == 1)
  expect_true(!is.na(res_t1$enteric_ch4_tier1))
  expect_true(!is.na(res_t1$manure_ch4_tier1))
  expect_true(!is.na(res_t1$manure_n2o_tier1))
  
  # Run Tier 2
  res_t2 <- calculate_livestock_emissions(test_data, method = "tier2")
  
  expect_true(nrow(res_t2) == 1)
  expect_true(!is.na(res_t2$enteric_ch4_tier2))
  expect_true(!is.na(res_t2$manure_ch4_tier2))
  expect_true(!is.na(res_t2$manure_n2o_tier2))
  
  # Check that values are reasonable (non-zero)
  expect_gt(res_t2$enteric_ch4_tier2, 0)
  expect_gt(res_t2$manure_ch4_tier2, 0)
  
  # Check that weight was joined correctly
  data_energy <- estimate_energy_demand(test_data |> dplyr::mutate(cohort = "Adult Female"))
  expect_true(!is.na(data_energy$weight))
  expect_gt(data_energy$weight, 500)
  
  # Test Dynamic Calculations (Phase 2)
  # High Quality Diet
  data_high <- test_data |> 
    dplyr::mutate(cohort = "Adult Female", diet_quality = "High") |>
    estimate_energy_demand()
    
  # Low Quality Diet
  data_low <- test_data |> 
    dplyr::mutate(cohort = "Adult Female", diet_quality = "Low") |>
    estimate_energy_demand()
    
  # Check DE and REM differences
  expect_gt(data_high$DE_percent, data_low$DE_percent)
  expect_gt(data_high$REM, data_low$REM)
  
  # Check Emissions differences
  res_high <- calculate_enteric_ch4(data_high, method = "tier2")
  res_low <- calculate_enteric_ch4(data_low, method = "tier2")
  
  # Ym should be different
  expect_false(res_high$ef_enteric_tier2 == res_low$ef_enteric_tier2)
  
})
