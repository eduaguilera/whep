
test_that("calculate_cohorts_systems works correctly", {
  data <- data.frame(
    year = 2020,
    geog_id = "ESP",
    species = "Cattle",
    heads = 100
  )
  
  cohort_shares <- data.frame(
    species = "Cattle",
    cohort = c("Adult", "Young"),
    share = c(0.6, 0.4)
  )
  
  system_shares <- data.frame(
    species = "Cattle",
    system = c("Grassland", "Mixed"),
    share = c(0.5, 0.5)
  )
  
  res <- calculate_cohorts_systems(data, cohort_shares, system_shares)
  
  expect_equal(nrow(res), 4) # 2 systems * 2 cohorts
  expect_equal(sum(res$heads), 100)
  expect_true(all(c("cohort", "system") %in% names(res)))
})

test_that("estimate_energy_demand calculates GE", {
  data <- data.frame(
    species = "Cattle",
    cohort = "Adult",
    heads = 10,
    weight = 500
  )
  
  energy_coefs <- data.frame(
    species = "Cattle",
    Cfi = 0.386,
    Ca_pasture = 0.17,
    Cp = 0.1
  )
  
  weight_data <- data.frame(
    species = "Cattle",
    cohort = "Adult",
    weight_kg = 500
  )
  
  feed_data <- data.frame(
    species = "Cattle",
    DE_percent = 65,
    UE_frac = 0.04,
    REM_ratio = 0.5
  )
  
  res <- estimate_energy_demand(data, energy_coefs, weight_data, feed_data)
  
  expect_true("GE" %in% names(res))
  expect_true(res$GE[1] > 0)
})

test_that("calculate_enteric_ch4 tier 1 works", {
  data <- data.frame(species = "Cattle", heads = 100)
  tier1_coefs <- data.frame(species = "Cattle", ef_enteric = 55)
  
  res <- calculate_enteric_ch4(data, method = "tier1", tier1_coefs = tier1_coefs)
  
  expect_equal(res$enteric_ch4_tonnes, (100 * 55) / 1000)
})

test_that("calculate_manure_emissions tier 2 works with MMS", {
  data <- data.frame(
    species = "Cattle", 
    system = "Mixed",
    heads = 100, 
    GE = 50
  )
  
  manure_params <- data.frame(species = "Cattle", Nex_kg_head_yr = 50, EF_n2o_direct = 0.005)
  feed_data <- data.frame(species = "Cattle", UE_frac = 0.04, DE_percent = 65, Ash_content = 0.08)
  tier2_coefs <- data.frame(species = "Cattle", Bo = 0.24, MCF_temp = 0.10)
  constants <- list(vs_energy_content = 18.45, days_in_year = 365, ch4_density = 0.67, n2o_n_conversion = 44/28)
  
  mms_shares <- data.frame(
    species = "Cattle",
    system = "Mixed",
    mms = c("Lagoon", "Solid Storage"),
    share = c(0.5, 0.5)
  )
  
  mms_coefs <- data.frame(
    mms = c("Lagoon", "Solid Storage"),
    MCF = c(0.70, 0.05),
    EF_n2o = c(0.00, 0.005)
  )
  
  res <- calculate_manure_emissions(data, method = "tier2", 
                                    tier1_coefs = NULL, 
                                    tier2_coefs = tier2_coefs, 
                                    manure_params = manure_params, 
                                    feed_data = feed_data, 
                                    mms_shares = mms_shares,
                                    mms_coefs = mms_coefs,
                                    constants = constants)
  
  expect_true("manure_ch4_tonnes" %in% names(res))
  expect_true("manure_n2o_tonnes" %in% names(res))
  expect_true(res$manure_ch4_tonnes > 0)
  
  # Check if MMS logic was applied (emissions should be weighted average of MMS)
  # VS = (50 * (1-0.65) + 0.04*50) * ((1-0.08)/18.45) = (17.5 + 2) * 0.0498 = 19.5 * 0.0498 = 0.97
  # EF_ch4 = VS * 365 * 0.24 * 0.67 * MCF_avg
  # MCF_avg = 0.5*0.7 + 0.5*0.05 = 0.375
  # EF_ch4 approx 0.97 * 365 * 0.24 * 0.67 * 0.375 = 21.3
  # Total CH4 = 100 * 21.3 / 1000 = 2.13
  
  # Without MMS (MCF=0.10):
  # EF_ch4 approx 0.97 * 365 * 0.24 * 0.67 * 0.10 = 5.7
  # Total CH4 = 0.57
  
  # So if result is closer to 2.13 than 0.57, MMS worked.
  expect_gt(res$manure_ch4_tonnes, 1.0) 
})
