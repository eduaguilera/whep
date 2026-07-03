testthat::test_that("calculate_manner_nh3 matches a hand-computed synthetic ef", {
  # Fertiliser Urea with a soil pH of 6 falls in the "pH<7" class (also the
  # non-calcareous soil type for the rate table, since non-calcareous maps
  # to the same "pH<7" class per MANNER_model.R line 325). Hand-derived
  # factors for this scenario:
  # - the soil-pH factor for Urea at "pH<7" is 1.00
  # - the fertiliser NH3 ceiling for Urea is 0.45
  # - the land-use factor is the fixed synthetic-path constant, 0.70
  # - an application rate of 50 kilograms N per hectare falls in the
  #   "30-60" rate bin (above 30, not above 60), whose Urea/non-calcareous
  #   factor is 0.45285
  # - zero rainfall and no irrigation give zero rain-days, hence the "dry"
  #   wetness class and the "norain" rain level, together the
  #   "noraindry" rainfall class, whose Urea/"pH<7" factor is 0.7
  # - a period temperature equal to the 8.625 degree Celsius reference used
  #   by the Urea/AN temperature response puts the exponential term at its
  #   baseline, so the temperature factor reduces to one third
  # Multiplying every factor above together gives the expected ef.
  expected_ef <- 1.00 * 0.45285 * 0.45 * 0.70 * 0.7 * (1 / 3)
  testthat::expect_equal(expected_ef, 0.033284475, tolerance = 1e-9)

  out <- whep::calculate_manner_nh3(
    n_applied_t = 10,
    fertiliser = "Urea",
    drivers = list(
      soil_ph = 6,
      rate_kg_ha = 50,
      rainfall_mm = 0,
      irrigated = FALSE,
      temp_c = 8.625,
      temp_c_annual_mean = 8.625
    )
  )

  testthat::expect_equal(out$ef, expected_ef, tolerance = 1e-6)
  testthat::expect_equal(out$nh3_n_t, 10 * expected_ef, tolerance = 1e-6)
  testthat::expect_equal(out$method_manner, "manner_synthetic_Urea")
})

testthat::test_that("calculate_manner_nh3 dispatches every fertiliser/manure to the right path", {
  synthetic_drivers <- list(
    soil_ph = 7.5,
    rate_kg_ha = 100,
    rainfall_mm = 40,
    irrigated = FALSE,
    temp_c = 15,
    temp_c_annual_mean = 15
  )
  organic_drivers <- list(
    rainfall_mm = 40,
    irrigated = FALSE,
    windspeed_ms = 3,
    technique = "Broadcast",
    system = "Arable",
    temp_c = 15,
    incorporation_delay_h = Inf,
    species = "Cattle"
  )

  synth_out <- purrr::map(
    c("Urea", "AN", "CAN", "AS"),
    \(f) {
      whep::calculate_manner_nh3(
        n_applied_t = 1,
        fertiliser = f,
        drivers = synthetic_drivers
      )
    }
  ) |>
    purrr::list_rbind()

  pointblank::expect_col_exists(
    synth_out,
    c("n_applied_t", "ef", "nh3_n_t", "method_manner")
  )
  testthat::expect_equal(
    synth_out$method_manner,
    c(
      "manner_synthetic_Urea",
      "manner_synthetic_AN",
      "manner_synthetic_CAN",
      "manner_synthetic_AS"
    )
  )

  organic_out <- purrr::map(
    c("cattle_slurry", "pig_slurry", "FYM", "poultry_manure"),
    \(f) {
      whep::calculate_manner_nh3(
        n_applied_t = 1,
        fertiliser = f,
        drivers = organic_drivers
      )
    }
  ) |>
    purrr::list_rbind()

  pointblank::expect_col_exists(
    organic_out,
    c("n_applied_t", "ef", "nh3_n_t", "method_manner")
  )
  testthat::expect_equal(
    organic_out$method_manner,
    c(
      "manner_organic_cattle_slurry",
      "manner_organic_pig_slurry",
      "manner_organic_FYM",
      "manner_organic_poultry_manure"
    )
  )
})

testthat::test_that("calculate_manner_nh3 clamps the AE climate factor to [0.6, 1.5]", {
  # precip_mm_period = 0 (falls into the "default" <= 15 branch) and
  # temp_c = 0: climate_factor_AE = 0.0431*0 + 1.5936 = 1.5936, which must
  # be clamped down to 1.5.
  out <- whep::calculate_manner_nh3(
    n_applied_t = 1,
    fertiliser = "cattle_slurry",
    drivers = list(
      rainfall_mm = 0,
      irrigated = FALSE,
      windspeed_ms = 3,
      technique = "Broadcast",
      system = "Arable",
      temp_c = 0,
      incorporation_delay_h = Inf,
      species = "Cattle"
    )
  )
  # AG = AE(clamped 1.5) * manure_coef(0.324) * dm_factor(1.0)
  # rain_wet_factor(noraindry) = 1.0, technique(Broadcast) = 1.0,
  # windspeed(nowind, 3 <= 4) = 1.0, system_factor(Arable) = 0.85,
  # incorporation_factor(No incorporation) = 1.0
  expected_ef <- 1.0 * (1.5 * 0.324 * 1.0) * 1.0 * 1.0 * 0.85 * 1.0
  testthat::expect_equal(out$ef, expected_ef, tolerance = 1e-6)
})

testthat::test_that("incorporation delay beyond the largest bin clamps, not zero rows", {
  drivers <- list(
    rainfall_mm = 0,
    irrigated = FALSE,
    windspeed_ms = 3,
    technique = "Broadcast",
    system = "Arable",
    temp_c = 15,
    species = "Cattle"
  )
  beyond_ceiling <- whep::calculate_manner_nh3(
    n_applied_t = 1,
    fertiliser = "cattle_slurry",
    drivers = c(drivers, list(incorporation_delay_h = 20000))
  )
  at_ceiling <- whep::calculate_manner_nh3(
    n_applied_t = 1,
    fertiliser = "cattle_slurry",
    drivers = c(drivers, list(incorporation_delay_h = 10000))
  )
  testthat::expect_equal(nrow(beyond_ceiling), 1L)
  testthat::expect_equal(beyond_ceiling$ef, at_ceiling$ef, tolerance = 1e-6)
})

testthat::test_that("calculate_manner_nh3 applies the FYM 0.4 correction", {
  drivers <- list(
    rainfall_mm = 40,
    irrigated = FALSE,
    windspeed_ms = 3,
    technique = "Broadcast",
    system = "Arable",
    temp_c = 15,
    incorporation_delay_h = Inf,
    species = "Cattle"
  )
  fym_out <- whep::calculate_manner_nh3(
    n_applied_t = 1,
    fertiliser = "FYM",
    drivers = drivers
  )
  cattle_out <- whep::calculate_manner_nh3(
    n_applied_t = 1,
    fertiliser = "cattle_slurry",
    drivers = drivers
  )
  # FYM and cattle_slurry share the same dm_factor (both use the "x6"
  # multiplier, ((8.3*6)+50.2)/100), so it cancels in the ratio: the only
  # differences between the two ef's are the manure_coef
  # (0.683 vs 0.324) and the FYM-only 0.4 correction applied on top.
  expected_ratio <- (0.683 / 0.324) * 0.4
  testthat::expect_equal(
    fym_out$ef,
    cattle_out$ef * expected_ratio,
    tolerance = 1e-6
  )
})

testthat::test_that("calculate_manner_nh3 urban special case fixes inorganic_n_fraction at 0.5 without species", {
  out <- whep::calculate_manner_nh3(
    n_applied_t = 1,
    fertiliser = "urban",
    drivers = list(
      rainfall_mm = 40,
      irrigated = FALSE,
      windspeed_ms = 3,
      technique = "Broadcast",
      system = "Arable",
      temp_c = 15,
      incorporation_delay_h = Inf
    )
  )
  cattle_out <- whep::calculate_manner_nh3(
    n_applied_t = 1,
    fertiliser = "cattle_slurry",
    drivers = list(
      rainfall_mm = 40,
      irrigated = FALSE,
      windspeed_ms = 3,
      technique = "Broadcast",
      system = "Arable",
      temp_c = 15,
      incorporation_delay_h = Inf,
      species = "Cattle"
    )
  )
  # urban uses the same organic Org_ef machinery but a manure_coef/dm_factor
  # of its own is undefined -- instead it borrows nothing from the manure
  # tables besides the inorganic_n_fraction override; the point of this test
  # is only that inorganic_n_fraction = 0.5 is applied without requiring a
  # species driver, so nh3_n_t should be well-defined and finite.
  testthat::expect_equal(out$method_manner, "manner_organic_urban")
  testthat::expect_true(is.finite(out$nh3_n_t))
  testthat::expect_true(out$nh3_n_t > 0)
})

testthat::test_that("calculate_manner_nh3 example fixture is schema-complete", {
  out <- whep::calculate_manner_nh3(example = TRUE)
  pointblank::expect_col_exists(
    out,
    c("n_applied_t", "ef", "nh3_n_t", "method_manner")
  )
  pointblank::expect_col_vals_gte(out, "ef", 0)
})

testthat::test_that("calculate_manner_nh3 aborts on an invalid fertiliser", {
  testthat::expect_error(
    whep::calculate_manner_nh3(
      n_applied_t = 1,
      fertiliser = "not_a_real_fertiliser",
      drivers = list()
    )
  )
})

# ---- calculate_manner_nh3_default -----------------------------------------

testthat::test_that("calculate_manner_nh3_default blends strictly between the No-incorporation and 12-24h bounds", {
  drivers <- list(
    rainfall_mm = 40,
    irrigated = FALSE,
    windspeed_ms = 3,
    system = "Arable",
    temp_c = 15,
    species = "Cattle"
  )
  no_incorporation <- whep::calculate_manner_nh3(
    n_applied_t = 10,
    fertiliser = "cattle_slurry",
    drivers = c(
      drivers,
      list(technique = "Broadcast", incorporation_delay_h = NA)
    )
  )
  within_24h <- whep::calculate_manner_nh3(
    n_applied_t = 10,
    fertiliser = "cattle_slurry",
    drivers = c(
      drivers,
      list(technique = "Broadcast", incorporation_delay_h = 24)
    )
  )
  default_out <- whep::calculate_manner_nh3_default(
    n_applied_t = 10,
    fertiliser = "cattle_slurry",
    drivers = drivers
  )

  lower_bound <- min(no_incorporation$ef, within_24h$ef)
  upper_bound <- max(no_incorporation$ef, within_24h$ef)
  testthat::expect_gt(default_out$ef, lower_bound)
  testthat::expect_lt(default_out$ef, upper_bound)
})

testthat::test_that("calculate_manner_nh3_default matches a hand-computed share-weighted blend", {
  drivers <- list(
    rainfall_mm = 40,
    irrigated = FALSE,
    windspeed_ms = 3,
    system = "Arable",
    temp_c = 15,
    species = "Cattle"
  )
  no_incorporation <- whep::calculate_manner_nh3(
    n_applied_t = 10,
    fertiliser = "cattle_slurry",
    drivers = c(
      drivers,
      list(technique = "Broadcast", incorporation_delay_h = NA)
    )
  )
  within_24h <- whep::calculate_manner_nh3(
    n_applied_t = 10,
    fertiliser = "cattle_slurry",
    drivers = c(
      drivers,
      list(technique = "Broadcast", incorporation_delay_h = 24)
    )
  )
  within_48h <- whep::calculate_manner_nh3(
    n_applied_t = 10,
    fertiliser = "cattle_slurry",
    drivers = c(
      drivers,
      list(technique = "Broadcast", incorporation_delay_h = 48)
    )
  )
  expected_ef <- 0.50 *
    no_incorporation$ef +
    0.25 * within_24h$ef +
    0.25 * within_48h$ef
  expected_nh3_n_t <- 0.50 *
    no_incorporation$nh3_n_t +
    0.25 * within_24h$nh3_n_t +
    0.25 * within_48h$nh3_n_t

  default_out <- whep::calculate_manner_nh3_default(
    n_applied_t = 10,
    fertiliser = "cattle_slurry",
    drivers = drivers
  )

  testthat::expect_equal(default_out$ef, expected_ef, tolerance = 1e-9)
  testthat::expect_equal(
    default_out$nh3_n_t,
    expected_nh3_n_t,
    tolerance = 1e-9
  )
  testthat::expect_equal(default_out$n_applied_t, 10)
})

testthat::test_that("calculate_manner_nh3_default stamps method_manner as manner_default_<fertiliser>", {
  drivers <- list(
    rainfall_mm = 40,
    irrigated = FALSE,
    windspeed_ms = 3,
    system = "Arable",
    temp_c = 15,
    species = "Pigs"
  )
  out <- whep::calculate_manner_nh3_default(
    n_applied_t = 5,
    fertiliser = "pig_slurry",
    drivers = drivers
  )
  testthat::expect_equal(out$method_manner, "manner_default_pig_slurry")
})

testthat::test_that("calculate_manner_nh3_default does not require technique/incorporation_delay_h drivers", {
  drivers <- list(
    rainfall_mm = 40,
    irrigated = FALSE,
    windspeed_ms = 3,
    system = "Arable",
    temp_c = 15,
    species = "Cattle"
  )
  testthat::expect_no_error(
    whep::calculate_manner_nh3_default(
      n_applied_t = 10,
      fertiliser = "cattle_slurry",
      drivers = drivers
    )
  )
})

testthat::test_that("calculate_manner_nh3_default aborts on a synthetic fertiliser", {
  drivers <- list(
    soil_ph = 6.5,
    rate_kg_ha = 100,
    rainfall_mm = 40,
    irrigated = FALSE,
    windspeed_ms = 3,
    system = "Arable",
    temp_c = 15,
    temp_c_annual_mean = 12,
    species = "Cattle"
  )
  testthat::expect_error(
    whep::calculate_manner_nh3_default(
      n_applied_t = 10,
      fertiliser = "Urea",
      drivers = drivers
    ),
    regexp = "cattle_slurry"
  )
})

testthat::test_that("calculate_manner_nh3_default example fixture is schema-complete", {
  out <- whep::calculate_manner_nh3_default(example = TRUE)
  pointblank::expect_col_exists(
    out,
    c("n_applied_t", "ef", "nh3_n_t", "method_manner")
  )
  pointblank::expect_col_vals_gte(out, "ef", 0)
})

testthat::test_that("manner_default_technique_mix has the 3 expected rows summing to 1", {
  mix <- whep::manner_default_technique_mix
  pointblank::expect_col_exists(
    mix,
    c("technique", "delay_bin", "incorporation_delay_h", "share")
  )
  testthat::expect_equal(nrow(mix), 3L)
  testthat::expect_true(all(mix$technique == "Broadcast"))
  testthat::expect_setequal(
    mix$delay_bin,
    c("No incorporation", "12-24 h", "1-2 days")
  )
  testthat::expect_equal(sum(mix$share), 1.0, tolerance = 1e-9)
  testthat::expect_true(is.na(
    mix$incorporation_delay_h[mix$delay_bin == "No incorporation"]
  ))
  testthat::expect_equal(
    mix$incorporation_delay_h[mix$delay_bin == "12-24 h"],
    24
  )
  testthat::expect_equal(
    mix$incorporation_delay_h[mix$delay_bin == "1-2 days"],
    48
  )
})
