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

testthat::test_that("calculate_manner_nh3 rejects an unknown land system", {
  drivers <- list(
    rainfall_mm = 0,
    irrigated = FALSE,
    windspeed_ms = 3,
    technique = "Broadcast",
    system = "Arabale",
    temp_c = 15,
    incorporation_delay_h = Inf,
    species = "Cattle"
  )

  testthat::expect_error(
    whep::calculate_manner_nh3(
      n_applied_t = 1,
      fertiliser = "cattle_slurry",
      drivers = drivers
    ),
    "system"
  )
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

testthat::test_that("calculate_manner_nh3 urban maps to the FYM class and fixes inorganic_n_fraction at 0.5 without species", {
  drivers <- list(
    rainfall_mm = 40,
    irrigated = FALSE,
    windspeed_ms = 3,
    technique = "Broadcast",
    system = "Arable",
    temp_c = 15,
    incorporation_delay_h = Inf
  )
  out <- whep::calculate_manner_nh3(
    n_applied_t = 1,
    fertiliser = "urban",
    drivers = drivers
  )
  fym_out <- whep::calculate_manner_nh3(
    n_applied_t = 1,
    fertiliser = "FYM",
    drivers = c(drivers, list(species = "Cattle"))
  )
  cattle_out <- whep::calculate_manner_nh3(
    n_applied_t = 1,
    fertiliser = "cattle_slurry",
    drivers = c(drivers, list(species = "Cattle"))
  )
  # Spain_Hist (N_coefficients.xlsx, Manner_ferts row 43) maps Urban to the
  # FYM MANNER class, so urban shares FYM's AG (manure_coef 0.683), the 0.4
  # Org_ef correction and the FYM incorporation factors: its ef must match
  # the FYM path and must NOT match cattle_slurry's. It keeps its own fixed
  # inorganic_n_fraction = 0.5 override (independent of species), so
  # nh3_n_t = ef * n_applied_t * 0.5 and no species driver is required.
  testthat::expect_equal(out$method_manner, "manner_organic_urban")
  testthat::expect_equal(out$ef, fym_out$ef, tolerance = 1e-9)
  testthat::expect_false(isTRUE(all.equal(out$ef, cattle_out$ef)))
  testthat::expect_equal(out$nh3_n_t, out$ef * 1 * 0.5, tolerance = 1e-9)
})

testthat::test_that("calculate_manner_nh3 FYM inorganic_n_fraction tracks the actual species driver", {
  drivers <- list(
    rainfall_mm = 40,
    irrigated = FALSE,
    windspeed_ms = 3,
    technique = "Broadcast",
    system = "Arable",
    temp_c = 15,
    incorporation_delay_h = Inf
  )
  # Spain_Hist maps every species' solid stream to the FYM MANNER class but
  # looks the ammoniacal fraction up per real species, so nh3_n_t must scale
  # by each species' Solid-stream inorganic_n_fraction (whereas ef, computed
  # before that scaling, stays constant across species). A regression to the
  # old hardcoded Cattle-Solid (0.225) would make every species identical.
  expected_solid_fraction <- c(
    Cattle = 0.225,
    Sheep = 0.2,
    Goats = 0.2,
    Horses = 0.15,
    Donkeys_mules = 0.15,
    Rabbits = 0.15,
    Poultry = 0.325,
    Pigs = 0.275
  )
  outs <- purrr::map(
    names(expected_solid_fraction),
    \(sp) {
      whep::calculate_manner_nh3(
        n_applied_t = 1,
        fertiliser = "FYM",
        drivers = c(drivers, list(species = sp))
      )
    }
  )
  ef_values <- purrr::map_dbl(outs, "ef")
  nh3_values <- purrr::map_dbl(outs, "nh3_n_t")
  testthat::expect_equal(
    diff(range(ef_values)),
    0,
    tolerance = 1e-12
  )
  testthat::expect_equal(
    nh3_values,
    ef_values * unname(expected_solid_fraction),
    tolerance = 1e-9
  )
  # A property that MUST hold once species matters: the two extreme fractions
  # give measurably different loss (guards against silent species drop).
  testthat::expect_gt(
    nh3_values[[which(names(expected_solid_fraction) == "Poultry")]],
    nh3_values[[which(names(expected_solid_fraction) == "Horses")]]
  )
})

testthat::test_that("calculate_manner_nh3 FYM falls back to Cattle Solid when species is omitted", {
  drivers <- list(
    rainfall_mm = 40,
    irrigated = FALSE,
    windspeed_ms = 3,
    technique = "Broadcast",
    system = "Arable",
    temp_c = 15,
    incorporation_delay_h = Inf
  )
  no_species <- whep::calculate_manner_nh3(
    n_applied_t = 1,
    fertiliser = "FYM",
    drivers = drivers
  )
  cattle <- whep::calculate_manner_nh3(
    n_applied_t = 1,
    fertiliser = "FYM",
    drivers = c(drivers, list(species = "Cattle"))
  )
  # A direct caller may omit species; the FYM default species is Cattle, so
  # nh3_n_t must equal the explicit Cattle call (Solid fraction 0.225).
  testthat::expect_equal(no_species$nh3_n_t, cattle$nh3_n_t, tolerance = 1e-9)
  testthat::expect_equal(
    no_species$nh3_n_t,
    no_species$ef * 0.225,
    tolerance = 1e-9
  )
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

testthat::test_that("calculate_manner_nh3_default blends strictly within the 4 blend bins' bounds", {
  drivers <- list(
    rainfall_mm = 40,
    irrigated = FALSE,
    windspeed_ms = 3,
    system = "Arable",
    temp_c = 15,
    species = "Cattle"
  )
  bin_efs <- purrr::map_dbl(
    list(NA, 2, 24, 48),
    \(delay_h) {
      whep::calculate_manner_nh3(
        n_applied_t = 10,
        fertiliser = "cattle_slurry",
        drivers = c(
          drivers,
          list(technique = "Broadcast", incorporation_delay_h = delay_h)
        )
      )$ef
    }
  )
  default_out <- whep::calculate_manner_nh3_default(
    n_applied_t = 10,
    fertiliser = "cattle_slurry",
    drivers = drivers
  )

  testthat::expect_gt(default_out$ef, min(bin_efs))
  testthat::expect_lt(default_out$ef, max(bin_efs))
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
  within_2h <- whep::calculate_manner_nh3(
    n_applied_t = 10,
    fertiliser = "cattle_slurry",
    drivers = c(
      drivers,
      list(technique = "Broadcast", incorporation_delay_h = 2)
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
  expected_ef <- 0.25 *
    no_incorporation$ef +
    0.25 * within_2h$ef +
    0.25 * within_24h$ef +
    0.25 * within_48h$ef
  expected_nh3_n_t <- 0.25 *
    no_incorporation$nh3_n_t +
    0.25 * within_2h$nh3_n_t +
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

testthat::test_that("manner_default_technique_mix has the 4 expected rows summing to 1", {
  mix <- whep::manner_default_technique_mix
  pointblank::expect_col_exists(
    mix,
    c("technique", "delay_bin", "incorporation_delay_h", "share")
  )
  testthat::expect_equal(nrow(mix), 4L)
  testthat::expect_true(all(mix$technique == "Broadcast"))
  testthat::expect_setequal(
    mix$delay_bin,
    c("No incorporation", "<2 h", "12-24 h", "1-2 days")
  )
  testthat::expect_equal(sum(mix$share), 1.0, tolerance = 1e-9)
  testthat::expect_true(all(mix$share == 0.25))
  testthat::expect_true(is.na(
    mix$incorporation_delay_h[mix$delay_bin == "No incorporation"]
  ))
  testthat::expect_equal(
    mix$incorporation_delay_h[mix$delay_bin == "<2 h"],
    2
  )
  testthat::expect_equal(
    mix$incorporation_delay_h[mix$delay_bin == "12-24 h"],
    24
  )
  testthat::expect_equal(
    mix$incorporation_delay_h[mix$delay_bin == "1-2 days"],
    48
  )
})
