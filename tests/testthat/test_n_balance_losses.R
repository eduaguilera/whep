# Tests for R/n_balance_losses.R (Module C, Task C5): calculate_nh3(),
# calculate_soil_n2o(), calculate_n_leaching(), calculate_indirect_n2o_nh3().

# ---- calculate_nh3 -------------------------------------------------------

testthat::test_that("calculate_nh3(method = \"ipcc\") applies the right fraction per fert_type", {
  x <- tibble::tribble(
    ~n_input_t, ~fert_type,
    10, "Synthetic",
    10, "Excreta_cattle_monog",
    10, "Recycling"
  )
  out <- whep::calculate_nh3(x, method = "ipcc")

  testthat::expect_equal(out$nh3_n_t[1], 10 * 0.11)
  testthat::expect_equal(out$nh3_n_t[2], 10 * 0.21)
  testthat::expect_equal(out$nh3_n_t[3], 0)
  testthat::expect_true(all(out$method_nh3 == "ipcc"))
})

testthat::test_that("calculate_nh3(method = \"manner\") aborts when a driver column is missing", {
  x <- tibble::tribble(
    ~n_input_t, ~fert_type, ~manner_fertiliser,
    10, "Synthetic", "Urea"
  )
  testthat::expect_error(whep::calculate_nh3(x, method = "manner"))
})

testthat::test_that("calculate_nh3(method = \"manner\") matches calculate_manner_nh3 directly", {
  x <- tibble::tribble(
    ~n_input_t,
    ~fert_type,
    ~manner_fertiliser,
    ~soil_ph,
    ~rate_kg_ha,
    ~rainfall_mm,
    ~irrigated,
    ~temp_c,
    ~temp_c_annual_mean,
    10,
    "Synthetic",
    "Urea",
    6,
    50,
    0,
    FALSE,
    8.625,
    8.625
  )
  out <- whep::calculate_nh3(x, method = "manner")
  direct <- whep::calculate_manner_nh3(
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

  testthat::expect_true(is.finite(out$nh3_n_t))
  testthat::expect_true(out$nh3_n_t > 0)
  testthat::expect_equal(out$nh3_n_t, direct$nh3_n_t, tolerance = 1e-9)
  testthat::expect_equal(out$method_nh3, "manner")
})

testthat::test_that("calculate_nh3 example fixture is schema-complete", {
  out <- whep::calculate_nh3(example = TRUE)
  pointblank::expect_col_exists(
    out,
    c("n_input_t", "fert_type", "nh3_n_t", "method_nh3")
  )
})

# ---- calculate_soil_n2o --------------------------------------------------

testthat::test_that("calculate_soil_n2o(method = \"aguilera\") matches a hand-computed ef*mf", {
  # Solid / MED / Drip: ef = 0.0051, mf = 0.38.
  x <- tibble::tribble(
    ~n_input_t, ~fert_type, ~climate, ~irrig_type,
    10, "Solid", "MED", "Drip"
  )
  out <- whep::calculate_soil_n2o(x, method = "aguilera")

  expected <- 10 * 0.0051 * 0.38
  testthat::expect_equal(out$n2o_direct_n_t, expected, tolerance = 1e-9)
  testthat::expect_equal(out$method_soil_n2o, "aguilera")
})

testthat::test_that("calculate_soil_n2o(method = \"aguilera\") aborts on unsupported ATL irrig_type", {
  x <- tibble::tribble(
    ~n_input_t, ~fert_type, ~climate, ~irrig_type,
    10, "Solid", "ATL", "Drip"
  )
  testthat::expect_error(whep::calculate_soil_n2o(x, method = "aguilera"))
})

testthat::test_that("calculate_soil_n2o(method = \"ipcc2019\") uses the climate-level rows regardless of irrig_type", {
  x <- tibble::tribble(
    ~n_input_t, ~climate, ~irrig_type,
    10, "MED", "Drip",
    10, "MED", "Flooded",
    10, "ATL", "Tier_1"
  )
  out <- whep::calculate_soil_n2o(x, method = "ipcc2019")

  testthat::expect_equal(out$n2o_direct_n_t[1], 10 * 0.005)
  testthat::expect_equal(out$n2o_direct_n_t[2], 10 * 0.005)
  testthat::expect_equal(out$n2o_direct_n_t[3], 10 * 0.01)
  testthat::expect_true(all(out$method_soil_n2o == "ipcc2019"))

  # The ATL value must equal build_crop_soil_n2o_extension()'s documented
  # EF1 = 0.010 literal (same underlying n2o_efs_disaggregated Tier_1 row).
  testthat::expect_equal(out$n2o_direct_n_t[3] / 10, 0.010)
})

testthat::test_that("calculate_soil_n2o(method = \"ipcc2006\") distinguishes flooded from rainfed MED", {
  x <- tibble::tribble(
    ~n_input_t, ~climate, ~irrig_type,
    10, "MED", "Flooded",
    10, "MED", "Rainfed"
  )
  out <- whep::calculate_soil_n2o(x, method = "ipcc2006")

  testthat::expect_equal(out$n2o_direct_n_t[1], 10 * 0.003)
  testthat::expect_equal(out$n2o_direct_n_t[2], 10 * 0.010)
  testthat::expect_true(all(out$method_soil_n2o == "ipcc2006"))
})

testthat::test_that("calculate_soil_n2o example fixture is schema-complete", {
  out <- whep::calculate_soil_n2o(example = TRUE)
  pointblank::expect_col_exists(
    out,
    c("n_input_t", "climate", "irrig_type", "n2o_direct_n_t", "method_soil_n2o")
  )
})

# ---- calculate_n_leaching ------------------------------------------------

testthat::test_that("calculate_n_leaching(method = \"meisinger_drainage\") matches a hand-computed scenario", {
  # fert_type = Solid (fert_cat = Manure -> tillage join "Not_specified"
  # regardless of x$tillage), climate = MED (climate_cat = "Semiarid"),
  # land_use = Cropland (a_cn_span = 120), cn_input = NA (coalesces to
  # a_cn_min_cn = 15, so a_cn = 0), irrig_cat = Rainfed, som_share = 0.06
  # (> 0.05 -> "High" bin), drainage_mm = 600 (500 < 600 <= 1000 -> "High").
  #
  # Looking up Manure / Not_specified / High / Semiarid / High in
  # meisinger_denitrification gives a denit_share of 0.12, and looking up
  # Solid / MED / Rainfed in subsoil_no3_reduction gives a no3_red of 0.35.
  # The raw denitrified amount is then 100 times 0.12, i.e. 12, so no3_n_t
  # works out to (100 minus 12) times (1 minus 0.35) times (1 minus 0),
  # i.e. 88 times 0.65, i.e. 57.2. The overwritten denitrification_n_t is
  # then 100 minus 57.2, i.e. 42.8, and n2o_indirect_no3_n_t is 57.2 times
  # 0.011, i.e. 0.6292.
  x <- tibble::tribble(
    ~n_surplus_t,
    ~fert_type,
    ~climate,
    ~irrig_cat,
    ~land_use,
    ~cn_input,
    ~tillage,
    ~som_share,
    100,
    "Solid",
    "MED",
    "Rainfed",
    "Cropland",
    NA_real_,
    "No_tillage",
    0.06
  )
  out <- whep::calculate_n_leaching(
    x,
    drainage_mm = 600,
    method = "meisinger_drainage"
  )

  testthat::expect_equal(out$no3_n_t, 57.2, tolerance = 1e-9)
  testthat::expect_equal(out$denitrification_n_t, 42.8, tolerance = 1e-9)
  testthat::expect_equal(out$n2o_indirect_no3_n_t, 0.6292, tolerance = 1e-9)
  testthat::expect_equal(out$method_leaching, "meisinger_drainage")
  testthat::expect_equal(
    out$no3_n_t + out$denitrification_n_t,
    out$n_surplus_t,
    tolerance = 1e-9
  )
})

testthat::test_that("calculate_n_leaching(meisinger_drainage) aborts on No_tillage for synthetic rows", {
  x <- tibble::tribble(
    ~n_surplus_t,
    ~fert_type,
    ~climate,
    ~irrig_cat,
    ~land_use,
    ~cn_input,
    ~tillage,
    ~som_share,
    100,
    "Synthetic",
    "MED",
    "Rainfed",
    "Cropland",
    NA_real_,
    "No_tillage",
    0.06
  )
  testthat::expect_error(
    whep::calculate_n_leaching(
      x,
      drainage_mm = 600,
      method = "meisinger_drainage"
    )
  )
})

testthat::test_that("calculate_n_leaching(meisinger_drainage) aborts on an unmapped fert_type", {
  # "Recycling" is a real fert_type elsewhere in this file's aguilera join
  # but has no row in subsoil_no3_reduction, so it must abort rather than
  # silently propagate NA through no3_n_t/denitrification_n_t.
  x <- tibble::tribble(
      ~n_surplus_t,
      ~fert_type,
      ~climate,
      ~irrig_cat,
      ~land_use,
      ~cn_input,
      ~tillage,
      ~som_share,
      100,
      "Recycling",
      "MED",
      "Rainfed",
      "Cropland",
      NA_real_,
      "Not_specified",
      0.03
    )
  testthat::expect_error(
    whep::calculate_n_leaching(
      x,
      drainage_mm = 600,
      method = "meisinger_drainage"
    )
  )
})

testthat::test_that("calculate_n_leaching(meisinger_drainage) aborts on an out-of-range som_share", {
  x <- tibble::tribble(
      ~n_surplus_t,
      ~fert_type,
      ~climate,
      ~irrig_cat,
      ~land_use,
      ~cn_input,
      ~tillage,
      ~som_share,
      100,
      "Synthetic",
      "MED",
      "Rainfed",
      "Cropland",
      NA_real_,
      "Tillage",
      -0.01
    )
  testthat::expect_error(
    whep::calculate_n_leaching(
      x,
      drainage_mm = 600,
      method = "meisinger_drainage"
    )
  )
})

testthat::test_that("calculate_n_leaching(method = \"ipcc_fracleach\") uses the 0.24 FracLEACH constant", {
  x <- tibble::tribble(~n_surplus_t, 100)
  out <- whep::calculate_n_leaching(x, method = "ipcc_fracleach")

  testthat::expect_equal(out$no3_n_t, 100 * 0.24, tolerance = 1e-9)
  testthat::expect_equal(
    out$denitrification_n_t,
    100 - 100 * 0.24,
    tolerance = 1e-9
  )
  testthat::expect_equal(
    out$no3_n_t + out$denitrification_n_t,
    out$n_surplus_t,
    tolerance = 1e-9
  )
  testthat::expect_equal(
    out$n2o_indirect_no3_n_t,
    100 * 0.24 * 0.011,
    tolerance = 1e-9
  )
  testthat::expect_equal(out$method_leaching, "ipcc_fracleach")
})

testthat::test_that("calculate_n_leaching example fixture is schema-complete", {
  out <- whep::calculate_n_leaching(example = TRUE)
  pointblank::expect_col_exists(
    out,
    c(
      "n_surplus_t",
      "no3_n_t",
      "denitrification_n_t",
      "n2o_indirect_no3_n_t",
      "method_leaching"
    )
  )
})

# ---- calculate_indirect_n2o_nh3 ------------------------------------------

testthat::test_that("calculate_indirect_n2o_nh3 applies EF4 for Atlantic rows", {
  x <- tibble::tribble(
    ~nh3_n_t, ~climate, ~fert_type, ~irrig_type,
    1, "ATL", "Solid", "Tier_1"
  )
  out <- whep::calculate_indirect_n2o_nh3(x)

  testthat::expect_equal(out$n2o_indirect_nh3_n_t, 1 * 0.016, tolerance = 1e-9)
})

testthat::test_that("calculate_indirect_n2o_nh3 reuses the aguilera ef*mf for Mediterranean rows", {
  # Same Solid / MED / Drip combination as the calculate_soil_n2o aguilera
  # test: ef = 0.0051, mf = 0.38.
  x <- tibble::tribble(
    ~nh3_n_t, ~climate, ~fert_type, ~irrig_type,
    1, "MED", "Solid", "Drip"
  )
  out <- whep::calculate_indirect_n2o_nh3(x)

  testthat::expect_equal(
    out$n2o_indirect_nh3_n_t,
    1 * 0.0051 * 0.38,
    tolerance = 1e-9
  )
})

testthat::test_that("calculate_indirect_n2o_nh3 example fixture is schema-complete", {
  out <- whep::calculate_indirect_n2o_nh3(example = TRUE)
  pointblank::expect_col_exists(
    out,
    c("nh3_n_t", "climate", "n2o_indirect_nh3_n_t")
  )
})
