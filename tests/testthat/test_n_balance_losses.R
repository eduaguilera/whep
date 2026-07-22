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

testthat::test_that("calculate_nh3 preserves MANNER's organic inorganic-N scaling", {
  x <- tibble::tribble(
    ~n_input_t,
    ~fert_type,
    ~manner_fertiliser,
    ~rainfall_mm,
    ~irrigated,
    ~windspeed_ms,
    ~technique,
    ~system,
    ~temp_c,
    ~incorporation_delay_h,
    ~species,
    10,
    "Liquid",
    "cattle_slurry",
    40,
    FALSE,
    3,
    "Broadcast",
    "Arable",
    15,
    Inf,
    "Cattle"
  )
  out <- whep::calculate_nh3(x, method = "manner")
  direct <- whep::calculate_manner_nh3(
    n_applied_t = 10,
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

  testthat::expect_equal(out$nh3_n_t, direct$nh3_n_t, tolerance = 1e-9)
  testthat::expect_false(isTRUE(all.equal(out$nh3_n_t, direct$ef * 10)))
})

testthat::test_that("calculate_nh3(method = \"manner_default\") aborts when a driver column is missing", {
  x <- tibble::tribble(
    ~n_input_t, ~fert_type, ~manner_fertiliser,
    10, "Solid", "cattle_slurry"
  )
  testthat::expect_error(whep::calculate_nh3(x, method = "manner_default"))
})

testthat::test_that("calculate_nh3(method = \"manner_default\") dispatches without technique/incorporation_delay_h columns", {
  x <- tibble::tribble(
    ~n_input_t,
    ~fert_type,
    ~manner_fertiliser,
    ~rainfall_mm,
    ~irrigated,
    ~windspeed_ms,
    ~system,
    ~temp_c,
    ~species,
    10,
    "Solid",
    "cattle_slurry",
    40,
    FALSE,
    3,
    "Arable",
    15,
    "Cattle"
  )
  # A transform must overwrite a pre-existing result column, not let dplyr's
  # data mask shadow the newly computed local vector.
  x$nh3_n_t <- -999
  testthat::expect_false(rlang::has_name(x, "technique"))
  testthat::expect_false(rlang::has_name(x, "incorporation_delay_h"))

  out <- whep::calculate_nh3(x, method = "manner_default")
  direct <- whep::calculate_manner_nh3_default(
    n_applied_t = 10,
    fertiliser = "cattle_slurry",
    drivers = list(
      rainfall_mm = 40,
      irrigated = FALSE,
      windspeed_ms = 3,
      system = "Arable",
      temp_c = 15,
      species = "Cattle"
    )
  )

  testthat::expect_true(is.finite(out$nh3_n_t))
  testthat::expect_true(out$nh3_n_t > 0)
  testthat::expect_equal(out$nh3_n_t, direct$nh3_n_t, tolerance = 1e-9)
  testthat::expect_equal(out$method_nh3, "manner_default")
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

testthat::test_that("calculate_soil_n2o defaults to the IPCC 2019 Tier 1 method", {
  # The default is the globally applicable IPCC 2019 Tier 1 climate-only EF1
  # (needing only climate), not the Mediterranean-calibrated aguilera.
  x <- tibble::tribble(
    ~n_input_t, ~climate,
    10, "MED"
  )
  out <- whep::calculate_soil_n2o(x)
  testthat::expect_equal(out$method_soil_n2o, "ipcc2019")
  # IPCC 2019 Tier 1 dry-climate EF1 is 0.005 (ten tonnes at 0.005).
  testthat::expect_equal(out$n2o_direct_n_t, 10 * 0.005, tolerance = 1e-9)
})

testthat::test_that("calculate_soil_n2o(method = \"aguilera\") aborts on unsupported ATL irrig_type", {
  x <- tibble::tribble(
    ~n_input_t, ~fert_type, ~climate, ~irrig_type,
    10, "Solid", "ATL", "Drip"
  )
  testthat::expect_error(whep::calculate_soil_n2o(x, method = "aguilera"))
})

testthat::test_that("calculate_soil_n2o aguilera aborts on unsupported MED irrig_type", {
  # Every supported Mediterranean irrigation key has a finite EF. An
  # unmatched key must not disappear in a downstream na.rm sum.
  x <- tibble::tribble(
      ~n_input_t, ~fert_type, ~climate, ~irrig_type,
      10, "Solid", "MED", "not_a_real_irrigation_type"
    )
  testthat::expect_error(whep::calculate_soil_n2o(x, method = "aguilera"))
})

testthat::test_that("calculate_soil_n2o(method = \"aguilera\") aborts on a missing fertiliser modifier", {
  # A fert_type with no fertiliser_n2o_modifiers row (BNF) yields NA mf; the
  # aguilera path must abort rather than multiply ef by NA and let a
  # downstream na.rm sum silently drop the term.
  x <- tibble::tribble(
    ~n_input_t, ~fert_type, ~climate, ~irrig_type,
    10, "BNF", "MED", "Drip"
  )
  testthat::expect_error(whep::calculate_soil_n2o(x, method = "aguilera"))
})

testthat::test_that("calculate_soil_n2o(method = \"aguilera\") never returns a silent NA modifier for MED SOM/Urban/Recycling", {
  # Invariant across the fertiliser_n2o_modifiers CSV state: these MED rows
  # are NA before the CSV fix and 0.00 after it, so the result must be EITHER
  # a clean abort (NA modifier) OR a finite value (0.00 -> 0), never a silent
  # NA n2o_direct_n_t.
  x <- tibble::tribble(
    ~n_input_t, ~fert_type, ~climate, ~irrig_type,
    10, "SOM", "MED", "Drip",
    10, "Urban", "MED", "Drip",
    10, "Recycling", "MED", "Drip"
  )
  out <- tryCatch(
    whep::calculate_soil_n2o(x, method = "aguilera"),
    error = function(e) NULL
  )
  if (!is.null(out)) {
    testthat::expect_false(anyNA(out$n2o_direct_n_t))
  } else {
    testthat::succeed("aborted on NA modifier (pre-CSV-fix state)")
  }
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

testthat::test_that("calculate_soil_n2o(method = \"ipcc2019\") rejects an unknown climate", {
  x <- tibble::tibble(n_input_t = 10, climate = "MDE")
  testthat::expect_error(whep::calculate_soil_n2o(x), "climate")
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

testthat::test_that("calculate_soil_n2o(method = \"ipcc2006\") aborts on an unsupported lookup", {
  x <- tibble::tribble(
    ~n_input_t, ~climate, ~irrig_type,
    10, "MED", "not_a_real_irrigation_type"
  )
  testthat::expect_error(whep::calculate_soil_n2o(x, method = "ipcc2006"))
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
  # Guard against a stale input result shadowing the newly computed local
  # no3_n_t vector inside mutate().
  x$no3_n_t <- -999
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

testthat::test_that("calculate_n_leaching(meisinger_drainage) resolves near-zero drainage to full denitrification for MED and ATL", {
  # Regression for the Meisinger join fan-out: a cell with drainage in the
  # "None" bin (-0.1 < S < 0.1, waterlogged) must resolve to denit_share = 1
  # (whole surplus denitrified) for BOTH climates. The table's "None" rows
  # all carry climate_cat = "Semiarid", so the join must key on climate
  # (MED/ATL), not on a derived Semiarid/Humid climate_cat: keying on the
  # latter fans a MED cell onto two rows (crash) and leaves an ATL cell
  # unmatched (abort).
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
    "Not_specified",
    0.06,
    100,
    "Solid",
    "ATL",
    "Rainfed",
    "Cropland",
    NA_real_,
    "Not_specified",
    0.06
  )
  out <- whep::calculate_n_leaching(
    x,
    drainage_mm = c(0.05, 0.05),
    method = "meisinger_drainage"
  )

  # Full denitrification share routes the whole surplus to denitrification,
  # leaving no3 at zero.
  testthat::expect_equal(out$no3_n_t, c(0, 0), tolerance = 1e-9)
  testthat::expect_equal(out$denitrification_n_t, c(100, 100), tolerance = 1e-9)
  testthat::expect_equal(out$n2o_indirect_no3_n_t, c(0, 0), tolerance = 1e-9)
})

testthat::test_that("calculate_n_leaching(meisinger_drainage) drops a value exactly on a shared drainage edge", {
  # S = 1000 is the shared High/Very_high edge; strictly-open bins match
  # neither (n_fun.r:939), so the row is unmatched and aborts.
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
    "Not_specified",
    0.06
  )
  testthat::expect_error(
    whep::calculate_n_leaching(
      x,
      drainage_mm = 1000,
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

testthat::test_that("calculate_indirect_n2o_nh3 applies EF4 for Atlantic rows without touching the EF lookup", {
  # The ATL branch is a flat nh3 * 0.016 that needs no emission factor or
  # irrig_type column at all.
  x <- tibble::tribble(
    ~nh3_n_t, ~climate, ~fert_type,
    1, "ATL", "Solid"
  )
  out <- whep::calculate_indirect_n2o_nh3(x)

  testthat::expect_equal(out$n2o_indirect_nh3_n_t, 1 * 0.016, tolerance = 1e-9)
})

testthat::test_that("calculate_indirect_n2o_nh3 uses the disaggregated ef (no mf) for Mediterranean rows", {
  # Same Solid / MED / Drip combination as the calculate_soil_n2o aguilera
  # test (ef = 0.0051), but the indirect NH3-N2O term is NH3_MgN * N2O_EF
  # (n_fun.r:955-957): the disaggregated ef ALONE, WITHOUT the fertiliser
  # modifier mf = 0.38 that only applies to the direct-N2O term.
  x <- tibble::tribble(
    ~nh3_n_t, ~climate, ~fert_type, ~irrig_type,
    1, "MED", "Solid", "Drip"
  )
  out <- whep::calculate_indirect_n2o_nh3(x)

  testthat::expect_equal(
    out$n2o_indirect_nh3_n_t,
    1 * 0.0051,
    tolerance = 1e-9
  )
})

testthat::test_that("calculate_indirect_n2o_nh3 aborts on an unsupported MED irrig_type", {
  x <- tibble::tribble(
    ~nh3_n_t, ~climate, ~irrig_type,
    1, "MED", "not_a_real_irrigation_type"
  )
  testthat::expect_error(whep::calculate_indirect_n2o_nh3(x))
})

testthat::test_that("calculate_indirect_n2o_nh3 rejects an unknown climate", {
  x <- tibble::tibble(nh3_n_t = 1, climate = "MDE")
  testthat::expect_error(whep::calculate_indirect_n2o_nh3(x), "climate")
})

testthat::test_that("calculate_indirect_n2o_nh3 example fixture is schema-complete", {
  out <- whep::calculate_indirect_n2o_nh3(example = TRUE)
  pointblank::expect_col_exists(
    out,
    c("nh3_n_t", "climate", "n2o_indirect_nh3_n_t")
  )
})
