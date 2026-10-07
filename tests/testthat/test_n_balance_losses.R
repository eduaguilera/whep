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
  testthat::expect_error(
    whep::calculate_nh3(x, method = "manner"),
    class = "whep_error_nh3_missing_driver"
  )
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
    8.4,
    8.4
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
      temp_c = 8.4,
      temp_c_annual_mean = 8.4
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
  testthat::expect_error(
    whep::calculate_nh3(x, method = "manner_default"),
    class = "whep_error_nh3_missing_driver"
  )
})

testthat::test_that("a missing windspeed_ms names the one reader that supplies it", {
  # whep#1078: the LPJmL wind reader is the only in-package source of the
  # driver and nothing wires it in, so the abort is where a user learns of it.
  x <- tibble::tribble(
    ~n_input_t, ~fert_type, ~manner_fertiliser, ~rainfall_mm, ~irrigated,
    ~system, ~temp_c, ~species,
    10, "Solid", "cattle_slurry", 40, FALSE, "Arable", 15, "Cattle"
  )
  err <- rlang::catch_cnd(
    whep::calculate_nh3(x, method = "manner_default"),
    classes = "whep_error_nh3_missing_driver"
  )
  msg <- cli::ansi_strip(conditionMessage(err))
  testthat::expect_match(msg, "windspeed_ms", fixed = TRUE)
  testthat::expect_match(msg, "read_lpjml_wind", fixed = TRUE)
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

testthat::test_that("aguilera blends the excreta modifier by the species share", {
  # MED / Drip: ef 0.0051, mf 0.20 (cattle/monogastric) and 0.30 (other), so
  # a 0.4 CPP share gives 0.4 * 0.20 + 0.6 * 0.30 = 0.26. ATL / Tier_1: ef
  # 0.010, mf 0.60 and 0.30, so 0.4 * 0.60 + 0.6 * 0.30 = 0.42. A row already
  # labelled by species keeps its own modifier and needs no share.
  x <- tibble::tribble(
    ~n_input_t, ~fert_type, ~climate, ~irrig_type, ~excreta_cpp_share,
    10, "Excreta", "MED", "Drip", 0.4,
    10, "Excreta", "ATL", "Tier_1", 0.4,
    10, "Excreta_other", "MED", "Drip", NA
  )
  out <- whep::calculate_soil_n2o(x, method = "aguilera")

  testthat::expect_equal(
    out$n2o_direct_n_t,
    c(10 * 0.0051 * 0.26, 10 * 0.010 * 0.42, 10 * 0.0051 * 0.30),
    tolerance = 1e-12
  )
  testthat::expect_equal(
    out$method_soil_n2o,
    c("aguilera_excreta_blend", "aguilera_excreta_blend", "aguilera")
  )
})

testthat::test_that("aguilera refuses blended excreta with no species share", {
  x <- tibble::tribble(
    ~n_input_t, ~fert_type, ~climate, ~irrig_type, ~excreta_cpp_share,
    10, "Excreta", "MED", "Drip", NA
  )
  testthat::expect_error(
    whep::calculate_soil_n2o(x, method = "aguilera"),
    class = "whep_excreta_share_missing"
  )
  testthat::expect_error(
    whep::calculate_soil_n2o(
      dplyr::select(x, -"excreta_cpp_share"),
      method = "aguilera"
    ),
    class = "whep_excreta_share_missing"
  )
})

testthat::test_that("calculate_soil_n2o defaults to the IPCC 2019 wet/dry method", {
  x <- tibble::tribble(
    ~n_input_t, ~fert_type, ~item_cbs_code, ~moisture_regime,
    10, "Synthetic", 2511L, "dry"
  )
  out <- whep::calculate_soil_n2o(x)
  testthat::expect_equal(out$method_soil_n2o, "ipcc2019_wet_dry")
  # IPCC 2019 Table 11.1 dry-climate EF1 is 0.005 for all N inputs.
  testthat::expect_equal(out$n2o_direct_n_t, 10 * 0.005, tolerance = 1e-12)
})

testthat::test_that("ipcc2019 splits EF1 into synthetic and other N in wet climates only", {
  # Table 11.1 (Updated), p. 11.13: wet 0.016 synthetic, 0.006 other N
  # inputs; dry 0.005 for all N inputs.
  other <- c("Solid", "Liquid", "Human", "Recycling", "SOM")
  x <- tidyr::expand_grid(
    moisture_regime = c("wet", "dry"),
    fert_type = c("Synthetic", other)
  ) |>
    dplyr::mutate(n_input_t = 10, item_cbs_code = 2511L)
  out <- whep::calculate_soil_n2o(x, method = "ipcc2019")
  ef <- out$n2o_direct_n_t / out$n_input_t
  wet <- out$moisture_regime == "wet"
  testthat::expect_equal(ef[wet & out$fert_type == "Synthetic"], 0.016)
  testthat::expect_equal(
    ef[wet & out$fert_type != "Synthetic"],
    rep(0.006, length(other))
  )
  testthat::expect_equal(ef[!wet], rep(0.005, length(other) + 1L))
  testthat::expect_true(all(out$method_soil_n2o == "ipcc2019_wet_dry"))
})

testthat::test_that("ipcc2019 prices every rice row with EF1FR in both regimes", {
  # Table 11.1: EF1FR 0.004 (aggregated). Rice is every CBS item of the
  # production items classed `Crop_irrig == "Rice"`: 2807, rice and products.
  testthat::expect_identical(whep:::.n_rice_item_cbs_codes(), 2807L)
  x <- tidyr::expand_grid(
    moisture_regime = c("wet", "dry"),
    water_regime = c("rainfed", "irrigated"),
    fert_type = c("Synthetic", "Solid", "Recycling")
  ) |>
    dplyr::mutate(n_input_t = 10, item_cbs_code = 2807L)
  out <- whep::calculate_soil_n2o(x, method = "ipcc2019")
  testthat::expect_equal(out$n2o_direct_n_t, rep(10 * 0.004, nrow(x)))
  testthat::expect_true(all(out$method_soil_n2o == "ipcc2019_ef1fr"))
})

testthat::test_that("ipcc2019 blends EF3PRP by the CPP share, worked by hand", {
  # Table 11.1: EF3PRP,CPP 0.006 wet, 0.002 dry; EF3PRP,SO 0.003.
  # Row 1, wet, share 0.25: 0.25 * 0.006 + 0.75 * 0.003 = 0.00375.
  # Row 2, dry, share 0.8: 0.8 * 0.002 + 0.2 * 0.003 = 0.0022.
  x <- tibble::tribble(
    ~n_input_t, ~fert_type, ~item_cbs_code, ~moisture_regime, ~excreta_cpp_share,
            40, "Excreta",         3000L,             "wet",               0.25,
            20, "Excreta",         3000L,             "dry",                0.8
  )
  out <- whep::calculate_soil_n2o(x, method = "ipcc2019")
  testthat::expect_equal(
    out$n2o_direct_n_t,
    c(40 * 0.00375, 20 * 0.0022),
    tolerance = 1e-12
  )
  testthat::expect_true(all(out$method_soil_n2o == "ipcc2019_ef3prp_blend"))
})

testthat::test_that("ipcc2019 keeps excreta on EF3PRP even on a rice row", {
  # Equation 11.1 prices F_PRP with EF3PRP whatever the crop; EF1FR covers
  # the N applied to flooded rice (synthetic, organic, residue, SOM).
  x <- tibble::tribble(
    ~n_input_t, ~fert_type, ~item_cbs_code, ~moisture_regime, ~excreta_cpp_share,
            10, "Excreta",         2807L,             "wet",                  1
  )
  out <- whep::calculate_soil_n2o(x, method = "ipcc2019")
  testthat::expect_equal(out$n2o_direct_n_t, 10 * 0.006, tolerance = 1e-12)
  testthat::expect_equal(out$method_soil_n2o, "ipcc2019_ef3prp_blend")
})

testthat::test_that("ipcc2019 refuses excreta N with no species share", {
  x <- tibble::tribble(
    ~n_input_t, ~fert_type, ~item_cbs_code, ~moisture_regime, ~excreta_cpp_share,
            10, "Excreta",         3000L,             "wet",                 NA,
             5, "Synthetic",       2511L,             "wet",                 NA
  )
  err <- testthat::expect_error(
    whep::calculate_soil_n2o(x, method = "ipcc2019"),
    class = "whep_excreta_share_missing"
  )
  testthat::expect_s3_class(err, "whep_absent_input")
  testthat::expect_match(cli::ansi_strip(conditionMessage(err)), "1 excreta")
  testthat::expect_error(
    whep::calculate_soil_n2o(
      dplyr::select(x, -"excreta_cpp_share"),
      method = "ipcc2019"
    ),
    class = "whep_excreta_share_missing"
  )
})

testthat::test_that("ipcc2019 refuses an excreta share outside [0, 1]", {
  x <- tibble::tribble(
    ~n_input_t, ~fert_type, ~item_cbs_code, ~moisture_regime, ~excreta_cpp_share,
            10, "Excreta",         3000L,             "wet",                1.2
  )
  testthat::expect_error(
    whep::calculate_soil_n2o(x, method = "ipcc2019"),
    class = "whep_excreta_share_invalid"
  )
})

testthat::test_that("ipcc2019 emits nothing from an excreta row with no N", {
  # A row with no excreta N has no mix to know (method_excreta_species
  # "no_excreta_n"), and zero N emits zero whatever the mix.
  x <- tibble::tribble(
    ~n_input_t, ~fert_type, ~item_cbs_code, ~moisture_regime, ~excreta_cpp_share,
             0, "Excreta",         3000L,             "dry",                 NA
  )
  out <- whep::calculate_soil_n2o(x, method = "ipcc2019")
  testthat::expect_identical(out$n2o_direct_n_t, 0)
})

testthat::test_that("ipcc2019 refuses an absent moisture regime", {
  x <- tibble::tribble(
    ~n_input_t, ~fert_type, ~item_cbs_code,
    10, "Synthetic", 2511L
  )
  err <- testthat::expect_error(
    whep::calculate_soil_n2o(x, method = "ipcc2019"),
    class = "whep_missing_moisture_regime"
  )
  testthat::expect_s3_class(err, "whep_absent_input")
  # The abort names the ATL/MED variant, which needs no moisture regime.
  testthat::expect_match(conditionMessage(err), "ipcc2019_atl_med")
})

testthat::test_that("ipcc2019 names the count of rows with no moisture regime", {
  x <- tibble::tribble(
    ~n_input_t, ~fert_type, ~item_cbs_code, ~moisture_regime,
    10, "Synthetic", 2511L, NA,
    10, "Solid", 2511L, NA,
    10, "Solid", 2511L, "wet"
  )
  err <- testthat::expect_error(
    whep::calculate_soil_n2o(x, method = "ipcc2019"),
    class = "whep_moisture_regime_na"
  )
  testthat::expect_s3_class(err, "whep_absent_input")
  testthat::expect_match(cli::ansi_strip(conditionMessage(err)), "2 rows")
})

testthat::test_that("ipcc2019 refuses a moisture regime outside wet/dry", {
  x <- tibble::tribble(
    ~n_input_t, ~fert_type, ~item_cbs_code, ~moisture_regime,
    10, "Synthetic", 2511L, "humid"
  )
  testthat::expect_error(
    whep::calculate_soil_n2o(x, method = "ipcc2019"),
    class = "whep_moisture_regime_value"
  )
})

testthat::test_that("ipcc2019 refuses an input it has no direct factor for", {
  # BNF and deposition are not direct N2O sources in Equation 11.1.
  x <- tibble::tribble(
    ~n_input_t, ~fert_type, ~item_cbs_code, ~moisture_regime,
    10, "BNF", 2511L, "wet"
  )
  testthat::expect_error(
    whep::calculate_soil_n2o(x, method = "ipcc2019"),
    class = "whep_soil_n2o_fert_type"
  )
  testthat::expect_error(
    whep::calculate_soil_n2o(
      dplyr::select(x, -"item_cbs_code"),
      method = "ipcc2019"
    ),
    class = "whep_soil_n2o_columns"
  )
})

testthat::test_that("ipcc2019_atl_med reproduces the former ipcc2019 output", {
  # Golden values from `calculate_soil_n2o(method = "ipcc2019")` at commit
  # bb604f8b, before the wet/dry method took the name: ATL 0.010, MED 0.005.
  x <- tidyr::expand_grid(
    climate = c("ATL", "MED"),
    fert_type = c(
      "Synthetic",
      "Solid",
      "Liquid",
      "Excreta_other",
      "Human",
      "Recycling",
      "SOM"
    )
  ) |>
    dplyr::mutate(
      n_input_t = seq_len(dplyr::n()) * 3.7,
      irrig_type = dplyr::if_else(.data$climate == "ATL", "Tier_1", "Drip")
    )
  golden <- c(
    0.037000000000000005,
    0.07400000000000001,
    0.11100000000000002,
    0.14800000000000002,
    0.185,
    0.22200000000000003,
    0.25900000000000001,
    0.14800000000000002,
    0.16650000000000004,
    0.185,
    0.20350000000000001,
    0.22200000000000003,
    0.24050000000000002,
    0.25900000000000001
  )
  out <- whep::calculate_soil_n2o(x, method = "ipcc2019_atl_med")
  testthat::expect_identical(out$n2o_direct_n_t, golden)
  testthat::expect_true(all(out$method_soil_n2o == "ipcc2019_atl_med"))
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

testthat::test_that("calculate_soil_n2o(method = \"aguilera\") never returns a silent NA modifier for MED SOM/Human/Recycling", {
  # Invariant across the fertiliser_n2o_modifiers CSV state: these MED rows
  # are NA before the CSV fix and 0.00 after it, so the result must be EITHER
  # a clean abort (NA modifier) OR a finite value (0.00 -> 0), never a silent
  # NA n2o_direct_n_t.
  x <- tibble::tribble(
    ~n_input_t, ~fert_type, ~climate, ~irrig_type,
    10, "SOM", "MED", "Drip",
    10, "Human", "MED", "Drip",
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

testthat::test_that("calculate_soil_n2o(method = \"ipcc2019_atl_med\") uses the climate-level rows regardless of irrig_type", {
  x <- tibble::tribble(
    ~n_input_t, ~climate, ~irrig_type,
    10, "MED", "Drip",
    10, "MED", "Flooded",
    10, "ATL", "Tier_1"
  )
  out <- whep::calculate_soil_n2o(x, method = "ipcc2019_atl_med")

  testthat::expect_equal(out$n2o_direct_n_t[1], 10 * 0.005)
  testthat::expect_equal(out$n2o_direct_n_t[2], 10 * 0.005)
  testthat::expect_equal(out$n2o_direct_n_t[3], 10 * 0.01)
  testthat::expect_true(all(out$method_soil_n2o == "ipcc2019_atl_med"))

  # The ATL value must equal build_crop_soil_n2o_extension()'s documented
  # EF1 = 0.010 literal (same underlying n2o_efs_disaggregated Tier_1 row).
  testthat::expect_equal(out$n2o_direct_n_t[3] / 10, 0.010)
})

testthat::test_that("calculate_soil_n2o(method = \"ipcc2019_atl_med\") rejects an unknown climate", {
  x <- tibble::tibble(n_input_t = 10, climate = "MDE")
  testthat::expect_error(
    whep::calculate_soil_n2o(x, method = "ipcc2019_atl_med"),
    "climate"
  )
})

testthat::test_that("calculate_soil_n2o(method = \"ipcc2019_atl_med\") aborts on a missing climate column", {
  x <- tibble::tibble(n_input_t = 10)
  testthat::expect_error(
    whep::calculate_soil_n2o(x, method = "ipcc2019_atl_med"),
    class = "whep_missing_climate"
  )
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
    c(
      "n_input_t",
      "fert_type",
      "item_cbs_code",
      "moisture_regime",
      "n2o_direct_n_t",
      "method_soil_n2o"
    )
  )
  # The fixture is what the default method returns for its own inputs.
  inputs <- dplyr::select(out, -"n2o_direct_n_t", -"method_soil_n2o")
  testthat::expect_equal(whep::calculate_soil_n2o(inputs), out)
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
  # neither, so the row is unmatched and aborts.
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

testthat::test_that("calculate_n_leaching(method = \"ipcc_fracleach\") applies FracLEACH-(H) by wet/dry class", {
  # Section 11.2.2.2, p. 11.25: FracLEACH-(H) 0.24 in wet climates and in
  # dry climates under irrigation other than drip (all irrigation counts as
  # non-drip), zero in dry climates otherwise. EF5 0.011 (Table 11.3).
  x <- tibble::tribble(
    ~n_surplus_t, ~moisture_regime, ~water_regime,
    100, "wet", "rainfed",
    100, "wet", "irrigated",
    100, "dry", "irrigated",
    100, "dry", "rainfed"
  )
  out <- whep::calculate_n_leaching(x, method = "ipcc_fracleach")

  testthat::expect_equal(out$no3_n_t, c(24, 24, 24, 0), tolerance = 1e-12)
  testthat::expect_equal(
    out$denitrification_n_t,
    c(76, 76, 76, 100),
    tolerance = 1e-12
  )
  testthat::expect_equal(
    out$no3_n_t + out$denitrification_n_t,
    out$n_surplus_t,
    tolerance = 1e-12
  )
  testthat::expect_equal(
    out$n2o_indirect_no3_n_t,
    c(24, 24, 24, 0) * 0.011,
    tolerance = 1e-12
  )
  testthat::expect_equal(
    out$method_leaching,
    c(
      "ipcc_fracleach_wet",
      "ipcc_fracleach_wet",
      "ipcc_fracleach_dry_irrigated",
      "ipcc_fracleach_dry_rainfed"
    )
  )
})

testthat::test_that("ipcc_fracleach needs no water regime on wet rows", {
  x <- tibble::tribble(~n_surplus_t, ~moisture_regime, 100, "wet")
  out <- whep::calculate_n_leaching(x, method = "ipcc_fracleach")
  testthat::expect_equal(out$no3_n_t, 24, tolerance = 1e-12)
})

testthat::test_that("ipcc_fracleach refuses a dry row whose water regime is unknown", {
  # Dry irrigated and dry rainfed land leach 0.24 and 0 of their surplus, so
  # a dry row without a water regime has no defensible fraction.
  x <- tibble::tribble(~n_surplus_t, ~moisture_regime, 100, "dry")
  err <- testthat::expect_error(
    whep::calculate_n_leaching(x, method = "ipcc_fracleach"),
    class = "whep_missing_water_regime"
  )
  testthat::expect_s3_class(err, "whep_absent_input")
  x_na <- tibble::tribble(
    ~n_surplus_t, ~moisture_regime, ~water_regime,
    100, "dry", NA,
    100, "wet", NA
  )
  err_na <- testthat::expect_error(
    whep::calculate_n_leaching(x_na, method = "ipcc_fracleach"),
    class = "whep_water_regime_value"
  )
  testthat::expect_match(cli::ansi_strip(conditionMessage(err_na)), "1 dry")
})

testthat::test_that("ipcc_fracleach refuses an absent or incomplete moisture regime", {
  x <- tibble::tribble(~n_surplus_t, 100)
  testthat::expect_error(
    whep::calculate_n_leaching(x, method = "ipcc_fracleach"),
    class = "whep_missing_moisture_regime"
  )
  x_na <- tibble::tribble(~n_surplus_t, ~moisture_regime, 100, NA)
  testthat::expect_error(
    whep::calculate_n_leaching(x_na, method = "ipcc_fracleach"),
    class = "whep_moisture_regime_na"
  )
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
  inputs <- dplyr::select(out, "n_surplus_t", "moisture_regime")
  testthat::expect_equal(
    whep::calculate_n_leaching(inputs, method = "ipcc_fracleach"),
    out
  )
})

# ---- calculate_indirect_n2o_nh3 ------------------------------------------

testthat::test_that("calculate_indirect_n2o_nh3 defaults to EF4 by wet/dry class", {
  # Table 11.3 (Updated), p. 11.26: EF4 0.014 wet, 0.005 dry.
  x <- tibble::tribble(
    ~nh3_n_t, ~moisture_regime,
    2, "wet",
    3, "dry"
  )
  out <- whep::calculate_indirect_n2o_nh3(x)
  testthat::expect_equal(
    out$n2o_indirect_nh3_n_t,
    c(2 * 0.014, 3 * 0.005),
    tolerance = 1e-12
  )
  testthat::expect_true(all(out$method_n2o_indirect_nh3 == "ipcc2019_wet_dry"))
})

testthat::test_that("calculate_indirect_n2o_nh3 refuses an absent or incomplete moisture regime", {
  testthat::expect_error(
    whep::calculate_indirect_n2o_nh3(tibble::tibble(nh3_n_t = 1)),
    class = "whep_missing_moisture_regime"
  )
  testthat::expect_error(
    whep::calculate_indirect_n2o_nh3(
      tibble::tibble(nh3_n_t = 1, moisture_regime = NA_character_)
    ),
    class = "whep_moisture_regime_na"
  )
  testthat::expect_error(
    whep::calculate_indirect_n2o_nh3(tibble::tibble(nh3_n_t = 1), method = "x")
  )
})

testthat::test_that("calculate_indirect_n2o_nh3 ipcc2019_atl_med reproduces the former output", {
  # Golden values from `calculate_indirect_n2o_nh3()` at commit bb604f8b,
  # before the method argument existed: ATL flat EF4 0.014, MED the
  # n2o_efs_disaggregated ef on irrig_type.
  x <- tibble::tribble(
    ~nh3_n_t, ~climate, ~irrig_type,
         1.3,    "ATL",    "Tier_1",
         2.9,    "ATL",          NA,
         4.1,    "MED",   "Rainfed",
         5.3,    "MED", "Traditional",
         6.7,    "MED",      "Drip",
         7.9,    "MED", "Sprinkler",
         8.3,    "MED",   "Flooded",
         9.7,    "MED", "Med_average"
  )
  golden <- c(
    0.018200000000000001,
    0.040599999999999997,
    0.01107,
    0.024910000000000002,
    0.034170000000000006,
    0.071890000000000009,
    0.015770000000000003,
    0.048499999999999995
  )
  out <- whep::calculate_indirect_n2o_nh3(x, method = "ipcc2019_atl_med")
  testthat::expect_identical(out$n2o_indirect_nh3_n_t, golden)
  testthat::expect_true(all(out$method_n2o_indirect_nh3 == "ipcc2019_atl_med"))
})

testthat::test_that("calculate_indirect_n2o_nh3 applies EF4 for Atlantic rows", {
  x <- tibble::tribble(
    ~nh3_n_t, ~climate, ~fert_type, ~irrig_type,
    1, "ATL", "Solid", "Tier_1"
  )
  out <- whep::calculate_indirect_n2o_nh3(x, method = "ipcc2019_atl_med")

  testthat::expect_equal(out$n2o_indirect_nh3_n_t, 1 * 0.014, tolerance = 1e-9)
})

testthat::test_that("calculate_indirect_n2o_nh3 applies EF4 for Atlantic rows without touching the EF lookup", {
  # The ATL branch is a flat nh3 * 0.014 that needs no emission factor or
  # irrig_type column at all.
  x <- tibble::tribble(
    ~nh3_n_t, ~climate, ~fert_type,
    1, "ATL", "Solid"
  )
  out <- whep::calculate_indirect_n2o_nh3(x, method = "ipcc2019_atl_med")

  testthat::expect_equal(out$n2o_indirect_nh3_n_t, 1 * 0.014, tolerance = 1e-9)
})

testthat::test_that("calculate_indirect_n2o_nh3 uses the disaggregated ef (no mf) for Mediterranean rows", {
  # Same Solid / MED / Drip combination as the calculate_soil_n2o aguilera test
  # (ef = 0.0051), but the indirect NH3-N2O term is NH3_MgN * N2O_EF as in the
  # source implementation: the disaggregated ef ALONE, WITHOUT the fertiliser
  # modifier mf = 0.38 that only applies to the direct-N2O term.
  x <- tibble::tribble(
    ~nh3_n_t, ~climate, ~fert_type, ~irrig_type,
    1, "MED", "Solid", "Drip"
  )
  out <- whep::calculate_indirect_n2o_nh3(x, method = "ipcc2019_atl_med")

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
  testthat::expect_error(
    whep::calculate_indirect_n2o_nh3(x, method = "ipcc2019_atl_med")
  )
})

testthat::test_that("calculate_indirect_n2o_nh3 rejects an unknown climate", {
  x <- tibble::tibble(nh3_n_t = 1, climate = "MDE")
  testthat::expect_error(
    whep::calculate_indirect_n2o_nh3(x, method = "ipcc2019_atl_med"),
    "climate"
  )
})

testthat::test_that("calculate_indirect_n2o_nh3 aborts on a missing climate column", {
  x <- tibble::tibble(nh3_n_t = 1)
  testthat::expect_error(
    whep::calculate_indirect_n2o_nh3(x, method = "ipcc2019_atl_med"),
    class = "whep_missing_climate"
  )
})

testthat::test_that("calculate_indirect_n2o_nh3 example fixture is schema-complete", {
  out <- whep::calculate_indirect_n2o_nh3(example = TRUE)
  pointblank::expect_col_exists(
    out,
    c(
      "nh3_n_t",
      "moisture_regime",
      "n2o_indirect_nh3_n_t",
      "method_n2o_indirect_nh3"
    )
  )
  inputs <- dplyr::select(out, "nh3_n_t", "moisture_regime")
  testthat::expect_equal(whep::calculate_indirect_n2o_nh3(inputs), out)
})

testthat::test_that("the former Urban key gets the Human coefficients, with a warning", {
  # "Urban" was renamed "Human". A table built before the rename must neither
  # lose its modifier (NA, then an abort) nor get a different one.
  human <- tibble::tribble(
    ~n_input_t, ~fert_type, ~climate, ~irrig_type, ~irrig_cat,
    10, "Human", "MED", "Drip", "Irrigated",
    10, "Human", "ATL", "Tier_1", "Rainfed"
  )
  legacy <- dplyr::mutate(human, fert_type = "Urban")
  testthat::expect_warning(
    old_mf <- whep:::.soil_n2o_ef_mf_aguilera(legacy),
    class = "whep_urban_fert_type_deprecated"
  )
  testthat::expect_identical(old_mf, whep:::.soil_n2o_ef_mf_aguilera(human))
  testthat::expect_false(anyNA(old_mf))
  testthat::expect_warning(
    old_red <- whep:::.leaching_no3_red(legacy),
    class = "whep_urban_fert_type_deprecated"
  )
  testthat::expect_identical(old_red, whep:::.leaching_no3_red(human))
  testthat::expect_equal(old_red, c(0.65, 0.70))
})
