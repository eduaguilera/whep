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

testthat::test_that("ipcc2019 prices fallback-labelled excreta as applied manure", {
  # Manure labelled excreta only as the fallback of a manure-type split is
  # collected manure, not grazing deposition: EF1 for other N inputs, 0.006
  # wet and 0.005 dry (Table 11.1, p. 11.13), whatever its missing mix, and
  # EF1FR on a rice item like any other applied input.
  x <- tibble::tribble(
    ~n_input_t, ~fert_type, ~item_cbs_code, ~moisture_regime,
    ~excreta_cpp_share, ~method_excreta_species,
    10, "Excreta", 2511L, "wet", NA, "fallback_label_no_species",
    10, "Excreta", 2511L, "dry", NA, "fallback_label_no_species",
    10, "Excreta", 2807L, "wet", NA, "fallback_label_no_species",
    10, "Excreta", 3000L, "wet", 0.5, "engine_livestock_category"
  )
  out <- whep::calculate_soil_n2o(x, method = "ipcc2019")
  testthat::expect_equal(
    out$n2o_direct_n_t,
    c(0.06, 0.05, 0.04, 10 * (0.5 * 0.006 + 0.5 * 0.003)),
    tolerance = 1e-12
  )
  testthat::expect_identical(
    out$method_soil_n2o,
    c(
      rep("ipcc2019_fallback_applied_manure", 3L),
      "ipcc2019_ef3prp_blend"
    )
  )
})

testthat::test_that("aguilera prices fallback-labelled excreta as Solid", {
  # Collected manure with no type split is applied manure, not grazing
  # deposition: it takes the Solid modifier, 0.38 on MED and 0.60 on ATL.
  x <- tibble::tribble(
    ~n_input_t, ~fert_type, ~climate, ~irrig_type, ~excreta_cpp_share,
    ~method_excreta_species,
    10, "Excreta", "MED", "Rainfed", NA, "fallback_label_no_species",
    10, "Excreta", "ATL", "Tier_1", NA, "fallback_label_no_species",
    10, "Solid", "MED", "Rainfed", NA, NA,
    10, "Solid", "ATL", "Tier_1", NA, NA
  )
  out <- whep::calculate_soil_n2o(x, method = "aguilera")
  # MED / Rainfed ef 0.0027 x mf 0.38; ATL / Tier_1 ef 0.010 x mf 0.60.
  testthat::expect_equal(
    out$n2o_direct_n_t[1:2],
    c(10 * 0.0027 * 0.38, 10 * 0.010 * 0.60),
    tolerance = 1e-12
  )
  # The same emission as the Solid rows of that climate and irrigation type.
  testthat::expect_equal(out$n2o_direct_n_t[1:2], out$n2o_direct_n_t[3:4])
  testthat::expect_true(all(
    out$method_soil_n2o[1:2] == "aguilera_fallback_applied_manure"
  ))
  testthat::expect_identical(out$method_soil_n2o[3:4], rep("aguilera", 2L))
})

testthat::test_that("ipcc2019 reads the species of a species-labelled row", {
  # Table 11.1: EF3PRP,CPP 0.006 wet and 0.002 dry for cattle, poultry and
  # pigs; EF3PRP,SO 0.003 for sheep and other animals. The label is the
  # species, so no share column is needed, and a share that agrees with it
  # is accepted.
  x <- tibble::tribble(
    ~n_input_t, ~fert_type, ~item_cbs_code, ~moisture_regime,
    10, "Excreta_cattle_monog", 3000L, "wet",
    10, "Excreta_cattle_monog", 3000L, "dry",
    10, "Excreta_other", 3000L, "wet",
    10, "Excreta_other", 3000L, "dry"
  )
  expected <- c(0.06, 0.02, 0.03, 0.03)
  out <- whep::calculate_soil_n2o(x, method = "ipcc2019")
  testthat::expect_equal(out$n2o_direct_n_t, expected, tolerance = 1e-12)
  testthat::expect_true(all(out$method_soil_n2o == "ipcc2019_ef3prp_blend"))
  agreeing <- dplyr::mutate(x, excreta_cpp_share = c(1, NA, 0, NA))
  out <- whep::calculate_soil_n2o(agreeing, method = "ipcc2019")
  testthat::expect_equal(out$n2o_direct_n_t, expected, tolerance = 1e-12)
})

testthat::test_that("ipcc2019 refuses a share that contradicts the label", {
  x <- tibble::tribble(
    ~n_input_t, ~fert_type, ~item_cbs_code, ~moisture_regime,
    ~excreta_cpp_share,
    10, "Excreta_cattle_monog", 3000L, "wet", 0,
    10, "Excreta_other", 3000L, "wet", 1,
    10, "Excreta_other", 3000L, "wet", 0
  )
  err <- testthat::expect_error(
    whep::calculate_soil_n2o(x, method = "ipcc2019"),
    class = "whep_excreta_share_label_conflict"
  )
  msg <- cli::ansi_strip(conditionMessage(err))
  testthat::expect_match(msg, "2 excreta rows", fixed = TRUE)
  testthat::expect_match(msg, "Excreta_cattle_monog", fixed = TRUE)
})

testthat::test_that("a duplicated IPCC 2019 factor row aborts the lookup", {
  # A factor table holding the wet EF1 for other N twice: two values for one
  # factor is a broken table, never one to pick from.
  table <- whep::ipcc_2019_soil_n_ef
  twice <- table$parameter == "EF1" & table$disaggregation == "other_wet"
  testthat::local_mocked_bindings(
    .ipcc2019_soil_ef_table = function() dplyr::bind_rows(table, table[twice, ])
  )
  x <- tibble::tribble(
    ~n_input_t, ~fert_type, ~item_cbs_code, ~moisture_regime,
    10, "Solid", 2511L, "wet"
  )
  err <- testthat::expect_error(
    whep::calculate_soil_n2o(x, method = "ipcc2019"),
    class = "whep_ipcc2019_ef_lookup"
  )
  msg <- cli::ansi_strip(conditionMessage(err))
  testthat::expect_match(msg, "2 values", fixed = TRUE)
  testthat::expect_match(msg, "other_wet", fixed = TRUE)
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

# Meisinger leaching rows: solid manure (fert_cat Manure, so the tillage join
# is "Not_specified"), MED climate, rainfed cropland, C:N at the A_CN floor
# (cn_input NA coalesces to 15, so a_cn = 0) and High soil organic matter
# (som_share 0.06 > 0.05), on a 100 cm root zone of porosity 0.45 and field
# capacity 0.30 at 15 degC. From the tables: d = 0.12 in the High drainage
# class, 0.08 in Very_high, 0.5 in Very_low and 1 in None; the subsoil
# reduction of Solid/MED/Rainfed is 0.35.
.leach_rows <- function(n = 1L, ...) {
  base <- tibble::tibble(
    n_surplus_t = 100,
    fert_type = "Solid",
    climate = "MED",
    irrig_cat = "Rainfed",
    land_use = "Cropland",
    cn_input = NA_real_,
    tillage = "Not_specified",
    som_share = 0.06
  )
  base[rep(1L, n), ] |>
    .leach_soil() |>
    dplyr::mutate(...)
}

.leach_soil <- function(x) {
  dplyr::mutate(
    x,
    root_depth_cm = 100,
    soil_porosity = 0.45,
    soil_field_capacity = 0.30,
    temp_c_annual_mean = 15
  )
}

# SWAT well-mixed transport for porosity 0.45 and a 100 cm (1,000 mm) root
# zone, theta_e 0.5: the mixing volume is 0.5 * 0.45 * 1000 = 225 mm.
.leach_f_swat_600 <- function() 1 - exp(-600 / 225)

# Every Meisinger row must close its own partition, carried-in pool included.
.expect_leaching_closes <- function(out) {
  testthat::expect_equal(
    out$n_surplus_t + out$retained_carried_in_n_t,
    out$no3_n_t + out$denitrification_n_t + out$retained_profile_n_t,
    tolerance = 1e-12
  )
  testthat::expect_true(all(out$denitrification_n_t >= -1e-12))
  testthat::expect_true(all(out$retained_profile_n_t >= 0))
}

testthat::test_that("the SWAT mixing transport matches a hand computation", {
  # f = 1 - exp(-D / ((1 - 0.5) * porosity * h)), D and h in mm.
  f <- whep:::.leaching_f_swat(
    drainage_mm = c(600, 50),
    soil_porosity = c(0.45, 0.40),
    root_depth_cm = c(100, 30)
  )
  testthat::expect_equal(
    f,
    c(1 - exp(-600 / (0.5 * 0.45 * 1000)), 1 - exp(-50 / (0.5 * 0.40 * 300))),
    tolerance = 1e-12
  )
  testthat::expect_equal(f, c(0.930516549, 0.565401804), tolerance = 1e-8)
  # D -> 0 gives f -> 0; a large D gives f -> 1; no drainage, no transport.
  limits <- whep:::.leaching_f_swat(c(1e-9, 1e6, 0, -0.05), 0.45, 100)
  testthat::expect_lt(limits[1], 1e-11)
  testthat::expect_equal(limits[2], 1, tolerance = 1e-12)
  testthat::expect_identical(limits[3:4], c(0, 0))
})

testthat::test_that("the Burns transport matches a hand computation", {
  # f = (P / (P + theta_fc))^(h / 2), P and h in cm.
  f <- whep:::.leaching_f_burns(
    drainage_mm = c(600, 50),
    soil_field_capacity = c(0.30, 0.25),
    root_depth_cm = c(100, 30)
  )
  testthat::expect_equal(
    f,
    c((60 / 60.3)^50, (5 / 5.25)^15),
    tolerance = 1e-12
  )
  testthat::expect_equal(f, c(0.779286, 0.481017), tolerance = 1e-5)
  limits <- whep:::.leaching_f_burns(c(1e-6, 1e7, 0, -0.05), 0.30, 100)
  testthat::expect_lt(limits[1], 1e-100)
  testthat::expect_equal(limits[2], 1, tolerance = 1e-3)
  testthat::expect_identical(limits[3:4], c(0, 0))
})

testthat::test_that("the residence_time transport is gone and aborts as unknown", {
  testthat::expect_error(
    whep::calculate_n_leaching(
      .leach_rows(),
      drainage_mm = 600,
      transport = "residence_time"
    ),
    regexp = "must be one of"
  )
})

testthat::test_that("a first-year row starts at the steady-state pool, by hand", {
  # D = 600 mm (High class, d = 0.12), red = 0.35, a_cn = 0. The pool's
  # input keeps r = (1 - d)(1 - f) of itself, so the steady state is
  # S r / (1 - r), and the row then retains exactly that pool again.
  x <- .leach_rows()
  x$no3_n_t <- -999
  out <- whep::calculate_n_leaching(x, drainage_mm = 600)
  f <- .leach_f_swat_600()
  r <- 0.88 * (1 - f)
  pool <- 100 * r / (1 - r)
  testthat::expect_equal(pool, 6.51277, tolerance = 1e-5)
  testthat::expect_equal(out$retained_carried_in_n_t, pool, tolerance = 1e-12)
  testthat::expect_equal(
    out$no3_n_t,
    (100 + pool) * 0.88 * f * 0.65,
    tolerance = 1e-12
  )
  testthat::expect_equal(out$no3_n_t, 56.6920, tolerance = 1e-5)
  testthat::expect_equal(out$retained_profile_n_t, pool, tolerance = 1e-12)
  testthat::expect_equal(
    out$denitrification_n_t,
    100 + pool - out$no3_n_t - pool,
    tolerance = 1e-12
  )
  testthat::expect_equal(out$n2o_indirect_no3_n_t, out$no3_n_t * 0.011)
  testthat::expect_equal(
    out$method_leaching,
    "meisinger_drainage;swat_mixing;none_retention;initial_pool_steady_state"
  )
  .expect_leaching_closes(out)
})

testthat::test_that("the composition identity holds for every transport", {
  x <- .leach_rows(4L, n_surplus_t = c(100, 0, 37.5, 250))
  drainage <- c(600, 0.05, 2500, 40)
  for (transport in c("swat_mixing", "burns")) {
    out <- whep::calculate_n_leaching(
      x,
      drainage_mm = drainage,
      transport = transport
    )
    .expect_leaching_closes(out)
  }
})

testthat::test_that("a key whose first year does not drain starts its vadose store at zero", {
  # D = 0 falls in the None class, which takes the Very_low share 0.5 under
  # the default retention: half the surplus denitrifies, half is retained,
  # nothing leaches.
  out <- whep::calculate_n_leaching(.leach_rows(), drainage_mm = 0)
  testthat::expect_equal(out$retained_carried_in_n_t, 0)
  testthat::expect_equal(out$no3_n_t, 0)
  testthat::expect_equal(out$denitrification_n_t, 50, tolerance = 1e-12)
  testthat::expect_equal(out$retained_profile_n_t, 50, tolerance = 1e-12)
  testthat::expect_equal(
    out$method_leaching,
    paste0(
      "meisinger_drainage;swat_mixing;none_retention;initial_vadose_zero;",
      "vadose_storage"
    )
  )
})

# Key "a" drains, then does not, then drains again; key "b" is absent from
# 2002. Rows are given out of year order on purpose.
.leach_three_years <- function() {
  .leach_rows(
    5L,
    cell = c("a", "b", "a", "a", "b"),
    year = c(2003L, 2001L, 2001L, 2002L, 2003L),
    n_surplus_t = c(80, 100, 100, 60, 40),
    drainage_mm = c(600, 600, 600, 0, 600)
  )
}

testthat::test_that("carried nitrate is held through a dry year and re-enters the next drained one", {
  x <- .leach_three_years()
  out <- whep::calculate_n_leaching(
    x,
    drainage_mm = "drainage_mm",
    carry_over_by = "cell"
  )
  .expect_leaching_closes(out)
  a <- out[out$cell == "a", ]
  a <- a[order(a$year), ]
  f <- .leach_f_swat_600()
  r <- 0.88 * (1 - f)
  pool_2001 <- 100 * r / (1 - r)
  testthat::expect_equal(a$retained_carried_in_n_t[1], pool_2001)
  # 2002 does not drain: the pool carried in is neither leached nor
  # denitrified; only the year's own surplus meets the Very_low share 0.5.
  testthat::expect_equal(
    a$retained_carried_in_n_t[2],
    a$retained_profile_n_t[1]
  )
  testthat::expect_equal(a$no3_n_t[2], 0)
  testthat::expect_equal(a$denitrification_n_t[2], 60 * 0.5, tolerance = 1e-12)
  testthat::expect_equal(
    a$retained_profile_n_t[2],
    a$retained_carried_in_n_t[2] + 60 * 0.5,
    tolerance = 1e-12
  )
  # 2003 drains: the stored pool re-enters the cascade with the year's
  # surplus.
  c_in <- a$retained_profile_n_t[2]
  testthat::expect_equal(a$retained_carried_in_n_t[3], c_in)
  testthat::expect_equal(
    a$no3_n_t[3],
    (80 + c_in) * 0.88 * f * 0.65,
    tolerance = 1e-12
  )
  testthat::expect_equal(
    a$retained_profile_n_t[3],
    (80 + c_in) * 0.88 * (1 - f),
    tolerance = 1e-12
  )
  testthat::expect_equal(
    a$method_leaching,
    c(
      "meisinger_drainage;swat_mixing;none_retention;initial_pool_steady_state",
      paste0(
        "meisinger_drainage;swat_mixing;none_retention;carried_pool;",
        "vadose_storage"
      ),
      "meisinger_drainage;swat_mixing;none_retention;carried_pool"
    )
  )
})

testthat::test_that("a key absent from a year keeps its pool and is stamped", {
  out <- whep::calculate_n_leaching(
    .leach_three_years(),
    drainage_mm = "drainage_mm",
    carry_over_by = "cell"
  )
  b <- out[out$cell == "b", ]
  b <- b[order(b$year), ]
  # The 2001 pool waits through 2002 on its own key and re-enters in 2003.
  testthat::expect_equal(
    b$retained_carried_in_n_t[2],
    b$retained_profile_n_t[1]
  )
  testthat::expect_equal(
    b$method_leaching,
    c(
      paste0(
        "meisinger_drainage;swat_mixing;none_retention;",
        "initial_pool_steady_state;pool_held_absent_key"
      ),
      "meisinger_drainage;swat_mixing;none_retention;carried_pool"
    )
  )
  # Nothing vanishes over the run: the surplus is the leached and denitrified
  # nitrogen plus the final pools minus the initial ones.
  first <- out |> dplyr::slice_min(.data$year, by = "cell")
  last <- out |> dplyr::slice_max(.data$year, by = "cell")
  testthat::expect_equal(
    sum(out$n_surplus_t),
    sum(out$no3_n_t) +
      sum(out$denitrification_n_t) +
      sum(last$retained_profile_n_t) -
      sum(first$retained_carried_in_n_t),
    tolerance = 1e-12
  )
})

testthat::test_that("the order of the rows does not change the carry-over", {
  x <- .leach_three_years()
  out <- whep::calculate_n_leaching(
    x,
    drainage_mm = "drainage_mm",
    carry_over_by = "cell"
  )
  reversed <- whep::calculate_n_leaching(
    x[rev(seq_len(nrow(x))), ],
    drainage_mm = "drainage_mm",
    carry_over_by = "cell"
  )
  testthat::expect_equal(reversed[rev(seq_len(nrow(x))), ], out)
})

testthat::test_that("none_class prices the None drainage class both ways", {
  # D = 0.05 mm lies in the None class (-0.1 < D < 0.1). Retention takes the
  # Very_low share 0.5; waterlogged keeps the table's literal d = 1.
  x <- .leach_rows()
  retention <- whep::calculate_n_leaching(x, drainage_mm = 0.05)
  f <- 1 - exp(-0.05 / 225)
  r <- 0.5 * (1 - f)
  pool <- 100 * r / (1 - r)
  testthat::expect_equal(
    retention$no3_n_t,
    (100 + pool) * 0.5 * f * 0.65,
    tolerance = 1e-12
  )
  testthat::expect_gt(retention$no3_n_t, 0)
  waterlogged <- whep::calculate_n_leaching(
    x,
    drainage_mm = 0.05,
    none_class = "waterlogged"
  )
  testthat::expect_equal(waterlogged$no3_n_t, 0)
  testthat::expect_equal(waterlogged$retained_profile_n_t, 0)
  testthat::expect_equal(waterlogged$denitrification_n_t, 100)
  testthat::expect_equal(
    waterlogged$method_leaching,
    "meisinger_drainage;swat_mixing;none_waterlogged;initial_pool_steady_state"
  )
})

testthat::test_that("calculate_n_leaching(meisinger_drainage) resolves near-zero drainage to full denitrification for MED and ATL when waterlogged", {
  # Regression for the Meisinger join fan-out: a cell with drainage in the
  # "None" bin (-0.1 < S < 0.1) under the literal waterlogged table must
  # resolve to denit_share = 1 (whole surplus denitrified) for BOTH climates.
  # The table's "None" rows all carry climate_cat = "Semiarid", so the join
  # must key on climate (MED/ATL), not on a derived Semiarid/Humid
  # climate_cat: keying on the latter fans a MED cell onto two rows (crash)
  # and leaves an ATL cell unmatched (abort).
  x <- .leach_rows(2L, climate = c("MED", "ATL"))
  out <- whep::calculate_n_leaching(
    x,
    drainage_mm = c(0.05, 0.05),
    none_class = "waterlogged"
  )
  testthat::expect_equal(out$no3_n_t, c(0, 0), tolerance = 1e-9)
  testthat::expect_equal(out$denitrification_n_t, c(100, 100), tolerance = 1e-9)
  testthat::expect_equal(out$n2o_indirect_no3_n_t, c(0, 0), tolerance = 1e-9)
})

testthat::test_that("drainage above 3,000 mm falls in the open top bin", {
  # Very_high is 1000 < D < 3000 in drainage_ranges; above it the share is
  # still Very_high's (0.08), instead of an unmatched-row abort.
  x <- .leach_rows(3L)
  d <- whep:::.leaching_denit_share(
    x,
    rep("Manure", 3),
    c(2000, 3000, 5000),
    "retention"
  )
  very_high <- whep::meisinger_denitrification |>
    dplyr::filter(
      .data$fert_cat == "Manure",
      .data$tillage == "Not_specified",
      .data$som_content == "High",
      .data$drainage_rate == "Very_high",
      .data$climate == "MED"
    ) |>
    dplyr::pull("denit_share")
  testthat::expect_equal(very_high, 0.08)
  testthat::expect_equal(d, rep(0.08, 3))
  out <- whep::calculate_n_leaching(x, drainage_mm = c(2000, 3000, 5000))
  .expect_leaching_closes(out)
})

testthat::test_that("calculate_n_leaching(meisinger_drainage) drops a value exactly on a shared drainage edge", {
  # S = 1000 is the shared High/Very_high edge; strictly-open bins match
  # neither, so the row is unmatched and aborts.
  testthat::expect_error(
    whep::calculate_n_leaching(.leach_rows(), drainage_mm = 1000)
  )
})

testthat::test_that("a missing leaching driver aborts with a condition class", {
  x <- .leach_rows()
  for (col in c("root_depth_cm", "soil_porosity")) {
    err <- testthat::expect_error(
      whep::calculate_n_leaching(x[setdiff(names(x), col)], drainage_mm = 600),
      class = "whep_missing_leaching_driver"
    )
    testthat::expect_s3_class(err, "whep_absent_input")
    testthat::expect_match(conditionMessage(err), col, fixed = TRUE)
  }
  # Each transport requires only the columns it reads.
  testthat::expect_error(
    whep::calculate_n_leaching(
      x[setdiff(names(x), "soil_field_capacity")],
      drainage_mm = 600,
      transport = "burns"
    ),
    class = "whep_missing_leaching_driver"
  )
  testthat::expect_no_error(
    whep::calculate_n_leaching(
      x[setdiff(names(x), "soil_field_capacity")],
      drainage_mm = 600
    )
  )
  # A missing value is as absent as a missing column, and so is drainage.
  testthat::expect_error(
    whep::calculate_n_leaching(
      dplyr::mutate(x, root_depth_cm = NA_real_),
      drainage_mm = 600
    ),
    class = "whep_missing_leaching_driver"
  )
  testthat::expect_error(
    whep::calculate_n_leaching(x, drainage_mm = NA_real_),
    class = "whep_missing_leaching_driver"
  )
  testthat::expect_error(
    whep::calculate_n_leaching(x),
    class = "whep_missing_leaching_driver"
  )
})

testthat::test_that("an impossible soil or drainage value aborts", {
  x <- .leach_rows()
  bad <- list(
    dplyr::mutate(x, root_depth_cm = 0),
    dplyr::mutate(x, soil_porosity = 1.2),
    dplyr::mutate(x, soil_porosity = 0)
  )
  for (row in bad) {
    testthat::expect_error(
      whep::calculate_n_leaching(row, drainage_mm = 600),
      class = "whep_leaching_driver_value"
    )
  }
  testthat::expect_error(
    whep::calculate_n_leaching(
      dplyr::mutate(x, soil_field_capacity = -0.1),
      drainage_mm = 600,
      transport = "burns"
    ),
    class = "whep_leaching_driver_value"
  )
  testthat::expect_error(
    whep::calculate_n_leaching(x, drainage_mm = -5),
    class = "whep_leaching_driver_value"
  )
})

testthat::test_that("a carry-over key must be present and unique per year", {
  x <- .leach_three_years()
  testthat::expect_error(
    whep::calculate_n_leaching(
      x,
      drainage_mm = "drainage_mm",
      carry_over_by = "plot"
    ),
    class = "whep_leaching_carry_key"
  )
  testthat::expect_error(
    whep::calculate_n_leaching(
      dplyr::select(x, -"year"),
      drainage_mm = "drainage_mm",
      carry_over_by = "cell"
    ),
    class = "whep_leaching_carry_key"
  )
  testthat::expect_error(
    whep::calculate_n_leaching(
      dplyr::bind_rows(x, x[1, ]),
      drainage_mm = "drainage_mm",
      carry_over_by = "cell"
    ),
    class = "whep_leaching_carry_key"
  )
})

testthat::test_that("calculate_n_leaching(meisinger_drainage) aborts on No_tillage for synthetic rows", {
  x <- .leach_rows(fert_type = "Synthetic", tillage = "No_tillage")
  testthat::expect_error(
    whep::calculate_n_leaching(x, drainage_mm = 600),
    "No_tillage"
  )
})

testthat::test_that("calculate_n_leaching(meisinger_drainage) aborts on an unmapped fert_type", {
  # "Recycling" is a real fert_type elsewhere in this file's aguilera join
  # but has no row in subsoil_no3_reduction, so it must abort rather than
  # silently propagate NA through no3_n_t/denitrification_n_t.
  x <- .leach_rows(fert_type = "Recycling", som_share = 0.03)
  testthat::expect_error(
    whep::calculate_n_leaching(x, drainage_mm = 600),
    "subsoil_no3_reduction"
  )
})

testthat::test_that("calculate_n_leaching(meisinger_drainage) aborts on an out-of-range som_share", {
  x <- .leach_rows(
    fert_type = "Synthetic",
    tillage = "Tillage",
    som_share = -0.01
  )
  testthat::expect_error(
    whep::calculate_n_leaching(x, drainage_mm = 600),
    "meisinger_denitrification"
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
  # Dry rainfed land leaches nothing, and its surplus is not estimated to
  # denitrify either: it is booked as retained in the profile.
  testthat::expect_equal(
    out$denitrification_n_t,
    c(76, 76, 76, 0),
    tolerance = 1e-12
  )
  testthat::expect_equal(
    out$retained_profile_n_t,
    c(0, 0, 0, 100),
    tolerance = 1e-12
  )
  testthat::expect_equal(
    out$no3_n_t + out$denitrification_n_t + out$retained_profile_n_t,
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
      "ipcc_fracleach_dry_rainfed_retained"
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
      "retained_profile_n_t",
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

# ---- calculate_n_leaching(method = "image_gnm") --------------------------

# IMAGE-GNM rows (Beusen et al. 2015, eqs. 10-14 and Table 2): a loam (field
# capacity 0.29 and wilting point 0.14, the loam row of
# soil_hydraulic_by_texture) at 15 degC with a total runoff of 100 mm/yr, so
# the top 1 m holds tawc = 0.15 x 1,000 = 150 mm and T_r = 150 / 100 =
# 1.5 yr, with f_K(15 degC) = 0.216327. Medium texture (f_text 0.1),
# moderately well drained (f_drain 0.1) and 2% organic carbon (f_soc 0.1). A
# surface runoff fraction of 0 runs no nitrogen off (eq. 9) and leaves the
# whole runoff as excess water (eq. 5, q_eff = q_tot), so the leaching tests
# below read eqs. 10-14 alone; 100 ha on a flat cell erode a little soil,
# which eq. 10 does not take from the budget.
.gnm_rows <- function(n = 1L, ...) {
  base <- tibble::tibble(
    n_surplus_t = 100,
    n_input_std_t = 150,
    nh3_n_t = 10,
    area_ha = 100,
    runoff_mm = 100,
    surface_runoff_fraction = 0,
    slope_m_per_km = 0,
    land_use = "Cropland",
    temp_c_annual_mean = 15,
    soil_field_capacity = 0.29,
    soil_wilting_point = 0.14,
    soil_texture_class = "medium",
    soil_drainage_class = "moderately_well_drained",
    soil_organic_carbon_pct = 2
  )
  dplyr::mutate(base[rep(1L, n), ], ...)
}

.gnm_f_k <- function(temp_c) {
  7.94e12 * exp(-74830 / (8.3144 * (temp_c + 273.15)))
}

# Every image_gnm row closes eq. 10: the budget less surface runoff is leached
# or denitrified, and nothing is carried (IMAGE-GNM has no pool). Surface
# runoff is a share of the inputs (eq. 9), so it can exceed the surplus; the
# budget left is then 0, as in the authors' code, and the row books the
# larger of the two. On grassland and natural land with a total runoff below
# 3 mm/yr (n_nowater.py) the surplus is held as inactive N in
# retained_profile_n_t instead, nothing leaches or denitrifies, and the
# grassland runoff, a share of the N the grass took up, is booked beside it.
.expect_gnm_closes <- function(out) {
  nowater <- grepl("nowater_inactive_n", out$method_leaching, fixed = TRUE)
  booked <- out$surface_runoff_n_t +
    out$no3_n_t +
    out$denitrification_n_t +
    out$retained_profile_n_t
  testthat::expect_equal(
    booked,
    dplyr::if_else(
      nowater,
      pmax(out$n_surplus_t, 0) + out$surface_runoff_n_t,
      pmax(out$n_surplus_t, out$surface_runoff_n_t)
    ),
    tolerance = 1e-12
  )
  testthat::expect_identical(
    out$retained_profile_n_t,
    dplyr::if_else(nowater, pmax(out$n_surplus_t, 0), 0)
  )
  testthat::expect_true(all(out$no3_n_t[nowater] == 0))
  testthat::expect_true(all(out$denitrification_n_t[nowater] == 0))
  testthat::expect_identical(out$retained_carried_in_n_t, rep(0, nrow(out)))
  testthat::expect_true(all(out$no3_n_t >= 0 & out$denitrification_n_t >= 0))
}

testthat::test_that("image_gnm follows eq. 11 by hand on a loam at 15 degC", {
  f_k <- .gnm_f_k(15)
  testthat::expect_equal(f_k, 0.216327, tolerance = 1e-5)
  x <- .gnm_rows(3L, land_use = c("Cropland", "Grassland", "Natural"))
  out <- whep::calculate_n_leaching(x, method = "image_gnm")
  # Cropland: T_r = min(1, 1.5) = 1 and f_landuse = 1. Grassland and natural
  # vegetation: T_r = 1.5 yr and f_landuse = 0.36.
  crop <- 1 - min(f_k * 1 + 0.1 + 0.1 + 0.1, 1)
  grass <- (1 - min(f_k * 1.5 + 0.1 + 0.1 + 0.1, 1)) * 0.36
  testthat::expect_equal(
    out$no3_n_t,
    100 * c(crop, grass, grass),
    tolerance = 1e-12
  )
  testthat::expect_equal(
    out$no3_n_t,
    c(48.3673, 13.5183, 13.5183),
    tolerance = 1e-5
  )
  testthat::expect_equal(
    out$denitrification_n_t,
    100 - out$no3_n_t,
    tolerance = 1e-12
  )
  testthat::expect_equal(out$n2o_indirect_no3_n_t, out$no3_n_t * 0.011)
  testthat::expect_identical(
    out$method_leaching,
    c(
      "image_gnm;tawc_top_1m;crop_residence_cap_1yr",
      "image_gnm;tawc_top_1m",
      "image_gnm;tawc_top_1m"
    )
  )
  .expect_gnm_closes(out)
})

testthat::test_that("image_gnm splits the total runoff by eq. 5 into q_eff", {
  # Eq. 5 as the authors' code computes it (n_soil_denitrification.py lines
  # 356, 376 and 399): q_eff = (1 - f_qsro(landuse) f_qsro) q_tot, with
  # f_qsro(landuse) 1, 0.25 and 0.125 on crops, grassland and natural
  # vegetation. f_qsro 0.4 and q_tot 300 mm leave 180, 270 and 285 mm, so
  # T_r = 150 / q_eff is 0.833, 0.556 and 0.526 yr, all below the cropland
  # cap. Eq. 9 runs off 0.3 x f_qsro(landuse) x 0.4 of 140 t (crops,
  # grassland) and of the 100 t surplus (natural): 16.8, 4.2 and 1.5 t.
  f_k <- .gnm_f_k(15)
  x <- .gnm_rows(
    3L,
    land_use = c("Cropland", "Grassland", "Natural"),
    surface_runoff_fraction = 0.4,
    runoff_mm = 300
  )
  out <- whep::calculate_n_leaching(x, method = "image_gnm")
  q_eff <- c(180, 270, 285)
  f_leach <- (1 - (f_k * 150 / q_eff + 0.3)) * c(1, 0.36, 0.36)
  testthat::expect_equal(
    out$surface_runoff_n_t,
    c(16.8, 4.2, 1.5),
    tolerance = 1e-12
  )
  testthat::expect_equal(
    out$no3_n_t,
    (100 - c(16.8, 4.2, 1.5)) * f_leach,
    tolerance = 1e-12
  )
  testthat::expect_equal(
    out$no3_n_t,
    c(43.2413410772, 19.9967782938, 20.7846585622),
    tolerance = 1e-10
  )
  # The whole runoff as q_eff (300 mm, T_r 0.5 yr) would leach more: the
  # part surface runoff carries off never enters the root zone.
  whole <- (1 - (f_k * 0.5 + 0.3)) * c(1, 0.36, 0.36)
  testthat::expect_true(all(out$no3_n_t < (100 - c(16.8, 4.2, 1.5)) * whole))
  testthat::expect_identical(
    out$method_leaching,
    rep("image_gnm;tawc_top_1m", 3)
  )
  .expect_gnm_closes(out)
})

testthat::test_that("image_gnm does not read drainage_mm", {
  # Eq. 14's water flow is the eq. 5 split of the total runoff; nothing else
  # in the method reads the deep seepage.
  x <- .gnm_rows(2L, land_use = c("Cropland", "Grassland"))
  bare <- whep::calculate_n_leaching(x, method = "image_gnm")
  testthat::expect_identical(
    whep::calculate_n_leaching(x, drainage_mm = 5, method = "image_gnm"),
    bare
  )
  from_column <- whep::calculate_n_leaching(
    dplyr::mutate(x, drainage_mm = c(0, 5000)),
    drainage_mm = "drainage_mm",
    method = "image_gnm"
  )
  testthat::expect_identical(dplyr::select(from_column, -"drainage_mm"), bare)
})

testthat::test_that("image_gnm caps the cropland residence time at one year", {
  f_k <- .gnm_f_k(15)
  # 10 mm/yr gives T_r = 15 yr. Cropland takes min(1, 15) = 1; grassland
  # keeps 15 yr, so f_climate = 3.24 saturates MIN(..., 1) and nothing
  # leaches.
  dry <- whep::calculate_n_leaching(
    .gnm_rows(2L, land_use = c("Cropland", "Grassland"), runoff_mm = 10),
    method = "image_gnm"
  )
  capped <- 100 * (1 - (f_k + 0.3))
  testthat::expect_equal(dry$no3_n_t, c(capped, 0), tolerance = 1e-12)
  testthat::expect_equal(dry$denitrification_n_t, c(100 - capped, 100))
  # Any T_r above one year gives the cropland row the same leaching; below
  # it (300 mm/yr, T_r = 0.5 yr) the cap does not bind.
  out <- whep::calculate_n_leaching(
    .gnm_rows(3L, runoff_mm = c(10, 75, 300)),
    method = "image_gnm"
  )
  testthat::expect_equal(
    out$no3_n_t,
    c(capped, capped, 100 * (1 - (f_k * 0.5 + 0.3))),
    tolerance = 1e-12
  )
  testthat::expect_identical(
    out$method_leaching,
    c(
      rep("image_gnm;tawc_top_1m;crop_residence_cap_1yr", 2),
      "image_gnm;tawc_top_1m"
    )
  )
  .expect_gnm_closes(out)
})

testthat::test_that("image_gnm scales grassland leaching by f_landuse 0.36", {
  # Coarse texture, well drained and 0.5% organic carbon take 0 from Table 2
  # (well drained from the authors' code), and 1,500 mm/yr gives T_r = 0.1 yr,
  # below the cropland cap, so the two rows differ only by f_landuse.
  x <- .gnm_rows(
    2L,
    land_use = c("Cropland", "Grassland"),
    soil_texture_class = "coarse",
    soil_drainage_class = "well_drained",
    soil_organic_carbon_pct = 0.5,
    runoff_mm = 1500
  )
  out <- whep::calculate_n_leaching(x, method = "image_gnm")
  f_leach <- 1 - .gnm_f_k(15) * 0.1
  testthat::expect_equal(
    out$no3_n_t,
    100 * f_leach * c(1, 0.36),
    tolerance = 1e-12
  )
  testthat::expect_equal(out$no3_n_t[2] / out$no3_n_t[1], 0.36)
  .expect_gnm_closes(out)
})

testthat::test_that("image_gnm reads every Table 2 class and SOC bin", {
  testthat::expect_equal(
    whep:::.gnm_class_factor(
      c("coarse", "medium", "fine", "very_fine", "organic"),
      "texture"
    ),
    c(0, 0.1, 0.2, 0.3, 0)
  )
  testthat::expect_equal(
    whep:::.gnm_class_factor(
      c(
        "excessively_drained",
        "somewhat_excessively_drained",
        "well_drained",
        "moderately_well_drained",
        "imperfectly_drained",
        "poorly_drained",
        "very_poorly_drained"
      ),
      "drainage"
    ),
    c(0, 0, 0, 0.1, 0.2, 0.3, 0.4)
  )
  testthat::expect_equal(
    whep:::.gnm_class_factor(c("Cropland", "Grassland", "Natural"), "land_use"),
    c(1, 0.36, 0.36)
  )
  # Each class holds its lower bound: 1-3% starts at 1, as in the authors'
  # class code.
  testthat::expect_equal(
    whep:::.gnm_soc_factor(c(0, 0.99, 1, 2.99, 3, 5.99, 6, 49.9, 50, 80)),
    c(0, 0, 0.1, 0.1, 0.2, 0.2, 0.3, 0.3, 0.3, 0.3)
  )
})

testthat::test_that("image_gnm takes a 100-year residence time without excess water", {
  # The authors' trzd(): T_r = 100 yr where q_eff is 0 (lines 179-182). On
  # cropland it is then capped at 1 yr (lines 377-378); grassland and natural
  # land with no water never reach eq. 14 (n_nowater.py, tested below).
  out <- whep::calculate_n_leaching(
    .gnm_rows(runoff_mm = 0),
    method = "image_gnm"
  )
  testthat::expect_equal(
    out$no3_n_t,
    100 * (1 - (.gnm_f_k(15) + 0.3)),
    tolerance = 1e-12
  )
  testthat::expect_identical(
    out$method_leaching,
    paste0(
      "image_gnm;tawc_top_1m;no_excess_water_residence_100yr;",
      "crop_residence_cap_1yr"
    )
  )
  .expect_gnm_closes(out)
  # q_eff is also 0 where surface runoff takes the whole flow: a cropland
  # f_qsro of 1 leaves (1 - 1 x 1) x 500 = 0 mm in the root zone.
  sealed <- whep::calculate_n_leaching(
    .gnm_rows(runoff_mm = 500, surface_runoff_fraction = 1),
    method = "image_gnm"
  )
  testthat::expect_identical(
    sealed$method_leaching,
    paste0(
      "image_gnm;tawc_top_1m;no_excess_water_residence_100yr;",
      "crop_residence_cap_1yr"
    )
  )
})

testthat::test_that("image_gnm sets a total runoff below 3 mm/yr to 0", {
  # The authors' code moves no water below a net precipitation of 3 mm/yr
  # (n_soil_denitrification.py lines 341-344; minimal_soil_infiltration = 3.0
  # mm/yr, parameters_history_100.ini line 204). A 1 mm tawc makes the cut
  # visible on cropland: at 3 mm, T_r = 1 / 3 yr; at 2.99 mm the flow is 0
  # and T_r = 100 yr, held at 1. A float residue of LPJmL's runoff (-1e-13)
  # is cut to 0 as well; a runoff of exactly 0 needs no cut.
  x <- .gnm_rows(
    4L,
    soil_wilting_point = 0.289,
    runoff_mm = c(3, 2.99, -1e-13, 0)
  )
  out <- whep::calculate_n_leaching(x, method = "image_gnm")
  f_k <- .gnm_f_k(15)
  testthat::expect_equal(
    out$no3_n_t,
    100 * c(1 - (f_k / 3 + 0.3), rep(1 - (f_k + 0.3), 3)),
    tolerance = 1e-12
  )
  cut <- "image_gnm;tawc_top_1m;runoff_below_min_infiltration"
  dry <- ";no_excess_water_residence_100yr;crop_residence_cap_1yr"
  testthat::expect_identical(
    out$method_leaching,
    c(
      "image_gnm;tawc_top_1m",
      paste0(cut, dry),
      paste0(cut, dry),
      paste0("image_gnm;tawc_top_1m", dry)
    )
  )
  .expect_gnm_closes(out)
})

testthat::test_that("image_gnm holds dry grassland and natural surpluses as inactive N", {
  # The authors' n_nowater.py (lines 31-46, called before surface runoff and
  # denitrification, N-model.py lines 210-214) sets the grassland and natural
  # balances to 0 where the net precipitation is below 3 mm/yr and books
  # them as inactive N; cropland is left alone ("there is always enough
  # water", line 19). Here that N is retained_profile_n_t: nothing leaches or
  # denitrifies. Surface runoff then reads the zeroed balance
  # (n_surface_runoff.py lines 96 and 128): natural land runs off
  # max(0, 0) = 0, and grassland a share of balance + uptake = uptake, the
  # input net of ammonia less the surplus, 140 - 100 = 40 t:
  # 0.3 x 0.25 x 0.5 x 40 = 1.5 t, booked beside the inactive N.
  x <- .gnm_rows(
    4L,
    land_use = c("Cropland", "Grassland", "Natural", "Grassland"),
    surface_runoff_fraction = 0.5,
    runoff_mm = c(2, 2, 2, 3)
  )
  out <- whep::calculate_n_leaching(x, method = "image_gnm")
  testthat::expect_equal(
    out$surface_runoff_n_t,
    c(21, 1.5, 0, 0.3 * 0.25 * 0.5 * 140),
    tolerance = 1e-12
  )
  testthat::expect_identical(out$retained_profile_n_t, c(0, 100, 100, 0))
  testthat::expect_identical(out$no3_n_t[2:3], c(0, 0))
  testthat::expect_identical(out$denitrification_n_t[2:3], c(0, 0))
  # Cropland keeps eq. 10: a budget of 100 - 21 = 79 t, T_r held at 1 yr.
  testthat::expect_equal(
    out$no3_n_t[1],
    79 * (1 - (.gnm_f_k(15) + 0.3)),
    tolerance = 1e-12
  )
  # At 3 mm/yr grassland is active again: q_eff = (1 - 0.25 x 0.5) x 3 mm.
  testthat::expect_equal(
    out$no3_n_t[4],
    (100 - 5.25) *
      (1 - min(.gnm_f_k(15) * 150 / (0.875 * 3) + 0.3, 1)) *
      0.36,
    tolerance = 1e-12
  )
  nowater <- "image_gnm;tawc_top_1m;runoff_below_min_infiltration"
  testthat::expect_identical(
    out$method_leaching,
    c(
      paste0(
        nowater,
        ";no_excess_water_residence_100yr;crop_residence_cap_1yr"
      ),
      paste0(nowater, ";nowater_inactive_n"),
      paste0(nowater, ";nowater_inactive_n"),
      "image_gnm;tawc_top_1m"
    )
  )
  .expect_gnm_closes(out)
  # A runoff of exactly 0 is dry without a cut, and a deficit is held as no
  # inactive N: the uptake share still runs off, 0.3 x 0.25 x 0.5 x 145.
  deficit <- whep::calculate_n_leaching(
    .gnm_rows(
      2L,
      land_use = "Grassland",
      n_surplus_t = c(50, -5),
      surface_runoff_fraction = 0.5,
      runoff_mm = 0
    ),
    method = "image_gnm"
  )
  testthat::expect_identical(deficit$retained_profile_n_t, c(50, 0))
  testthat::expect_equal(
    deficit$surface_runoff_n_t,
    0.3 * 0.25 * 0.5 * c(90, 145),
    tolerance = 1e-12
  )
  testthat::expect_identical(
    deficit$method_leaching,
    c(
      "image_gnm;tawc_top_1m;nowater_inactive_n",
      "image_gnm;tawc_top_1m;nowater_inactive_n;negative_surplus_budget_0"
    )
  )
  .expect_gnm_closes(deficit)
})

testthat::test_that("image_gnm caps the temperature effect at one", {
  # f_K reaches 1 near 30 degC. At 35 degC the uncapped 1.64 is held at 1:
  # with T_r = 150 / 300 = 0.5 yr, f_climate = 0.5 and 20 t leach, where the
  # uncapped 0.82 + 0.3 would saturate MIN(..., 1) and leach nothing. At
  # 25 degC f_K = 0.6167 is kept.
  testthat::expect_gt(.gnm_f_k(35), 1)
  out <- whep::calculate_n_leaching(
    .gnm_rows(2L, temp_c_annual_mean = c(35, 25), runoff_mm = 300),
    method = "image_gnm"
  )
  testthat::expect_equal(
    out$no3_n_t,
    c(100 * (1 - (0.5 + 0.3)), 100 * (1 - (.gnm_f_k(25) * 0.5 + 0.3))),
    tolerance = 1e-12
  )
  testthat::expect_identical(
    out$method_leaching,
    c("image_gnm;tawc_top_1m;temperature_effect_cap_1", "image_gnm;tawc_top_1m")
  )
})

testthat::test_that("image_gnm leaches only the budget surface runoff leaves (eq. 10)", {
  # Cropland, f_qsro 0.5 and 210 - 10 = 200 t of input net of ammonia:
  # N_sro = 0.3 x 1 x 0.5 x 200 = 30 t (eq. 9), so 70 of the 100 t surplus
  # is the budget of eq. 10. q_eff = 0.5 x 100 = 50 mm, so T_r = 3 yr, held
  # at 1 on cropland.
  out <- whep::calculate_n_leaching(
    .gnm_rows(n_input_std_t = 210, surface_runoff_fraction = 0.5),
    method = "image_gnm"
  )
  testthat::expect_equal(out$surface_runoff_n_t, 30, tolerance = 1e-12)
  f_leach <- 1 - (.gnm_f_k(15) + 0.3)
  testthat::expect_equal(out$no3_n_t, 70 * f_leach, tolerance = 1e-12)
  testthat::expect_equal(
    out$denitrification_n_t,
    70 * (1 - f_leach),
    tolerance = 1e-12
  )
  testthat::expect_identical(
    out$method_leaching,
    "image_gnm;tawc_top_1m;crop_residence_cap_1yr"
  )
  .expect_gnm_closes(out)
})

testthat::test_that("image_gnm books the runoff when it exceeds the surplus (D66)", {
  # Surface runoff is a share of the input (eq. 9), so on crops and grassland
  # it can exceed the surplus; eq. 10 is then negative. The authors' code
  # sets the budget to 0 (n_surface_runoff.py lines 106 and 122), so nothing
  # leaches or denitrifies and the row books max(surplus, runoff) = runoff.
  # Crops run off 0.3 x 1 x 0.5 x 140 = 21 t and grassland 0.3 x 0.25 x 0.5 x
  # 140 = 5.25 t against a 2 t surplus. Natural vegetation runs off a share of
  # its surplus (line 128), 0.3 x 0.125 x 0.5 x 2 = 0.0375 t, which never
  # exceeds it.
  x <- .gnm_rows(
    3L,
    land_use = c("Cropland", "Grassland", "Natural"),
    n_surplus_t = 2,
    surface_runoff_fraction = 0.5
  )
  out <- whep::calculate_n_leaching(x, method = "image_gnm")
  testthat::expect_equal(
    out$surface_runoff_n_t,
    c(21, 5.25, 0.0375),
    tolerance = 1e-12
  )
  testthat::expect_identical(out$no3_n_t[1:2], c(0, 0))
  testthat::expect_identical(out$denitrification_n_t[1:2], c(0, 0))
  booked <- out$surface_runoff_n_t + out$no3_n_t + out$denitrification_n_t
  testthat::expect_equal(booked, c(21, 5.25, 2), tolerance = 1e-12)
  testthat::expect_equal(
    booked,
    pmax(out$n_surplus_t, out$surface_runoff_n_t),
    tolerance = 1e-12
  )
  testthat::expect_identical(
    endsWith(out$method_leaching, ";surface_runoff_exceeds_budget"),
    c(TRUE, TRUE, FALSE)
  )
  .expect_gnm_closes(out)
})

testthat::test_that("image_gnm floors a negative surplus at a budget of 0", {
  # A direct caller may pass a deficit. The authors' code floors the balance
  # at 0 (n_surface_runoff.py lines 106, 122 and 136) and runs natural
  # vegetation off max(0, balance) (line 128): nothing leaches or
  # denitrifies, the deficit is booked as no loss, and the row says so.
  # Grassland still runs off its input net of ammonia, 0.3 x 0.25 x 0.5 x
  # 140 = 5.25 t, so it carries both fields.
  x <- .gnm_rows(
    3L,
    land_use = c("Cropland", "Natural", "Grassland"),
    n_surplus_t = -5,
    surface_runoff_fraction = c(0, 0.5, 0.5)
  )
  out <- whep::calculate_n_leaching(x, method = "image_gnm")
  testthat::expect_equal(out$surface_runoff_n_t, c(0, 0, 5.25))
  testthat::expect_identical(out$no3_n_t, c(0, 0, 0))
  testthat::expect_identical(out$denitrification_n_t, c(0, 0, 0))
  base <- "image_gnm;tawc_top_1m"
  testthat::expect_identical(
    out$method_leaching,
    c(
      paste0(base, ";crop_residence_cap_1yr;negative_surplus_budget_0"),
      paste0(base, ";negative_surplus_budget_0"),
      paste0(
        base,
        ";negative_surplus_budget_0;surface_runoff_exceeds_budget"
      )
    )
  )
  .expect_gnm_closes(out)
})

testthat::test_that("image_gnm computes surface runoff N by eq. 9 on two authors' cells", {
  # f_qsro of two cells of the authors' 0.5-degree layer
  # (water_input/pcrglobwb_100/fQsro.map of the paper's supplement): 5.25E
  # 52.25N, 0.0135612, and 10.25E 46.25N, 0.524785. Eq. 9 (p. 4052):
  # N_sro = f_cal f_qsro(landuse) f_qsro N_inp, with f_cal 0.3 and
  # f_qsro(landuse) 1 on crops and 0.25 on grassland (Table 3, p. 4056).
  # N_inp is the input net of ammonia, 200 - 20 and 80 - 8 t.
  x <- .gnm_rows(
    2L,
    land_use = c("Cropland", "Grassland"),
    surface_runoff_fraction = c(0.0135612, 0.524785),
    n_input_std_t = c(200, 80),
    nh3_n_t = c(20, 8)
  )
  out <- whep::calculate_n_leaching(x, method = "image_gnm")
  testthat::expect_equal(
    out$surface_runoff_n_t,
    c(0.3 * 1 * 0.0135612 * 180, 0.3 * 0.25 * 0.524785 * 72),
    tolerance = 1e-12
  )
  testthat::expect_equal(
    out$surface_runoff_n_t,
    c(0.7323048, 2.833839),
    tolerance = 1e-7
  )
  # The budget of eq. 10 is the surplus less that runoff. Eq. 5 leaves the
  # grassland cell (1 - 0.25 x 0.524785) x 100 = 86.88 mm of excess water,
  # T_r = 1.7265 yr; the cropland cell's T_r is held at 1.
  f_k <- .gnm_f_k(15)
  q_eff_grass <- 100 * (1 - 0.25 * 0.524785)
  f_leach <- c(1 - (f_k + 0.3), (1 - (f_k * 150 / q_eff_grass + 0.3)) * 0.36)
  testthat::expect_equal(f_leach[2], 0.1175433081, tolerance = 1e-9)
  testthat::expect_equal(
    out$no3_n_t,
    (100 - out$surface_runoff_n_t) * f_leach,
    tolerance = 1e-12
  )
  .expect_gnm_closes(out)
})

testthat::test_that("image_gnm takes N_inp by land use as the authors' code does", {
  # Natural vegetation runs off a share of its balance, f_qsro(nat) 0.125
  # (n_surface_runoff.py line 128); crops and grassland a share of the input
  # net of ammonia, floored at 0 (lines 96 and 112).
  x <- .gnm_rows(
    3L,
    land_use = c("Natural", "Cropland", "Grassland"),
    n_surplus_t = c(10, 100, 100),
    surface_runoff_fraction = 0.111708,
    n_input_std_t = c(500, 5, 40),
    nh3_n_t = c(0, 8, 4)
  )
  out <- whep::calculate_n_leaching(x, method = "image_gnm")
  testthat::expect_equal(
    out$surface_runoff_n_t,
    c(0.3 * 0.125 * 0.111708 * 10, 0, 0.3 * 0.25 * 0.111708 * 36),
    tolerance = 1e-12
  )
  testthat::expect_equal(out$surface_runoff_n_t[1], 0.0418905, tolerance = 1e-9)
  .expect_gnm_closes(out)
})

testthat::test_that("image_gnm computes erosion N on each land use (eq. 8)", {
  # The authors' code (n_erosion.py lines 50-55, 88-135): erodibility
  # -1.5 + 17 / (1 + exp(2.3 - 6.1 S / 1000)) times the texture factor
  # (texture_erosion.vat: 1.5 medium, 0.5 coarse), soil loss per km2 of
  # 1151, 99 and 29 t times it on crops, grassland and natural vegetation,
  # and N = topsoil organic carbon x soil loss / C:N (12, 14, 14).
  slope_factor <- function(s) -1.5 + 17 / (1 + exp(2.3 - 6.1 * s / 1000))
  x <- .gnm_rows(
    3L,
    land_use = c("Cropland", "Grassland", "Natural"),
    slope_m_per_km = c(1.2113556, 220.75122, 11.114079),
    soil_texture_class = c("medium", "coarse", "medium"),
    soil_organic_carbon_pct = c(2, 4, 1),
    area_ha = c(1000, 500, 2000)
  )
  out <- whep::calculate_n_leaching(x, method = "image_gnm")
  testthat::expect_equal(
    out$erosion_n_t,
    c(
      slope_factor(1.2113556) * 1.5 * 1151 * 10 * 0.02 / 12,
      slope_factor(220.75122) * 0.5 * 99 * 5 * 0.04 / 14,
      slope_factor(11.114079) * 1.5 * 29 * 20 * 0.01 / 14
    ),
    tolerance = 1e-12
  )
  testthat::expect_equal(
    out$erosion_n_t,
    c(1.7128436874917512, 2.2836236181895324, 0.09148988379762495),
    tolerance = 1e-12
  )
  # Erosion is the soil's long-term memory (eq. 8), not the year's budget:
  # the authors' code adds it to the river load and never takes it from the
  # balance (N-model.py lines 242-243 and 307), so leaching is unchanged.
  flat <- whep::calculate_n_leaching(
    dplyr::mutate(x, area_ha = 0),
    method = "image_gnm"
  )
  testthat::expect_identical(flat$erosion_n_t, c(0, 0, 0))
  testthat::expect_identical(flat$no3_n_t, out$no3_n_t)
  testthat::expect_identical(flat$denitrification_n_t, out$denitrification_n_t)
  .expect_gnm_closes(out)
})

testthat::test_that("image_gnm reads every erosion texture class", {
  testthat::expect_equal(
    whep:::.gnm_runoff_factor(
      c("coarse", "medium", "fine", "very_fine", "organic"),
      "erosion_texture"
    ),
    c(0.5, 1.5, 1.5, 0.5, 0.5)
  )
  testthat::expect_equal(
    whep:::.gnm_runoff_factor(
      c("Cropland", "Grassland", "Natural"),
      "runoff_land_use"
    ),
    c(1, 0.25, 0.125)
  )
  testthat::expect_error(
    whep:::.gnm_runoff_factor("loam", "erosion_texture"),
    class = "whep_leaching_class_value"
  )
})

testthat::test_that("image_gnm refuses a supplied surface runoff", {
  testthat::expect_error(
    whep::calculate_n_leaching(
      .gnm_rows(surface_runoff_n_t = 5),
      method = "image_gnm"
    ),
    class = "whep_gnm_runoff_supplied"
  )
})

testthat::test_that("image_gnm keeps no pool across years", {
  x <- .gnm_rows(3L, year = 2000:2002, key = "a", runoff_mm = c(100, 0, 100))
  threaded <- whep::calculate_n_leaching(
    x,
    method = "image_gnm",
    carry_over_by = "key"
  )
  single <- whep::calculate_n_leaching(x, method = "image_gnm")
  testthat::expect_identical(threaded, single)
  testthat::expect_equal(threaded$no3_n_t[1], threaded$no3_n_t[3])
  .expect_gnm_closes(threaded)
})

testthat::test_that("image_gnm aborts on an absent or missing driver", {
  x <- .gnm_rows()
  needed <- c(
    "n_input_std_t",
    "nh3_n_t",
    "area_ha",
    "runoff_mm",
    "surface_runoff_fraction",
    "slope_m_per_km",
    "land_use",
    "temp_c_annual_mean",
    "soil_field_capacity",
    "soil_wilting_point",
    "soil_texture_class",
    "soil_drainage_class",
    "soil_organic_carbon_pct"
  )
  for (col in needed) {
    err <- testthat::expect_error(
      whep::calculate_n_leaching(
        x[setdiff(names(x), col)],
        method = "image_gnm"
      ),
      class = "whep_missing_leaching_driver"
    )
    testthat::expect_s3_class(err, "whep_absent_input")
    testthat::expect_match(conditionMessage(err), col, fixed = TRUE)
  }
  for (col in c("soil_wilting_point", "runoff_mm")) {
    err <- testthat::expect_error(
      whep::calculate_n_leaching(
        dplyr::mutate(x, "{col}" := NA_real_),
        method = "image_gnm"
      ),
      class = "whep_missing_leaching_driver"
    )
    testthat::expect_s3_class(err, "whep_absent_input")
    testthat::expect_match(conditionMessage(err), col, fixed = TRUE)
  }
})

testthat::test_that("image_gnm aborts on an impossible driver value", {
  x <- .gnm_rows()
  bad <- list(
    dplyr::mutate(x, soil_wilting_point = 0.29),
    dplyr::mutate(x, soil_wilting_point = 0),
    dplyr::mutate(x, soil_field_capacity = 1.2),
    dplyr::mutate(x, soil_organic_carbon_pct = -1),
    dplyr::mutate(x, temp_c_annual_mean = Inf),
    dplyr::mutate(x, surface_runoff_fraction = -0.1),
    dplyr::mutate(x, surface_runoff_fraction = 1.2),
    dplyr::mutate(x, slope_m_per_km = -1),
    dplyr::mutate(x, area_ha = -1),
    dplyr::mutate(x, n_input_std_t = -1),
    dplyr::mutate(x, nh3_n_t = -1),
    dplyr::mutate(x, runoff_mm = -0.1),
    dplyr::mutate(x, runoff_mm = Inf)
  )
  for (row in bad) {
    testthat::expect_error(
      whep::calculate_n_leaching(row, method = "image_gnm"),
      class = "whep_leaching_driver_value"
    )
  }
})

testthat::test_that("the default Meisinger path is unchanged by IMAGE-GNM runoff", {
  # Golden values from calculate_n_leaching() at commit 8ab9ea6d, before
  # IMAGE-GNM computed its own surface runoff: the default
  # meisinger_drainage with swat_mixing, threaded and single-year, and the
  # burns transport.
  rows <- tibble::tibble(
    n_surplus_t = c(80, 100, 100, 60, 40, 55),
    fert_type = c(
      "Solid",
      "Synthetic",
      "Solid",
      "Liquid",
      "Synthetic",
      "Solid"
    ),
    climate = c("MED", "ATL", "MED", "ATL", "MED", "ATL"),
    irrig_cat = "Rainfed",
    land_use = "Cropland",
    cn_input = c(NA, 20, 40, NA, 12, 90),
    tillage = c(
      "Not_specified",
      "Tillage",
      "Not_specified",
      "Not_specified",
      "Tillage",
      "Not_specified"
    ),
    som_share = c(0.06, 0.015, 0.04, 0.03, 0.06, 0.03),
    root_depth_cm = c(100, 60, 120, 90, 100, 40),
    soil_porosity = c(0.45, 0.40, 0.5, 0.42, 0.45, 0.38),
    soil_field_capacity = 0.30,
    temp_c_annual_mean = 15,
    cell = c("a", "b", "a", "a", "b", "c"),
    year = c(2003L, 2001L, 2001L, 2002L, 2003L, 2001L),
    drainage_mm = c(600, 250, 900, 0, 45, 3500)
  )
  threaded <- whep::calculate_n_leaching(
    rows,
    drainage_mm = "drainage_mm",
    carry_over_by = "cell"
  )
  testthat::expect_equal(
    threaded$no3_n_t,
    c(
      47.776632259147078,
      40.202953823230345,
      46.677254954609317,
      0,
      5.594215086321622,
      5.9193750000000005
    ),
    tolerance = 1e-14
  )
  testthat::expect_equal(
    threaded$retained_profile_n_t,
    c(
      5.4885731545569296,
      11.435596323446291,
      3.762595820994898,
      9.7625958209948962,
      31.583928384682689,
      0
    ),
    tolerance = 1e-14
  )
  single <- whep::calculate_n_leaching(rows, drainage_mm = "drainage_mm")
  testthat::expect_equal(
    single$denitrification_n_t,
    c(
      34.646396839747325,
      59.797046176769655,
      53.32274504539069,
      54,
      28.727969652569072,
      49.080624999999998
    ),
    tolerance = 1e-14
  )
  burns <- whep::calculate_n_leaching(
    rows,
    drainage_mm = "drainage_mm",
    transport = "burns"
  )
  testthat::expect_equal(
    burns$no3_n_t,
    c(
      44.255871371805469,
      38.310016919174416,
      44.660373594788638,
      0,
      3.4040056852627241,
      5.8493663043936923
    ),
    tolerance = 1e-14
  )
  testthat::expect_false(rlang::has_name(threaded, "surface_runoff_n_t"))
  testthat::expect_false(rlang::has_name(threaded, "erosion_n_t"))
})

testthat::test_that("image_gnm aborts on a class outside its tables", {
  x <- .gnm_rows()
  bad <- list(
    dplyr::mutate(x, soil_texture_class = "loam"),
    dplyr::mutate(x, soil_drainage_class = "Well"),
    dplyr::mutate(x, land_use = "Forest")
  )
  for (row in bad) {
    testthat::expect_error(
      whep::calculate_n_leaching(row, method = "image_gnm"),
      class = "whep_leaching_class_value"
    )
  }
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

testthat::test_that("the vectorised bin lookup equals the per-value filter", {
  # The per-value filter it replaced: the first bin in table order whose
  # strictly-open range holds the value, NA on an edge, outside or NA.
  reference <- function(values, ranges, label_col, min_col, max_col) {
    purrr::map_chr(values, function(v) {
      hit <- ranges[
        v > ranges[[min_col]] & v < ranges[[max_col]],
        ,
        drop = FALSE
      ]
      hit[[label_col]][1]
    })
  }
  withr::local_seed(1)
  drainage <- c(
    -0.2,
    -0.1,
    0,
    0.1,
    50,
    200,
    500,
    1000,
    3000,
    4000,
    NA,
    stats::runif(200, -1, 3500)
  )
  som <- c(0, 0.02, 0.05, 1, 1.2, NA, stats::runif(200, -0.1, 1.1))
  bins <- list(
    list(drainage, whep::drainage_ranges, "drainage_rate", "s_min", "s_max"),
    list(som, whep::som_ranges, "som_content", "som_min", "som_max")
  )
  for (b in bins) {
    testthat::expect_identical(
      whep:::.bin_range(b[[1]], b[[2]], b[[3]], b[[4]], b[[5]]),
      reference(b[[1]], b[[2]], b[[3]], b[[4]], b[[5]])
    )
  }
})

testthat::test_that("pooled excreta reads the species' shared subsoil reduction", {
  x <- tibble::tibble(
    fert_type = c("Excreta", "Excreta_other", "Synthetic"),
    climate = "ATL",
    irrig_cat = "Rainfed"
  )
  testthat::expect_equal(whep:::.leaching_no3_red(x), c(0.70, 0.70, 0.50))
  diverged <- dplyr::mutate(
    whep::subsoil_no3_reduction,
    no3_red = dplyr::if_else(
      .data$fert_type == "Excreta_other",
      .data$no3_red + 0.01,
      .data$no3_red
    )
  )
  testthat::expect_error(
    whep:::.leaching_subsoil_excreta("Excreta", diverged),
    class = "whep_leaching_excreta_species"
  )
})
