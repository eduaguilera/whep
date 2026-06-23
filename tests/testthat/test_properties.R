# Property / invariant tests. These assert structural and mathematical
# properties that must hold regardless of the coefficient values, so they
# complement the golden-value and afsetools-equivalence regressions and run on
# CI without afsetools.

test_that("residue destinies conserve mass and stay non-negative (both methods)", {
  x <- tibble::tibble(
    item_prod_code = c("15", "56", "176"),
    residue_dm_t = c(80, 120, 30),
    region_krausmann = "West Europe",
    region_hanpp = "Western Europe",
    year = 1980
  )
  for (m in c("krausmann_regional", "shares")) {
    out <- suppressWarnings(whep::calculate_residue_destinies(x, method = m))
    testthat::expect_equal(
      out$residue_feed_dm_t + out$residue_burn_dm_t + out$residue_soil_dm_t,
      out$residue_dm_t
    )
    testthat::expect_true(all(out$residue_feed_dm_t >= 0))
    testthat::expect_true(all(out$residue_burn_dm_t >= 0))
    testthat::expect_true(all(out$residue_soil_dm_t >= 0))
  }
})

test_that("crop NPP and its carbon/nitrogen partition are additive", {
  out <- tibble::tibble(
    item_prod_code = c("15", "56"),
    production_t = c(100, 200),
    area_ha = c(40, 60)
  ) |>
    whep::calculate_crop_npp() |>
    whep::calculate_npp_carbon_nitrogen()
  testthat::expect_equal(
    out$crop_npp_dm_t,
    out$product_dm_t + out$residue_dm_t + out$root_dm_t
  )
  testthat::expect_equal(
    out$crop_npp_n_t,
    out$product_n_t + out$residue_n_t + out$root_n_t
  )
  testthat::expect_equal(
    out$crop_npp_c_t,
    out$product_c_t + out$residue_c_t + out$root_c_t
  )
  testthat::expect_equal(
    out$total_npp_dm_t,
    out$crop_npp_dm_t + out$weed_npp_dm_t
  )
})

test_that("residue ensemble lies between the ipcc and ratio estimates", {
  x <- tibble::tibble(item_prod_code = "15", production_t = 100, area_ha = 40)
  e <- whep::calculate_crop_residues(x, method = "ensemble")$residue_dm_t
  i <- whep::calculate_crop_residues(x, method = "ipcc")$residue_dm_t
  r <- whep::calculate_crop_residues(x, method = "ratio")$residue_dm_t
  testthat::expect_gte(e, min(i, r))
  testthat::expect_lte(e, max(i, r))
})

test_that("nitrogen inhibition of BNF is monotone decreasing and bounded", {
  f <- function(n) {
    whep::calculate_crop_bnf(tibble::tibble(
      item_prod_code = "176",
      crop_npp_n_t = 10,
      product_n_t = 5,
      n_synth_kg_ha = n
    ))$f_nitrogen_symbiotic
  }
  vals <- vapply(c(0, 50, 100, 200), f, numeric(1))
  testthat::expect_equal(vals[1], 1)
  testthat::expect_true(all(diff(vals) < 0))
  testthat::expect_true(all(vals > 0 & vals <= 1))
})

test_that("more synthetic N lowers crop BNF; more N input lowers roots", {
  bnf <- function(n) {
    whep::calculate_crop_bnf(tibble::tibble(
      item_prod_code = "176",
      crop_npp_n_t = 10,
      product_n_t = 5,
      n_synth_kg_ha = n
    ))$crop_bnf_t
  }
  testthat::expect_lt(bnf(200), bnf(0))
  rt <- function(n) {
    whep::calculate_crop_roots(tibble::tibble(
      item_prod_code = "15",
      product_dm_t = 87.9,
      residue_dm_t = 135.7,
      area_ha = 40,
      n_input_kg_ha = n
    ))$root_dm_t
  }
  testthat::expect_lt(rt(250), rt(10))
})

test_that("inhibitory BNF environmental factors stay in (0, 1]", {
  out <- whep::calculate_nonsymbiotic_bnf(tibble::tibble(
    area_ha = 40,
    n_synth_kg_ha = 120,
    temp_c = 30,
    water_input_mm = 200,
    pet_mm = 1200,
    som_pct = 1,
    soil_ph = 8.5,
    clay_pct = 5
  ))
  for (f in c(
    "f_nitrogen_nonsymbiotic",
    "f_temperature_nonsymbiotic",
    "f_water_nonsymbiotic"
  )) {
    testthat::expect_true(out[[f]] > 0 && out[[f]] <= 1, info = f)
  }
  testthat::expect_true(out$f_env_nonsymbiotic > 0)
})

test_that("total BNF equals the sum of its three components", {
  x <- tibble::tibble(
    item_prod_code = "176",
    crop_npp_n_t = 10,
    product_n_t = 5,
    weed_npp_n_t = 4,
    land_use = "Cropland",
    legumes_seeded = 0,
    seeded_cover_crop_share = 0,
    area_ha = 40
  )
  out <- whep::calculate_bnf(x)
  testthat::expect_equal(
    out$bnf_t,
    out$crop_bnf_t + out$weed_bnf_t + out$nonsymbiotic_bnf_t
  )
})

test_that("crop residues is row-wise (batch equals per-row)", {
  batch <- whep::calculate_crop_residues(tibble::tibble(
    item_prod_code = c("15", "56"),
    production_t = c(100, 200),
    area_ha = c(40, 60)
  ))$residue_dm_t
  one1 <- whep::calculate_crop_residues(
    tibble::tibble(item_prod_code = "15", production_t = 100, area_ha = 40)
  )$residue_dm_t
  one2 <- whep::calculate_crop_residues(
    tibble::tibble(item_prod_code = "56", production_t = 200, area_ha = 60)
  )$residue_dm_t
  testthat::expect_equal(batch, c(one1, one2))
})
