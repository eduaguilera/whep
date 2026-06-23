# Golden-value regression. These exact outputs were verified byte-identical to
# the afsetools originals (see test_afsetools_equivalence.R) and are hardcoded
# here so CI catches any coefficient or formula drift WITHOUT afsetools. The
# adjustment cases use crops/years where the factor is genuinely != 1 (wheat
# modern-variety in 1960; maize irrigation, which is sensitive).

tol <- 1e-5

test_that("crop-NPP golden values", {
  base <- tibble::tibble(
    item_prod_code = "15",
    production_t = 100,
    area_ha = 40
  )

  testthat::expect_equal(
    whep::calculate_crop_residues(base)$residue_dm_t,
    135.746693,
    tolerance = tol
  )
  testthat::expect_equal(
    whep::calculate_crop_residues(base, method = "ipcc")$residue_dm_t,
    153.529,
    tolerance = tol
  )
  testthat::expect_equal(
    whep::calculate_crop_residues(base, method = "ratio")$residue_dm_t,
    117.964387,
    tolerance = tol
  )
  testthat::expect_equal(
    whep::calculate_crop_residues(
      dplyr::mutate(base, year = 1960, region_hanpp = "West Europe")
    )$residue_dm_t,
    195.475238,
    tolerance = tol
  )
  testthat::expect_equal(
    whep::calculate_crop_residues(tibble::tibble(
      item_prod_code = "56",
      production_t = 200,
      area_ha = 60,
      water_regime = "Irrigated"
    ))$residue_dm_t,
    171.017715,
    tolerance = tol
  )

  k <- tibble::tibble(
    item_prod_code = "15",
    product_dm_t = 87.9,
    residue_dm_t = 135.746693,
    area_ha = 40
  )
  testthat::expect_equal(
    whep::calculate_crop_roots(k)$root_dm_t,
    66.319370,
    tolerance = tol
  )
  testthat::expect_equal(
    whep::calculate_crop_roots(dplyr::mutate(k, n_input_kg_ha = 150))$root_dm_t,
    63.747433,
    tolerance = tol
  )
  testthat::expect_equal(
    whep::calculate_crop_roots(dplyr::mutate(
      k,
      water_regime = "Irrigated"
    ))$root_dm_t,
    62.461464,
    tolerance = tol
  )

  cn <- base |>
    whep::calculate_crop_npp() |>
    whep::calculate_npp_carbon_nitrogen()
  testthat::expect_equal(cn$crop_npp_dm_t, 289.966063, tolerance = tol)
  testthat::expect_equal(cn$crop_npp_n_t, 4.362475, tolerance = tol)
  testthat::expect_equal(cn$crop_npp_c_t, 173.381678, tolerance = tol)
})

test_that("BNF golden values", {
  b <- tibble::tibble(
    item_prod_code = "176",
    crop_npp_n_t = 10,
    product_n_t = 5
  )
  testthat::expect_equal(
    whep::calculate_crop_bnf(b)$crop_bnf_t,
    4.0,
    tolerance = tol
  )
  testthat::expect_equal(
    whep::calculate_crop_bnf(b)$crop_bnf_anglade_t,
    3.466667,
    tolerance = tol
  )
  testthat::expect_equal(
    whep::calculate_crop_bnf(dplyr::mutate(
      b,
      n_synth_kg_ha = 100,
      temp_c = 18,
      water_input_mm = 400,
      pet_mm = 800
    ))$crop_bnf_t,
    3.052387,
    tolerance = tol
  )
  testthat::expect_equal(
    whep::calculate_weed_bnf(tibble::tibble(
      weed_npp_n_t = 10,
      land_use = "Cropland",
      legumes_seeded = 0.5,
      seeded_cover_crop_share = 0.3,
      n_synth_kg_ha = 50,
      temp_c = 16
    ))$weed_bnf_t,
    1.029963,
    tolerance = tol
  )
  testthat::expect_equal(
    whep::calculate_nonsymbiotic_bnf(tibble::tibble(
      area_ha = 40,
      n_synth_kg_ha = 80,
      temp_c = 20,
      water_input_mm = 500,
      pet_mm = 900,
      som_pct = 3,
      soil_ph = 6.5,
      clay_pct = 30
    ))$nonsymbiotic_bnf_t,
    0.182404,
    tolerance = tol
  )
})

test_that("residue-destiny golden values (wheat, West Europe)", {
  d <- whep::calculate_residue_destinies(tibble::tibble(
    item_prod_code = "15",
    residue_dm_t = 100,
    region_krausmann = "West Europe",
    region_hanpp = "Western Europe"
  ))
  testthat::expect_equal(d$residue_feed_dm_t, 10.5, tolerance = tol)
  testthat::expect_equal(d$residue_burn_dm_t, 59.5, tolerance = tol)
  testthat::expect_equal(d$residue_soil_dm_t, 30.0, tolerance = tol)
})
