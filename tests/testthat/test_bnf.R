test_that("calculate_crop_bnf leaves modifiers at 1 with no drivers", {
  x <- tibble::tibble(
    item_prod_code = "176",
    crop_npp_n_t = 10,
    product_n_t = 5
  )
  out <- whep::calculate_crop_bnf(x)
  nb <- whep::whep_coef_table("names_bnf")
  bnf <- whep::whep_coef_table("bnf")
  p <- bnf[bnf$name_bnf == nb$name_bnf[nb$item_prod_code == "176"], ]

  testthat::expect_equal(out$f_env_symbiotic, 1)
  testthat::expect_equal(out$ndfa_adj, p$ndfa)
  testthat::expect_equal(out$crop_bnf_t, 10 * p$ndfa * p$leguminous_share)
  testthat::expect_equal(
    out$crop_bnf_anglade_t,
    5 * p$leguminous_share * p$ndfa * p$below_ground_n_ratio / p$n_harvest_index
  )
  testthat::expect_equal(out$bnf_product_ratio_npp, out$crop_bnf_t / 5)
})

test_that("calculate_crop_bnf nitrogen inhibition lowers fixation", {
  x <- tibble::tibble(
    item_prod_code = "176",
    crop_npp_n_t = 10,
    product_n_t = 5,
    n_synth_kg_ha = 200
  )
  out <- whep::calculate_crop_bnf(x)
  testthat::expect_equal(out$f_nitrogen_symbiotic, exp(-0.0035 * 200))
  testthat::expect_lt(out$crop_bnf_t, 10 * 0.40)
})

test_that("calculate_crop_bnf errors on missing columns", {
  testthat::expect_error(
    whep::calculate_crop_bnf(tibble::tibble(item_prod_code = "176")),
    "missing required"
  )
})

test_that("calculate_weed_bnf weights spontaneous and seeded legumes", {
  x <- tibble::tibble(
    weed_npp_n_t = 10,
    land_use = "Cropland",
    legumes_seeded = 0,
    seeded_cover_crop_share = 0
  )
  out <- whep::calculate_weed_bnf(x)
  bnf <- whep::whep_coef_table("bnf")
  legs <- whep::whep_coef_table("legs_spontweeds")
  ref <- bnf$ndfa[bnf$name_bnf == "Weeds"]
  spont <- legs$legumes_spontaneous[legs$land_use == "Cropland"]
  testthat::expect_equal(out$weed_ndfa_ref, ref)
  testthat::expect_equal(out$weed_leg_share, spont)
  testthat::expect_equal(out$weed_bnf_t, 10 * ref * spont)
})

test_that("calculate_nonsymbiotic_bnf uses base rate and area, no drivers", {
  out <- whep::calculate_nonsymbiotic_bnf(tibble::tibble(area_ha = 40))
  testthat::expect_equal(out$f_env_nonsymbiotic, 1)
  testthat::expect_equal(out$nonsymbiotic_base_kg_ha, 5)
  testthat::expect_equal(out$nonsymbiotic_bnf_t, 5 * 40 / 1000)
})

test_that("calculate_nonsymbiotic_bnf nitrogen inhibition is stronger", {
  out <- whep::calculate_nonsymbiotic_bnf(
    tibble::tibble(area_ha = 40, n_synth_kg_ha = 100)
  )
  testthat::expect_equal(out$f_nitrogen_nonsymbiotic, exp(-0.005 * 100))
})

test_that("calculate_nonsymbiotic_bnf joins the crop-specific base rate", {
  nb <- whep::whep_coef_table("names_bnf")
  bnf <- whep::whep_coef_table("bnf")
  m <- merge(nb, bnf, by = "name_bnf")
  hit <- m[!is.na(m$nonsymbiotic_base_kg_ha) & m$nonsymbiotic_base_kg_ha > 5, ][
    1,
  ]
  out <- whep::calculate_nonsymbiotic_bnf(
    tibble::tibble(
      item_prod_code = as.character(hit$item_prod_code),
      area_ha = 40
    )
  )
  testthat::expect_equal(
    out$nonsymbiotic_base_kg_ha,
    hit$nonsymbiotic_base_kg_ha
  )
})
