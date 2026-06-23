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
