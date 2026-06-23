.npp_coef_ref <- function(coefs, model, param, component = NULL) {
  m <- coefs$model == model & coefs$parameter == param
  if (!is.null(component)) {
    m <- m & coefs$component == component
  }
  coefs$value[m]
}

test_that("calculate_potential_npp miami matches the afsetools formula", {
  x <- tibble::tibble(temp_c = 15, water_input_mm = 600, aet_mm = 450)
  out <- whep::calculate_potential_npp(x, method = "miami")

  testthat::expect_true(
    all(c("npp_potential_dm_t_ha", "method_npp_potential") %in% names(out))
  )
  testthat::expect_equal(out$method_npp_potential, "miami")

  coefs <- whep::whep_coef_table("npp_model_coefs")
  t_max <- .npp_coef_ref(coefs, "Miami", "Max_gCm2yr", "F_MAT")
  t_mid <- .npp_coef_ref(coefs, "Miami", "Midpoint", "F_MAT")
  t_rate <- .npp_coef_ref(coefs, "Miami", "Rate", "F_MAT")
  p_max <- .npp_coef_ref(coefs, "Miami", "Max_gCm2yr", "F_MAP")
  p_rate <- .npp_coef_ref(coefs, "Miami", "Rate", "F_MAP")
  expected <- pmin(
    (t_max / (1 + exp(t_mid - t_rate * 15))) / 100,
    (p_max * (1 - exp(-p_rate * 600))) / 100
  )
  testthat::expect_equal(out$npp_potential_dm_t_ha, expected)
})

test_that("calculate_potential_npp nceas uses non-tree ANPP", {
  x <- tibble::tibble(temp_c = 15, water_input_mm = 600, aet_mm = 450)
  out <- whep::calculate_potential_npp(x, method = "nceas")

  coefs <- whep::whep_coef_table("npp_model_coefs")
  carbon_fraction <- whep::whep_coef_table("weed_coefs")$residue_c_kgdm_weed
  a_max <- .npp_coef_ref(coefs, "NCEAS_nontree_ANPP", "Max_gCm2yr")
  a_rate <- .npp_coef_ref(coefs, "NCEAS_nontree_ANPP", "Rate")
  expected <- a_max * (1 - exp(-a_rate * 600)) / (100 * carbon_fraction)

  testthat::expect_equal(out$npp_potential_dm_t_ha, expected)
  testthat::expect_gt(out$npp_potential_dm_t_ha, 0)
})

test_that("lpjml potential-NPP method is not yet wired", {
  testthat::expect_error(
    whep::calculate_potential_npp(
      tibble::tibble(temp_c = 1),
      method = "lpjml"
    ),
    "not yet wired"
  )
})
