# Input-validation and error-path coverage. Confirms each function fails loudly
# and specifically on bad input rather than producing silent nonsense.

test_that("estimation functions require their columns", {
  bad <- tibble::tibble(item_prod_code = "15")
  testthat::expect_error(whep::calculate_crop_residues(bad), "missing required")
  testthat::expect_error(whep::calculate_crop_roots(bad), "missing required")
  testthat::expect_error(
    whep::calculate_npp_carbon_nitrogen(bad),
    "missing required"
  )
  testthat::expect_error(whep::calculate_crop_bnf(bad), "missing required")
  testthat::expect_error(
    whep::calculate_weed_bnf(tibble::tibble(x = 1)),
    "missing required"
  )
  testthat::expect_error(
    whep::calculate_nonsymbiotic_bnf(tibble::tibble(x = 1)),
    "missing required"
  )
  testthat::expect_error(
    whep::calculate_residue_destinies(bad),
    "missing required"
  )
  testthat::expect_error(
    whep::build_residue_feed_avail(bad),
    "missing required"
  )
  testthat::expect_error(
    whep::calculate_crop_npp_components(bad),
    "missing required"
  )
})

test_that("method arguments are validated", {
  res <- tibble::tibble(item_prod_code = "15", production_t = 100, area_ha = 40)
  testthat::expect_error(whep::calculate_crop_residues(res, method = "bogus"))
  testthat::expect_error(
    whep::calculate_potential_npp(tibble::tibble(temp_c = 1), method = "bogus")
  )
  testthat::expect_error(
    whep::calculate_residue_destinies(
      tibble::tibble(item_prod_code = "15", residue_dm_t = 1),
      method = "bogus"
    )
  )
})

test_that("ensemble weights must be in [0, 1]", {
  res <- tibble::tibble(item_prod_code = "15", production_t = 100, area_ha = 40)
  testthat::expect_error(
    whep::calculate_crop_residues(res, weights = list(w_ipcc = 2)),
    "between 0 and 1"
  )
  testthat::expect_error(
    whep::calculate_crop_residues(res, weights = list(w_ipcc = -0.1)),
    "between 0 and 1"
  )
  k <- tibble::tibble(
    item_prod_code = "15",
    product_dm_t = 1,
    residue_dm_t = 1,
    area_ha = 1
  )
  testthat::expect_error(
    whep::calculate_crop_roots(k, weights = list(w_ref = 1.5)),
    "between 0 and 1"
  )
})

test_that("residue destinies need their region / year drivers", {
  testthat::expect_error(
    whep::calculate_residue_destinies(
      tibble::tibble(item_prod_code = "15", residue_dm_t = 1)
    ),
    "region"
  )
  testthat::expect_error(
    whep::calculate_residue_destinies(
      tibble::tibble(item_prod_code = "15", residue_dm_t = 1),
      method = "shares"
    ),
    "year"
  )
})

test_that("lpjml potential NPP needs grid coordinates", {
  testthat::expect_error(
    whep::calculate_potential_npp(
      tibble::tibble(year = 2000),
      method = "lpjml"
    ),
    "lon"
  )
})

test_that("zero-row BNF input warns and returns zero rows", {
  empty <- tibble::tibble(
    item_prod_code = character(),
    crop_npp_n_t = numeric(),
    product_n_t = numeric()
  )
  testthat::expect_warning(whep::calculate_crop_bnf(empty), "zero rows")
  out <- suppressWarnings(whep::calculate_crop_bnf(empty))
  testthat::expect_equal(nrow(out), 0L)
})
