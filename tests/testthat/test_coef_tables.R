test_that("bio_coefs loads, is item-keyed and unique", {
  x <- whep::whep_coef_table("bio_coefs")
  testthat::expect_true(
    all(c("item_prod_code", "item_prod_name", "name_biomass") %in% names(x))
  )
  testthat::expect_equal(nrow(x), dplyr::n_distinct(x$item_prod_code))
})

test_that("residue_feed_fraction has a global fallback row", {
  x <- whep::whep_coef_table("residue_feed_fraction")
  testthat::expect_true(any(x$region_hanpp == "Global"))
})

test_that("natural-grain ipcc tables share the snake_case ipcc_crop key", {
  res <- whep::whep_coef_table("ipcc_residue_coefs")
  root <- whep::whep_coef_table("ipcc_root_coefs")
  testthat::expect_true("ipcc_crop" %in% names(res))
  testthat::expect_true("ipcc_crop" %in% names(root))
})

test_that("unknown coefficient table errors", {
  testthat::expect_error(
    whep::whep_coef_table("does_not_exist"),
    "Unknown coefficient table"
  )
})
