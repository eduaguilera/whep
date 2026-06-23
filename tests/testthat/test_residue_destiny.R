test_that("krausmann_regional split is mass-conserving and feeds livestock", {
  x <- tibble::tibble(
    item_prod_code = "15",
    residue_dm_t = 100,
    region_krausmann = "West Europe",
    region_hanpp = "Western Europe"
  )
  out <- whep::calculate_residue_destinies(x)
  testthat::expect_equal(
    out$residue_feed_dm_t + out$residue_burn_dm_t + out$residue_soil_dm_t,
    100
  )
  testthat::expect_gt(out$residue_feed_dm_t, 0)
  testthat::expect_equal(out$method_residue_destiny, "krausmann_regional")
})

test_that("shares method splits use/burn/soil and flags provisional", {
  x <- tibble::tibble(item_prod_code = "15", residue_dm_t = 100, year = 1950)
  out <- suppressWarnings(
    whep::calculate_residue_destinies(x, method = "shares")
  )
  sh <- whep::whep_coef_table("residue_shares")
  r <- sh[sh$item_prod_code == "15" & sh$year == 1950, ]
  testthat::expect_equal(out$residue_feed_dm_t, 100 * r$use_share)
  testthat::expect_true(out$residue_destiny_to_be_revised)
})

test_that("build_residue_feed_availability yields the redistribute_feed contract", {
  x <- tibble::tibble(
    item_prod_code = "15",
    year = 2000,
    sub_territory = "ESP",
    residue_feed_dm_t = 50
  )
  out <- whep::build_residue_feed_availability(x)
  required <- c(
    "year",
    "sub_territory",
    "item_cbs_code",
    "feed_group",
    "feed_quality",
    "avail_dm_t",
    "feed_scale"
  )
  testthat::expect_true(all(required %in% names(out)))
  testthat::expect_equal(out$feed_quality, "residues")
  testthat::expect_equal(out$avail_dm_t, 50 * 0.85)
  testthat::expect_equal(out$item_cbs_code, 2105)
})
