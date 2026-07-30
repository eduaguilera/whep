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

test_that("build_residue_feed_avail yields the redistribute_feed contract", {
  x <- tibble::tibble(
    item_prod_code = "15",
    year = 2000,
    sub_territory = "ESP",
    residue_feed_dm_t = 50
  )
  out <- whep::build_residue_feed_avail(x)
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

test_that("calculate_residue_destinies conserves mass with an unmatched region", {
  # Mass conservation holds whether or not the region matched, which is exactly why an
  # unmatched region is hard to notice: nothing about the output looks wrong. The row simply
  # sends 100% of its residue to soil, because `recovery_rates` falls back to 0 rather than to
  # a central estimate.
  #
  # So the fallbacks now announce themselves, and this test asserts that they do. It is the
  # only test in the suite that exercises an unmatched region, so without the assertion the
  # diagnostic could be deleted and nothing would notice. See whep#405, where this same
  # silence hid a real vocabulary mismatch that killed all 17 regional feed coefficients.
  x <- tibble::tibble(
    item_prod_code = "15",
    residue_dm_t = 100,
    region_krausmann = "Nowhere",
    region_hanpp = "Nowhere"
  )

  msgs <- character()
  out <- withCallingHandlers(
    whep::calculate_residue_destinies(x),
    warning = function(w) {
      msgs <<- c(msgs, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  testthat::expect_equal(
    out$residue_feed_dm_t + out$residue_burn_dm_t + out$residue_soil_dm_t,
    100
  )
  testthat::expect_equal(out$residue_soil_dm_t, 100)

  joined <- paste(msgs, collapse = " ")
  # Both fallbacks fired, and each says which value did not match and what it cost.
  testthat::expect_match(joined, "feed_use_fraction", fixed = TRUE)
  testthat::expect_match(joined, "recovery_rates", fixed = TRUE)
  testthat::expect_match(joined, "Nowhere", fixed = TRUE)
  testthat::expect_match(joined, "all of that residue to soil", fixed = TRUE)
})

test_that("region-map guard rejects a krausmann label with two HANPP regions", {
  # The real regions_full map is 1:1, so calculate_residue_destinies works; the
  # guard exists so a future fan-out (one Krausmann label -> several HANPP
  # regions) aborts loudly instead of silently keeping the first (relates #170).
  fan_out <- tibble::tibble(
    input_region = c("Western Europe", "Western Europe"),
    recovery_region = c("West Europe", "North America and Oceania")
  )
  testthat::expect_error(
    whep:::.assert_unique_region_map(fan_out),
    "region_HANPP"
  )
  one_to_one <- tibble::tibble(
    input_region = c("Western Europe", "Eastern Asia"),
    recovery_region = c("West Europe", "East Asia")
  )
  testthat::expect_no_error(whep:::.assert_unique_region_map(one_to_one))
})

test_that("krausmann split accepts regions_full recovery labels", {
  out <- whep::calculate_residue_destinies(tibble::tibble(
    item_prod_code = "15",
    residue_dm_t = 100,
    region_krausmann = "Western Europe",
    region_hanpp = "Western Europe"
  ))
  testthat::expect_gt(out$residue_feed_dm_t, 0)
  testthat::expect_gt(out$residue_burn_dm_t, 0)
  testthat::expect_equal(
    out$residue_feed_dm_t + out$residue_burn_dm_t + out$residue_soil_dm_t,
    100
  )
})
