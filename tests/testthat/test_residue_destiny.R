test_that("krausmann_regional split is mass-conserving and feeds livestock", {
  x <- tibble::tibble(
    item_prod_code = "15",
    residue_dm_t = 100,
    region_krausmann = "West Europe",
    region_un_sub = "Western Europe"
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
  out <- whep::calculate_residue_destinies(tibble::tibble(
    item_prod_code = "15",
    residue_dm_t = 100,
    region_krausmann = "Nowhere",
    region_un_sub = "Nowhere"
  ))
  testthat::expect_equal(
    out$residue_feed_dm_t + out$residue_burn_dm_t + out$residue_soil_dm_t,
    100
  )
  testthat::expect_equal(out$residue_soil_dm_t, 100)
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
    region_un_sub = "Western Europe"
  ))
  testthat::expect_gt(out$residue_feed_dm_t, 0)
  testthat::expect_gt(out$residue_burn_dm_t, 0)
  testthat::expect_equal(
    out$residue_feed_dm_t + out$residue_burn_dm_t + out$residue_soil_dm_t,
    100
  )
})

.toy_residue_row <- function() {
  tibble::tibble(
    item_prod_code = "15",
    residue_dm_t = 100,
    region_krausmann = "West Europe",
    region_un_sub = "Western Europe"
  )
}

test_that("bedding_fraction defaults to zero and moves nothing", {
  # The default has to reproduce the published split exactly: bedding is a
  # science decision with no sourced fraction (whep#1005), so switching it on
  # must be the caller's act, not this function's.
  out <- whep::calculate_residue_destinies(.toy_residue_row())
  testthat::expect_equal(out$residue_bedding_dm_t, 0)
  testthat::expect_equal(out$bedding_fraction, 0)
  testthat::expect_equal(out$residue_feed_dm_t, 10.5)
  testthat::expect_equal(out$residue_burn_dm_t, 59.5)
  testthat::expect_equal(out$residue_soil_dm_t, 30)
})

test_that("bedding is carved out of the non-feed removal, not out of feed", {
  base <- whep::calculate_residue_destinies(.toy_residue_row())
  out <- whep::calculate_residue_destinies(
    .toy_residue_row(),
    bedding_fraction = 0.25
  )
  testthat::expect_equal(out$residue_feed_dm_t, base$residue_feed_dm_t)
  testthat::expect_equal(out$residue_soil_dm_t, base$residue_soil_dm_t)
  testthat::expect_equal(out$residue_bedding_dm_t, 0.25 * 59.5)
  testthat::expect_equal(out$residue_burn_dm_t, 0.75 * 59.5)
  testthat::expect_equal(
    out$residue_feed_dm_t +
      out$residue_bedding_dm_t +
      out$residue_burn_dm_t +
      out$residue_soil_dm_t,
    100
  )
})

test_that("the shares method also carries a bedding destiny", {
  x <- tibble::tibble(item_prod_code = "15", residue_dm_t = 100, year = 1950)
  out <- suppressWarnings(
    whep::calculate_residue_destinies(
      x,
      method = "shares",
      bedding_fraction = 0.5
    )
  )
  testthat::expect_equal(
    out$residue_feed_dm_t +
      out$residue_bedding_dm_t +
      out$residue_burn_dm_t +
      out$residue_soil_dm_t,
    100
  )
  testthat::expect_equal(out$bedding_fraction, 0.5)
})

test_that("bedding_fraction is validated", {
  x <- .toy_residue_row()
  testthat::expect_error(
    whep::calculate_residue_destinies(x, bedding_fraction = 1.5),
    class = "whep_error_bedding_fraction"
  )
  testthat::expect_error(
    whep::calculate_residue_destinies(x, bedding_fraction = -0.1),
    class = "whep_error_bedding_fraction"
  )
  testthat::expect_error(
    whep::calculate_residue_destinies(x, bedding_fraction = c(0.1, 0.2)),
    class = "whep_error_bedding_fraction"
  )
  testthat::expect_error(
    whep::calculate_residue_destinies(x, bedding_fraction = NA_real_),
    class = "whep_error_bedding_fraction"
  )
})

test_that("build_residue_bedding_supply carries residue C and N", {
  # Straw composition comes from the same bio_coefs residue columns the
  # crop-NPP and nitrogen-balance paths use, so the bedding that reaches the
  # manure heap is the same straw the rest of the package describes.
  coefs <- whep::whep_coef_table("bio_coefs")
  wheat <- coefs[coefs$item_prod_code == 15, ]
  out <- tibble::tibble(
    item_prod_code = "15",
    year = 2020L,
    territory = "203",
    residue_bedding_dm_t = 1000
  ) |>
    whep::build_residue_bedding_supply()
  testthat::expect_equal(out$bedding_dm_t, 1000)
  testthat::expect_equal(out$bedding_c_t, 1000 * wheat$residue_c_kgdm)
  testthat::expect_equal(out$bedding_n_t, 1000 * wheat$residue_n_kgdm)
  testthat::expect_true(is.na(out$sub_territory))
  # Straw C:N is far wider than the excreta it is mixed with, which is the
  # whole reason bedding raises the C:N of stored farmyard manure.
  testthat::expect_gt(out$bedding_c_t / out$bedding_n_t, 40)
})

test_that("build_residue_bedding_supply names bedding it cannot convert", {
  # A crop with no bio_coefs residue row would otherwise be summed away to
  # zero carbon and zero nitrogen, losing exactly what this traces.
  testthat::expect_warning(
    tibble::tibble(
      item_prod_code = "not-a-crop",
      year = 2020L,
      territory = "203",
      residue_bedding_dm_t = 1000
    ) |>
      whep::build_residue_bedding_supply(),
    "residue composition"
  )
})

test_that("build_residue_bedding_supply requires its input columns", {
  testthat::expect_error(
    whep::build_residue_bedding_supply(
      tibble::tibble(item_prod_code = "15", year = 2020L)
    ),
    "territory"
  )
})
