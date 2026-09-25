test_that("recovery_regional split is mass-conserving and feeds livestock", {
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
  testthat::expect_equal(out$method_residue_destiny, "recovery_regional")
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
  testthat::expect_warning(
    out <- whep::build_residue_feed_avail(x),
    class = "whep_residue_feed_avail_deprecated"
  )
  required <- c(
    "year",
    "territory",
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

# whep#1138: the output carried the country only in `sub_territory`, which
# redistribute_feed() blanks for feed_scale = "national" -- so every country's
# residue was pooled into one "All_territories" row that no territory's demand
# could reach, and the documented pipe served zero intake.
test_that("build_residue_feed_avail output reaches each territory's demand", {
  x <- tibble::tibble(
    item_prod_code = "15",
    year = 2000L,
    sub_territory = c("ESP", "FRA"),
    residue_feed_dm_t = c(50, 1000)
  )
  avail <- suppressWarnings(whep::build_residue_feed_avail(x))
  demand <- tibble::tribble(
    ~year, ~territory, ~sub_territory, ~livestock_category,
    ~item_cbs_code, ~feed_group, ~feed_quality, ~demand_dm_t, ~fixed_demand,
    2000L, "ESP", "ESP", "Cattle_meat",
    NA_integer_, NA_character_, "residues", 100, FALSE,
    2000L, "FRA", "FRA", "Cattle_meat",
    NA_integer_, NA_character_, "residues", 100, FALSE
  )
  intake <- whep::redistribute_feed(
    demand,
    avail,
    options = list(distribute_surplus = FALSE)
  ) |>
    dplyr::summarise(intake = sum(intake_dm_t), .by = territory) |>
    dplyr::arrange(territory)
  testthat::expect_equal(intake$territory, c("ESP", "FRA"))
  # ESP is capped at its own 42.5 t and FRA is fully fed from its own 850 t:
  # neither draws on the other's residue.
  testthat::expect_equal(intake$intake, c(50 * 0.85, 100))
})

test_that("calculate_residue_destinies conserves mass with an unmatched region", {
  out <- suppressWarnings(
    whep::calculate_residue_destinies(tibble::tibble(
      item_prod_code = "15",
      residue_dm_t = 100,
      region_krausmann = "Nowhere",
      region_un_sub = "Nowhere"
    ))
  )
  testthat::expect_equal(
    out$residue_feed_dm_t + out$residue_burn_dm_t + out$residue_soil_dm_t,
    100
  )
  testthat::expect_equal(out$residue_soil_dm_t, 100)
})

test_that("an unmatched recovery rate is reported, not passed off as zero", {
  # whep#1175. The mass balance is the check that CANNOT see this: the failed
  # lookup books every tonne to soil, so feed + burn + soil still equals the
  # residue exactly, no total moves, and 16.65 Gt left the commodity balance in
  # silence. The guard has to fire on the input the identity is happy with.
  unmatched <- tibble::tibble(
    item_prod_code = "15",
    residue_dm_t = 100,
    region_krausmann = "Nowhere",
    region_un_sub = "Nowhere"
  )
  out <- suppressWarnings(whep::calculate_residue_destinies(unmatched))

  expect_supplied_guard(
    identity = isTRUE(all.equal(
      out$residue_feed_dm_t + out$residue_burn_dm_t + out$residue_soil_dm_t,
      100
    )),
    guard = whep::calculate_residue_destinies(unmatched),
    class = "whep_unmatched_recovery",
    condition = "warning"
  )
  testthat::expect_false(out$residue_recovery_matched)
})

test_that("a recovery rate the table gives as zero is not an unmatched one", {
  # 18 of the recovery table's 160 rows really are zero -- fodder crops in West
  # Europe is one -- and before whep#1175 that was the same value, through the
  # same `replace_na()`, as a lookup that found nothing. Item 638 is "Forage and
  # silage, rye grass", whose Krausmann category is "Fodder crops".
  out <- whep::calculate_residue_destinies(tibble::tibble(
    item_prod_code = "638",
    residue_dm_t = 100,
    region_krausmann = "West Europe",
    region_un_sub = "Western Europe"
  ))

  testthat::expect_true(out$residue_recovery_matched)
  testthat::expect_equal(out$residue_soil_dm_t, 100)
})

test_that("unmatched_recovery = 'abort' refuses to continue", {
  testthat::expect_error(
    whep::calculate_residue_destinies(
      tibble::tibble(
        item_prod_code = "15",
        residue_dm_t = 100,
        region_krausmann = "Nowhere",
        region_un_sub = "Nowhere"
      ),
      unmatched_recovery = "abort"
    ),
    class = "whep_unmatched_recovery"
  )
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

## ---- The bedding destiny (whep#1005) --------------------------------------

.rd_bedding_input <- function() {
  tibble::tibble(
    item_prod_code = "15",
    residue_dm_t = 100,
    region_krausmann = "Western Europe",
    region_un_sub = "Western Europe"
  )
}

test_that("bedding_fraction defaults to zero and changes nothing", {
  bare <- whep::calculate_residue_destinies(.rd_bedding_input())
  testthat::expect_equal(bare$residue_bedding_dm_t, 0)
  testthat::expect_equal(bare$residue_bedding_fraction, 0)
})

test_that("bedding is carved out of the non-feed removed share only", {
  bare <- whep::calculate_residue_destinies(.rd_bedding_input())
  bedded <- whep::calculate_residue_destinies(
    .rd_bedding_input(),
    bedding_fraction = 0.3
  )
  # Feed and the on-field share are untouched: bedding never comes out of the
  # residue that stayed on the field (IPCC 2019 Vol. 4 Ch. 10 p. 10.95).
  testthat::expect_equal(bedded$residue_feed_dm_t, bare$residue_feed_dm_t)
  testthat::expect_equal(bedded$residue_soil_dm_t, bare$residue_soil_dm_t)
  testthat::expect_equal(
    bedded$residue_bedding_dm_t,
    0.3 * bare$residue_burn_dm_t
  )
  testthat::expect_equal(bedded$residue_burn_dm_t, 0.7 * bare$residue_burn_dm_t)
})

test_that("the four destinies still sum to the whole residue", {
  for (frac in c(0, 0.14, 0.5, 1)) {
    out <- whep::calculate_residue_destinies(
      .rd_bedding_input(),
      bedding_fraction = frac
    )
    testthat::expect_equal(
      out$residue_feed_dm_t +
        out$residue_bedding_dm_t +
        out$residue_burn_dm_t +
        out$residue_soil_dm_t,
      100
    )
  }
})

test_that("the shares method carves bedding the same way", {
  x <- tibble::tibble(item_prod_code = "15", residue_dm_t = 100, year = 1950)
  out <- suppressWarnings(whep::calculate_residue_destinies(
    x,
    method = "shares",
    bedding_fraction = 0.25
  ))
  testthat::expect_equal(
    out$residue_feed_dm_t +
      out$residue_bedding_dm_t +
      out$residue_burn_dm_t +
      out$residue_soil_dm_t,
    100
  )
})

test_that("an out-of-range bedding fraction is refused", {
  bad_values <- list(-0.1, 1.5, NA_real_, c(0.1, 0.2), "0.1")
  for (bad in bad_values) {
    testthat::expect_error(
      whep::calculate_residue_destinies(
        .rd_bedding_input(),
        bedding_fraction = bad
      ),
      "bedding_fraction"
    )
  }
})

test_that("recovery = selects the rate column and is recorded (whep#1163)", {
  # Item 157 is sugar beet: Wirsenius Table 3.17 gives 0.90 in every region,
  # the legacy table 0 in West Europe.
  beet <- tibble::tibble(
    item_prod_code = "157",
    residue_dm_t = 100,
    region_krausmann = "West Europe",
    region_un_sub = "Western Europe"
  )
  default <- whep::calculate_residue_destinies(beet)
  legacy <- whep::calculate_residue_destinies(beet, recovery = "legacy")
  testthat::expect_equal(default$method_residue_recovery, "wirsenius")
  testthat::expect_equal(legacy$method_residue_recovery, "legacy")
  testthat::expect_equal(default$residue_soil_dm_t, 10)
  testthat::expect_equal(legacy$residue_soil_dm_t, 100)
  for (out in list(default, legacy)) {
    testthat::expect_equal(
      out$residue_feed_dm_t + out$residue_burn_dm_t + out$residue_soil_dm_t,
      100
    )
  }
  testthat::expect_error(
    whep::calculate_residue_destinies(beet, recovery = "krausmann"),
    class = "rlang_error"
  )
})

test_that("a category the source is silent on is the same in both variants", {
  # Item 176 is dry beans: Wirsenius models no residue for pulses, so the
  # Wirsenius variant keeps the legacy rate rather than inventing one.
  beans <- tibble::tibble(
    item_prod_code = "176",
    residue_dm_t = 100,
    region_krausmann = "East Asia",
    region_un_sub = "Eastern Asia"
  )
  testthat::expect_equal(
    whep::calculate_residue_destinies(beans)$residue_soil_dm_t,
    whep::calculate_residue_destinies(
      beans,
      recovery = "legacy"
    )$residue_soil_dm_t
  )
})

test_that("the shares method records no recovery variant", {
  x <- tibble::tibble(item_prod_code = "15", residue_dm_t = 100, year = 1950)
  out <- suppressWarnings(
    whep::calculate_residue_destinies(x, method = "shares")
  )
  testthat::expect_true(is.na(out$method_residue_recovery))
})
