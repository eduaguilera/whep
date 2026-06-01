# test_build_integration.R — end-to-end tests using small fixtures
#
# These tests run the build pipelines on pre-captured fixture data
# (no remote data needed) and verify output contracts.

# -- Fixtures ------------------------------------------------------------------

prod_raw_fixture <- function() {
  readRDS(testthat::test_path("fixtures", "prod_raw_small.rds"))
}

prod_expected_fixture <- function() {
  readRDS(testthat::test_path("fixtures", "prod_expected.rds"))
}

cbs_fixed_fixture <- function() {
  readRDS(testthat::test_path("fixtures", "cbs_fixed_small.rds"))
}

cbs_expected_fixture <- function() {
  readRDS(testthat::test_path("fixtures", "cbs_expected.rds"))
}

# -- Production ----------------------------------------------------------------

test_that("build_primary_production returns expected columns", {
  result <- whep::build_primary_production(
    .raw_data = prod_raw_fixture()
  )
  expected_cols <- c(
    "year",
    "area_code",
    "item_prod_code",
    "item_cbs_code",
    "live_anim_code",
    "unit",
    "value",
    "source"
  )
  expect_equal(names(result), expected_cols)
})

test_that("build_primary_production has no duplicate keys", {
  result <- whep::build_primary_production(
    .raw_data = prod_raw_fixture()
  )
  keys <- dplyr::select(
    result,
    year,
    area_code,
    item_prod_code,
    unit
  )
  expect_equal(nrow(keys), nrow(dplyr::distinct(keys)))
})

test_that("build_primary_production preserves row count", {
  result <- whep::build_primary_production(
    .raw_data = prod_raw_fixture()
  )
  expected <- prod_expected_fixture()
  expect_equal(nrow(result), nrow(expected))
})

test_that("build_primary_production value column has no NAs", {
  result <- whep::build_primary_production(
    .raw_data = prod_raw_fixture()
  )
  expect_false(any(is.na(result$value)))
})

test_that("build_primary_production sources are from known set", {
  result <- whep::build_primary_production(
    .raw_data = prod_raw_fixture()
  )
  known_sources <- c(
    "FAOSTAT_prod",
    "EuropeAgriDB",
    "DM_yield_estimate",
    "fill_linear",
    "fill_linear_historical",
    "imputed_yield",
    "imputed_cbs_ratio",
    "LUH2_cropland",
    "LUH2_agriland",
    "LUH2_grassland",
    "LUH2",
    "Estimated",
    NA_character_
  )
  actual <- unique(result$source)
  unexpected <- setdiff(actual, known_sources)
  # Allow imputed_yield_* pattern
  unexpected <- unexpected[
    !grepl("^imputed_yield", unexpected)
  ]
  expect_length(unexpected, 0L)
})

test_that("build_primary_production matches expected output", {
  result <- whep::build_primary_production(
    .raw_data = prod_raw_fixture()
  )
  expected <- prod_expected_fixture()
  expect_equal(result, expected, ignore_attr = TRUE)
})

test_that("build_primary_production spot-check USA wheat", {
  result <- whep::build_primary_production(
    .raw_data = prod_raw_fixture()
  )
  spot <- result |>
    dplyr::filter(
      area_code == 231,
      item_prod_code == 15,
      unit == "tonnes",
      year == 2000
    ) |>
    dplyr::pull(value)
  expect_equal(spot, 60639376)
})

# -- CBS -----------------------------------------------------------------------

test_that("build_commodity_balances returns expected columns", {
  result <- whep::build_commodity_balances(
    .fixed_data = cbs_fixed_fixture()
  )
  expected_cols <- c(
    "year",
    "area_code",
    "item_cbs_code",
    "element",
    "value",
    "source",
    "fao_flag"
  )
  expect_equal(names(result), expected_cols)
})

test_that("build_commodity_balances has no duplicate keys", {
  result <- whep::build_commodity_balances(
    .fixed_data = cbs_fixed_fixture()
  )
  keys <- dplyr::select(
    result,
    year,
    area_code,
    item_cbs_code,
    element
  )
  expect_equal(nrow(keys), nrow(dplyr::distinct(keys)))
})

test_that("build_commodity_balances preserves row count", {
  result <- whep::build_commodity_balances(
    .fixed_data = cbs_fixed_fixture()
  )
  expected <- cbs_expected_fixture()
  expect_equal(nrow(result), nrow(expected))
})

test_that("build_commodity_balances elements are valid", {
  result <- whep::build_commodity_balances(
    .fixed_data = cbs_fixed_fixture()
  )
  valid_elements <- c(
    "domestic_supply",
    "production",
    "import",
    "export",
    "stock_variation",
    "food",
    "feed",
    "seed",
    "other_uses",
    "processing"
  )
  actual <- unique(result$element)
  expect_true(all(actual %in% valid_elements))
})

test_that("build_commodity_balances sources are from known set", {
  result <- whep::build_commodity_balances(
    .fixed_data = cbs_fixed_fixture()
  )
  known_sources <- c(
    "FAOSTAT_prod",
    "FAOSTAT_FBS_New",
    "FAOSTAT_FBS_Old",
    "FAOSTAT_FBS_Old_scaled",
    "FAOSTAT_CBS",
    "FAOSTAT_trade",
    "mean",
    "historical_fill",
    "Processed",
    "Processed_round2",
    NA_character_
  )
  actual <- unique(result$source)
  unexpected <- setdiff(actual, known_sources)
  expect_length(unexpected, 0L)
})

test_that("build_commodity_balances matches expected output", {
  result <- whep::build_commodity_balances(
    .fixed_data = cbs_fixed_fixture()
  )
  expected <- cbs_expected_fixture()
  expect_equal(result, expected)
})

test_that("build_commodity_balances spot-check ESP wheat", {
  result <- whep::build_commodity_balances(
    .fixed_data = cbs_fixed_fixture()
  )
  prod_val <- result |>
    dplyr::filter(
      area_code == 203,
      item_cbs_code == 2511,
      element == "production",
      year == 2000
    ) |>
    dplyr::pull(value)
  expect_equal(prod_val, 7293623)

  food_val <- result |>
    dplyr::filter(
      area_code == 203,
      item_cbs_code == 2511,
      element == "food",
      year == 2000
    ) |>
    dplyr::pull(value)
  expect_equal(food_val, 3575000)
})
