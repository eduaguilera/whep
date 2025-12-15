# Tests for calculate_lmdi()

test_that("calculate_lmdi performs basic decomposition", {
  data <- tibble::tribble(
    ~year, ~emissions, ~gdp, ~population,
    2010,        100, 1000,          46,
    2011,        110, 1100,          47,
    2012,        120, 1200,          48
  )

  result <- calculate_lmdi(
    data,
    identity = "emissions:gdp*population",
    time_var = "year",
    verbose = FALSE
  )

  expect_true(is.data.frame(result))
  expect_true("additive" %in% names(result))
})

test_that("calculate_lmdi works with grouping", {
  data <- tibble::tribble(
    ~country, ~year, ~emissions, ~gdp, ~population,
    "ESP", 2010, 100, 1000, 46,
    "ESP", 2011, 110, 1100, 47,
    "FRA", 2010, 200, 2000, 65,
    "FRA", 2011, 220, 2200, 66
  )

  result <- calculate_lmdi(
    data,
    identity = "emissions:gdp*population",
    time_var = "year",
    analysis_by = "country",
    verbose = FALSE
  )

  expect_true("country" %in% names(result))
  expect_true("ESP" %in% result$country)
  expect_true("FRA" %in% result$country)
})

test_that("calculate_lmdi returns expected columns", {
  data <- tibble::tribble(
    ~year, ~emissions, ~gdp, ~population,
    2010,        100, 1000,          46,
    2011,        110, 1100,          47,
    2012,        120, 1200,          48
  )

  result <- calculate_lmdi(
    data,
    identity = "emissions:gdp*population",
    time_var = "year",
    verbose = FALSE
  )

  expect_true("component_type" %in% names(result))
  expect_true("additive" %in% names(result))
  expect_true("multiplicative" %in% names(result))
})

test_that("calculate_lmdi handles multiple periods", {
  data <- tibble::tribble(
    ~year, ~emissions, ~gdp, ~population,
    2010,        100, 1000,          46,
    2011,        110, 1100,          47,
    2012,        120, 1200,          48,
    2013,        130, 1300,          49
  )

  result <- calculate_lmdi(
    data,
    identity = "emissions:gdp*population",
    time_var = "year",
    periods = c(2010, 2012, 2013),
    verbose = FALSE
  )

  expect_true(is.data.frame(result))
  expect_true(nrow(result) > 0)
})
