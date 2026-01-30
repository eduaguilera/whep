# Tests for calculate_lmdi()

# Helper fixtures --------------------------------------------------------------

#' Generate consistent LMDI test data where emissions = activity * intensity.
#' This ensures perfect decomposition closure.
lmdi_basic_fixture <- function() {
  tibble::tribble(
    ~year, ~activity, ~intensity, ~emissions,
    2010,  1000,      0.10,       100,
    2011,  1100,      0.10,       110,
    2012,  1200,      0.10,       120,
    2013,  1300,      0.10,       130
  )
}

#' Test data with changing intensity (emissions = activity * intensity).
lmdi_varying_fixture <- function() {
  tibble::tribble(
    ~year, ~activity, ~intensity, ~emissions,
    2010,  1000,      0.10,       100,
    2011,  1100,      0.12,       132,
    2012,  1200,      0.08,       96
  )
}

#' Test data with multiple countries.
lmdi_country_fixture <- function() {
  tibble::tribble(
    ~country, ~year, ~activity, ~intensity, ~emissions,
    "ESP",    2010,  1000,      0.10,       100,
    "ESP",    2011,  1100,      0.11,       121,
    "ESP",    2012,  1200,      0.10,       120,
    "FRA",    2010,  2000,      0.05,       100,
    "FRA",    2011,  2200,      0.05,       110,
    "FRA",    2012,  2400,      0.05,       120
  )
}

#' Test data with sectors for structural decomposition.
lmdi_sector_fixture <- function() {
  tibble::tribble(
    ~year, ~sector, ~activity, ~emissions,
    2010, "industry", 600, 60,
    2010, "transport", 400, 40,
    2011, "industry", 700, 63,
    2011, "transport", 500, 55
  )
}

#' Inconsistent data that violates the identity (for closure warning tests).
lmdi_inconsistent_fixture <- function() {
  tibble::tribble(
    ~year, ~activity, ~intensity, ~emissions,
    2010,  1000,      0.10,       100,
    2011,  1100,      0.10,       200
  )
}

#' Data with zeros to test epsilon replacement.
lmdi_zeros_fixture <- function() {
  tibble::tribble(
    ~year, ~activity, ~intensity, ~emissions,
    2010,  1000,      0.10,       100,
    2011,  1100,      0.00,       0,
    2012,  1200,      0.10,       120
  )
}

#' Data for rolling mean tests (needs enough years).
lmdi_rolling_fixture <- function() {
  years <- 2010:2020
  activity <- seq(1000, 2000, length.out = 11)
  intensity <- rep(0.1, 11)
  tibble::tibble(
    year = years,
    activity = activity,
    intensity = intensity,
    emissions = activity * intensity
  )
}


# Basic decomposition ----------------------------------------------------------

test_that("calculate_lmdi performs basic decomposition with perfect closure", {
  data <- lmdi_basic_fixture()

  result <- calculate_lmdi(
    data,
    identity = "emissions:activity*intensity",
    time_var = year,
    verbose = FALSE
  )

  expect_true(tibble::is_tibble(result))
  expect_true("additive" %in% names(result))
  expect_true("multiplicative" %in% names(result))
  expect_true("component_type" %in% names(result))

  # Check closure for first period (2010-2011)
  period_result <- result |>
    dplyr::filter(period == "2010-2011")

  target_change <- period_result |>
    dplyr::filter(component_type == "target") |>
    dplyr::pull(additive)

  factor_sum <- period_result |>
    dplyr::filter(component_type == "factor") |>
    dplyr::pull(additive) |>
    sum()

  expect_equal(target_change, factor_sum, tolerance = 1e-6)
})

test_that("calculate_lmdi multiplicative closure holds", {
  data <- lmdi_varying_fixture()

  result <- calculate_lmdi(
    data,
    identity = "emissions:activity*intensity",
    time_var = year,
    verbose = FALSE
  )

  # Check multiplicative closure for first period
  period_result <- result |>
    dplyr::filter(period == "2010-2011")

  target_ratio <- period_result |>
    dplyr::filter(component_type == "target") |>
    dplyr::pull(multiplicative)

  factor_product <- period_result |>
    dplyr::filter(component_type == "factor") |>
    dplyr::pull(multiplicative) |>
    prod()

  expect_equal(target_ratio, factor_product, tolerance = 1e-6)
})

test_that("calculate_lmdi produces correct values (manual calculation)", {
  # Create a simple 2-year, 2-factor case for manual verification

  # Identity emissions = activity * intensity
  # Year 2010: 100 = 1000 * 0.10
  # Year 2011: 110 = 1100 * 0.10 (only activity changes by 10%)
  data <- tibble::tribble(
    ~year, ~activity, ~intensity, ~emissions,
    2010,  1000,      0.10,       100,
    2011,  1100,      0.10,       110
  )

  result <- calculate_lmdi(
    data,
    identity = "emissions:activity*intensity",
    time_var = year,
    verbose = FALSE
  )

  period_result <- result |>
    dplyr::filter(period == "2010-2011")

  # Manual LMDI calculation:
  # Log mean weight: L(Y_T, Y_0) = (Y_T - Y_0) / ln(Y_T / Y_0)
  # L = (110 - 100) / ln(110/100) = 10 / ln(1.1) ≈ 104.98
  l_weight <- (110 - 100) / log(110 / 100)

  # Activity effect: L * ln(Activity_T / Activity_0)
  # = L * ln(1100/1000) = L * ln(1.1) ≈ 10

  expected_activity_add <- l_weight * log(1100 / 1000)

  # Intensity effect: L * ln(Intensity_T / Intensity_0)
  # = L * ln(0.10/0.10) = L * 0 = 0
  expected_intensity_add <- l_weight * log(0.10 / 0.10)

  # Get actual values from result
  activity_add <- period_result |>
    dplyr::filter(factor_label == "activity") |>
    dplyr::pull(additive)

  intensity_add <- period_result |>
    dplyr::filter(factor_label == "intensity") |>
    dplyr::pull(additive)

  expect_equal(activity_add, expected_activity_add, tolerance = 1e-6)
  expect_equal(intensity_add, expected_intensity_add, tolerance = 1e-6)

  # Multiplicative exp(ln(factor_T / factor_0))
  # Activity mult = 1100/1000 = 1.1
  # Intensity mult = 0.10/0.10 = 1.0
  activity_mult <- period_result |>
    dplyr::filter(factor_label == "activity") |>
    dplyr::pull(multiplicative)

  intensity_mult <- period_result |>
    dplyr::filter(factor_label == "intensity") |>
    dplyr::pull(multiplicative)

  expect_equal(activity_mult, 1.1, tolerance = 1e-6)
  expect_equal(intensity_mult, 1.0, tolerance = 1e-6)
})

# Ratio notation ---------------------------------------------------------------

test_that("calculate_lmdi accepts ratio notation in identity", {
  data <- lmdi_basic_fixture()

  result <- calculate_lmdi(
    data,
    identity = "emissions:(emissions/activity)*activity",
    time_var = year,
    verbose = FALSE
  )

  expect_true(tibble::is_tibble(result))
  expect_true(nrow(result) > 0)

  # Verify factor_label is correctly parsed (not corrupted with control chars)
  labels <- result |>
    dplyr::filter(component_type == "factor") |>
    dplyr::pull(factor_label) |>
    unique()

  expect_true("emissions/activity" %in% labels)
  expect_true("activity" %in% labels)

  # Check closure
  period_result <- result |>
    dplyr::filter(period == "2010-2011")

  target_change <- period_result |>
    dplyr::filter(component_type == "target") |>
    dplyr::pull(additive)

  factor_sum <- period_result |>
    dplyr::filter(component_type == "factor") |>
    dplyr::pull(additive) |>
    sum()

  expect_equal(target_change, factor_sum, tolerance = 1e-6)
})


# Multi-period analysis --------------------------------------------------------

test_that("calculate_lmdi default periods creates year-over-year", {
  data <- lmdi_basic_fixture()

  result <- calculate_lmdi(
    data,
    identity = "emissions:activity*intensity",
    time_var = year,
    verbose = FALSE
  )

  periods_in_result <- result |>
    dplyr::pull(period) |>
    unique()

  expect_true("2010-2011" %in% periods_in_result)
  expect_true("2011-2012" %in% periods_in_result)
  expect_true("2012-2013" %in% periods_in_result)
})

test_that("calculate_lmdi explicit periods vector works", {
  data <- lmdi_basic_fixture()

  result <- calculate_lmdi(
    data,
    identity = "emissions:activity*intensity",
    time_var = year,
    periods = c(2010, 2013),
    verbose = FALSE
  )

  periods_in_result <- result |>
    dplyr::pull(period) |>
    unique()

  expect_equal(periods_in_result, "2010-2013")
})

test_that("calculate_lmdi periods_2 appends additional period", {
  data <- lmdi_basic_fixture()

  result <- calculate_lmdi(
    data,
    identity = "emissions:activity*intensity",
    time_var = year,
    periods = c(2010, 2011, 2012),
    periods_2 = c(2010, 2013),
    verbose = FALSE
  )

  periods_in_result <- result |>
    dplyr::pull(period) |>
    unique()

  expect_true("2010-2011" %in% periods_in_result)
  expect_true("2011-2012" %in% periods_in_result)
  expect_true("2010-2013" %in% periods_in_result)
})


# Grouping with analysis_by ----------------------------------------------------

test_that("calculate_lmdi performs separate decomposition per group", {
  data <- lmdi_country_fixture()

  result <- calculate_lmdi(
    data,
    identity = "emissions:activity*intensity",
    time_var = year,
    analysis_by = "country",
    verbose = FALSE
  )

  expect_true("country" %in% names(result))

  countries <- result |>
    dplyr::pull(country) |>
    unique()
  expect_true("ESP" %in% countries)
  expect_true("FRA" %in% countries)

  # Check closure for each country separately
  for (ctry in c("ESP", "FRA")) {
    period_result <- result |>
      dplyr::filter(country == ctry, period == "2010-2011")

    target_change <- period_result |>
      dplyr::filter(component_type == "target") |>
      dplyr::pull(additive)

    factor_sum <- period_result |>
      dplyr::filter(component_type == "factor") |>
      dplyr::pull(additive) |>
      sum()

    expect_equal(
      target_change,
      factor_sum,
      tolerance = 1e-6,
      label = paste("Closure for", ctry)
    )
  }
})


# Structural decomposition with [] brackets ------------------------------------

test_that("calculate_lmdi auto-detects sector selectors from identity", {
  data <- lmdi_sector_fixture()

  # Calculate total activity per year for the identity
  data_with_total <- data |>
    dplyr::group_by(year) |>
    dplyr::mutate(total_activity = sum(activity)) |>
    dplyr::ungroup()

  # Three-factor structural decomposition
  result <- calculate_lmdi(
    data_with_total,
    identity = paste0(
      "emissions:total_activity*",
      "(activity[sector]/total_activity)*",
      "(emissions[sector]/activity[sector])"
    ),
    time_var = year,
    verbose = FALSE
  )

  expect_true(tibble::is_tibble(result))
  expect_true(nrow(result) > 0)

  # Should have 3 factors + 1 target = 4 rows per period
  n_rows_per_period <- result |>
    dplyr::filter(period == "2010-2011") |>
    nrow()

  expect_equal(n_rows_per_period, 4)
})


# Rolling mean -----------------------------------------------------------------
test_that("calculate_lmdi applies rolling mean smoothing", {
  data <- lmdi_rolling_fixture()

  result <- calculate_lmdi(
    data,
    identity = "emissions:activity*intensity",
    time_var = year,
    rolling_mean = 3,
    verbose = FALSE
  )

  expect_true(tibble::is_tibble(result))
  # With 11 years and rolling_mean = 3, we lose 1 year on each end
  # So 9 years remain, giving 8 periods
  n_periods <- result |>
    dplyr::pull(period) |>
    unique() |>
    length()

  expect_equal(n_periods, 8)
})


# Identity labels --------------------------------------------------------------

test_that("calculate_lmdi uses custom identity labels", {
  data <- lmdi_basic_fixture()

  result <- calculate_lmdi(
    data,
    identity = "emissions:activity*intensity",
    identity_labels = c(
      "Total Emissions",
      "Activity Effect",
      "Intensity Effect"
    ),
    time_var = year,
    verbose = FALSE
  )

  labels <- result |>
    dplyr::pull(factor_label) |>
    unique()

  expect_true("Total Emissions" %in% labels)
  expect_true("Activity Effect" %in% labels)
  expect_true("Intensity Effect" %in% labels)
})

test_that("calculate_lmdi rejects wrong number of identity labels", {
  data <- lmdi_basic_fixture()

  expect_error(
    calculate_lmdi(
      data,
      identity = "emissions:activity*intensity",
      identity_labels = c("Label1", "Label2"),
      time_var = year,
      verbose = FALSE
    ),
    "identity_labels must have"
  )
})


# Output formats ---------------------------------------------------------------

test_that("calculate_lmdi clean output contains expected columns only", {
  data <- lmdi_basic_fixture()

  result <- calculate_lmdi(
    data,
    identity = "emissions:activity*intensity",
    time_var = year,
    output_format = "clean",
    verbose = FALSE
  )

  # Clean format should have these columns
  clean_cols <- c(
    "period",
    "period_years",
    "factor_label",
    "component_type",
    "identity",
    "identity_var",
    "additive",
    "multiplicative",
    "multiplicative_log"
  )
  purrr::walk(clean_cols, ~ expect_true(.x %in% names(result)))

  # Clean format should NOT have these columns (only in total)
  total_only_cols <- c(
    "factor_formula",
    "target_initial",
    "target_final",
    "total_change",
    "percentage_change",
    "closure_gap_additive",
    "closure_gap_ratio"
  )
  purrr::walk(total_only_cols, ~ expect_false(.x %in% names(result)))
})

test_that("calculate_lmdi total output contains all columns", {
  data <- lmdi_basic_fixture()

  result <- calculate_lmdi(
    data,
    identity = "emissions:activity*intensity",
    time_var = year,
    output_format = "total",
    verbose = FALSE
  )

  # Total format should have all columns
  all_cols <- c(
    "period",
    "period_years",
    "factor_label",
    "factor_formula",
    "component_type",
    "identity",
    "identity_var",
    "target_initial",
    "target_final",
    "total_change",
    "percentage_change",
    "additive",
    "multiplicative",
    "multiplicative_log",
    "closure_gap_additive",
    "closure_gap_ratio"
  )
  purrr::walk(all_cols, ~ expect_true(.x %in% names(result)))
})


# Edge cases -------------------------------------------------------------------

test_that("calculate_lmdi handles minimum data (2 years)", {
  data <- tibble::tribble(
    ~year, ~activity, ~intensity, ~emissions,
    2010,  1000,      0.10,       100,
    2011,  1100,      0.10,       110
  )

  result <- calculate_lmdi(
    data,
    identity = "emissions:activity*intensity",
    time_var = year,
    verbose = FALSE
  )

  expect_true(tibble::is_tibble(result))
  expect_true(nrow(result) > 0)
})

test_that("calculate_lmdi errors with insufficient data (1 year)", {
  data <- tibble::tribble(
    ~year, ~activity, ~intensity, ~emissions,
    2010,  1000,      0.10,       100
  )

  expect_error(
    calculate_lmdi(
      data,
      identity = "emissions:activity*intensity",
      time_var = year,
      verbose = FALSE
    ),
    "at least two periods"
  )
})

test_that("calculate_lmdi handles zeros with epsilon replacement", {
  data <- lmdi_zeros_fixture()

  result <- calculate_lmdi(
    data,
    identity = "emissions:activity*intensity",
    time_var = year,
    verbose = FALSE
  )

  expect_true(tibble::is_tibble(result))
  # 2 periods × 3 rows (2 factors + 1 target)
  expect_equal(nrow(result), 6)

  # Get period 2010-2011 results (drop from 100 to ~0)
  period_1 <- result |> dplyr::filter(period == "2010-2011")
  target_add_1 <- period_1 |>
    dplyr::filter(component_type == "target") |>
    dplyr::pull(additive)
  factors_sum_1 <- period_1 |>
    dplyr::filter(component_type == "factor") |>
    dplyr::pull(additive) |>
    sum()

  # Additive contributions should sum to target change (within tolerance)
  expect_equal(factors_sum_1, target_add_1, tolerance = 1)

  # Get period 2011-2012 results (rise from ~0 to 120)
  period_2 <- result |> dplyr::filter(period == "2011-2012")
  target_add_2 <- period_2 |>
    dplyr::filter(component_type == "target") |>
    dplyr::pull(additive)
  factors_sum_2 <- period_2 |>
    dplyr::filter(component_type == "factor") |>
    dplyr::pull(additive) |>
    sum()

  expect_equal(factors_sum_2, target_add_2, tolerance = 1)

  # Verify target changes match original data (emissions went 100 -> ~0 -> 120)
  expect_equal(target_add_1, -100, tolerance = 1)
  expect_equal(target_add_2, 120, tolerance = 1)
})

test_that("calculate_lmdi recalculates target after epsilon replacement", {
  # Test that identity is preserved after zero replacement
  # emissions = activity * intensity, but row 2 has intensity = 0
  data <- tibble::tribble(
    ~year, ~activity, ~intensity, ~emissions,
    2010, 1000, 0.1, 100,
    2011, 1100, 0.0, 0
  )

  result <- calculate_lmdi(
    data,
    identity = "emissions:activity*intensity",
    time_var = year,
    verbose = FALSE
  )

  # Get the decomposition for this period
  factors <- result |> dplyr::filter(component_type == "factor")
  target <- result |> dplyr::filter(component_type == "target")

  # Additive factors should sum to target additive (closure)
  factors_add_sum <- sum(factors$additive)
  target_add <- target$additive

  expect_equal(factors_add_sum, target_add, tolerance = 0.01)

  # Multiplicative factors should multiply to target multiplicative (closure)
  factors_mult_prod <- prod(factors$multiplicative)
  target_mult <- target$multiplicative

  expect_equal(factors_mult_prod, target_mult, tolerance = 0.001)
})


# Closure warning tests --------------------------------------------------------

test_that("calculate_lmdi warns on inconsistent data", {
  data <- lmdi_inconsistent_fixture()

  # Nest expect_warning to capture both warnings from single execution
  expect_warning(
    expect_warning(
      calculate_lmdi(
        data,
        identity = "emissions:activity*intensity",
        time_var = year,
        verbose = FALSE
      ),
      "Additive contributions differ"
    ),
    "Multiplicative contributions differ"
  )
})
