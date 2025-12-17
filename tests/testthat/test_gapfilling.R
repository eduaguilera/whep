# Helper fixtures --------------------------------------------------------------

fill_linear_fixture <- function() {
  tibble::tribble(
    ~category, ~year, ~value,
    "a", 2015, NA,
    "a", 2016, 3,
    "a", 2017, NA,
    "a", 2018, NA,
    "a", 2019, 0,
    "a", 2020, NA,
    "b", 2015, 1,
    "b", 2016, NA,
    "b", 2017, NA,
    "b", 2018, NA,
    "b", 2019, 5,
    "b", 2020, NA
  )
}

simple_linear_series <- function() {
  tibble::tribble(
    ~year, ~value,
    2015, 10,
    2016, NA,
    2017, NA,
    2018, NA,
    2019, 20
  )
}

single_anchor_series <- function(anchor = 42) {
  tibble::tribble(
    ~year, ~value,
    2015, NA,
    2016, anchor,
    2017, NA
  )
}

fill_sum_fixture <- function() {
  tibble::tribble(
    ~category, ~year, ~value, ~change_variable,
    "a", 2014, NA, 2,
    "a", 2015, NA, 3,
    "a", 2016, 3, 2,
    "a", 2017, NA, 3,
    "a", 2018, NA, 4,
    "a", 2019, 0, 1,
    "a", 2020, NA, 1,
    "b", 2015, 1, 0,
    "b", 2016, NA, 0,
    "b", 2017, NA, 0,
    "b", 2018, NA, 0,
    "b", 2019, 5, 0,
    "b", 2020, NA, 1
  )
}

# fill_linear ------------------------------------------------------------------

testthat::test_that("fill_linear fills gaps and preserves originals", {
  result <- fill_linear_fixture() |>
    fill_linear(value, year, .by = "category")

  result |>
    pointblank::expect_col_exists("source_value") |>
    pointblank::expect_col_vals_in_set(
      source_value,
      c(
        "Original",
        "Linear interpolation",
        "Last value carried forward",
        "First value carried backwards"
      )
    ) |>
    pointblank::expect_col_vals_not_null(value) |>
    pointblank::expect_col_vals_equal(
      value,
      c(3, 0, 1, 5),
      preconditions = \(df) df |> dplyr::filter(source_value == "Original")
    )

  dplyr::is_grouped_df(result) |>
    testthat::expect_false()
})

testthat::test_that("fill_linear interpolates between anchor points, and adds flags", {
  fill_linear_fixture() |>
    fill_linear(
      value,
      year,
      interpolate = TRUE,
      fill_forward = FALSE,
      fill_backward = FALSE,
      .by = "category"
    ) |>
    testthat::expect_equal(
      tibble::tribble(
        ~category, ~year, ~value, ~source_value,
        "a", 2015, NA, "Gap not filled",
        "a", 2016, 3, "Original",
        "a", 2017, 2, "Linear interpolation",
        "a", 2018, 1, "Linear interpolation",
        "a", 2019, 0, "Original",
        "a", 2020, NA, "Gap not filled",
        "b", 2015, 1, "Original",
        "b", 2016, 2, "Linear interpolation",
        "b", 2017, 3, "Linear interpolation",
        "b", 2018, 4, "Linear interpolation",
        "b", 2019, 5, "Original",
        "b", 2020, NA, "Gap not filled"
      )
    )
})

testthat::test_that("fill_linear carries values backward from first anchor, and adds flags", {
  fill_linear_fixture() |>
    fill_linear(
      value,
      year,
      interpolate = FALSE,
      fill_forward = FALSE,
      fill_backward = TRUE,
      .by = "category"
    ) |>
    testthat::expect_equal(
      tibble::tribble(
        ~category, ~year, ~value, ~source_value,
        "a", 2015, 3, "First value carried backwards",
        "a", 2016, 3, "Original",
        "a", 2017, NA, "Gap not filled",
        "a", 2018, NA, "Gap not filled",
        "a", 2019, 0, "Original",
        "a", 2020, NA, "Gap not filled",
        "b", 2015, 1, "Original",
        "b", 2016, NA, "Gap not filled",
        "b", 2017, NA, "Gap not filled",
        "b", 2018, NA, "Gap not filled",
        "b", 2019, 5, "Original",
        "b", 2020, NA, "Gap not filled"
      )
    )
})

testthat::test_that("fill_linear carries values forward from last anchor, and adds flags", {
  fill_linear_fixture() |>
    fill_linear(
      value,
      year,
      interpolate = FALSE,
      fill_forward = TRUE,
      fill_backward = FALSE,
      .by = "category"
    ) |>
    testthat::expect_equal(
      tibble::tribble(
        ~category, ~year, ~value, ~source_value,
        "a", 2015, NA, "Gap not filled",
        "a", 2016, 3, "Original",
        "a", 2017, NA, "Gap not filled",
        "a", 2018, NA, "Gap not filled",
        "a", 2019, 0, "Original",
        "a", 2020, 0, "Last value carried forward",
        "b", 2015, 1, "Original",
        "b", 2016, NA, "Gap not filled",
        "b", 2017, NA, "Gap not filled",
        "b", 2018, NA, "Gap not filled",
        "b", 2019, 5, "Original",
        "b", 2020, 5, "Last value carried forward"
      )
    )
})

testthat::test_that("fill_linear interpolates grouped series", {
  fill_linear_fixture() |>
    fill_linear(value, year, .by = "category") |>
    pointblank::expect_col_vals_equal(
      value,
      c(3, 3, 2, 1, 0, 0),
      preconditions = \(df) df |> dplyr::filter(category == "a")
    ) |>
    pointblank::expect_col_vals_equal(
      value,
      c(1, 2, 3, 4, 5, 5),
      preconditions = \(df) df |> dplyr::filter(category == "b")
    )
})

testthat::test_that("fill_linear propagates a single anchor value", {
  single_anchor_series() |>
    fill_linear(
      value,
      year,
      interpolate = FALSE,
      fill_forward = TRUE,
      fill_backward = TRUE
    ) |>
    pointblank::expect_col_vals_equal(value, c(42, 42, 42)) |>
    pointblank::expect_col_vals_in_set(
      source_value,
      c(
        "First value carried backwards",
        "Original",
        "Last value carried forward"
      )
    )
})

testthat::test_that("fill_linear with value_smooth_window uses smoothed values for filling", {
  # Create data with high variability
  noisy_data <- tibble::tribble(
    ~year, ~value,
    2010, 100,
    2011, 120,
    2012, 80,
    2013, NA,
    2014, NA,
    2015, 110,
    2016, 90,
    2017, 130

)

  # Without smoothing: interpolation uses raw anchor values (80 and 110)
  result_no_smooth <- noisy_data |>
    fill_linear(value, year, fill_forward = FALSE, fill_backward = FALSE)

  # With smoothing (window = 3): uses moving average of anchors

  result_smooth <- noisy_data |>
    fill_linear(
      value,
      year,
      fill_forward = FALSE,
      fill_backward = FALSE,
      value_smooth_window = 3
    )

  # Both should fill the gaps
  testthat::expect_false(any(is.na(result_no_smooth$value[4:5])))
  testthat::expect_false(any(is.na(result_smooth$value[4:5])))

  # Smoothed result should differ from non-smoothed due to moving average
  testthat::expect_false(
    all(result_no_smooth$value[4:5] == result_smooth$value[4:5])
  )

  # Original values should be preserved in both cases
  testthat::expect_equal(result_no_smooth$value[1], 100)
  testthat::expect_equal(result_smooth$value[1], 100)
  testthat::expect_equal(result_no_smooth$value[6], 110)
  testthat::expect_equal(result_smooth$value[6], 110)
})

testthat::test_that("fill_linear value_smooth_window NULL behaves as default", {
  result_default <- simple_linear_series() |>
    fill_linear(value, year)

  result_null <- simple_linear_series() |>
    fill_linear(value, year, value_smooth_window = NULL)

  testthat::expect_equal(result_default, result_null)
})

testthat::test_that("fill_linear value_smooth_window works with carry forward/backward", {
  # Data where smoothing affects the carried value
  edge_data <- tibble::tribble(
    ~year, ~value,
    2010, 100,
    2011, 120,
    2012, 80,
    2013, NA,
    2014, NA
  )

  # Without smoothing: carries 80 forward
  result_no_smooth <- edge_data |>
    fill_linear(
      value,
      year,
      interpolate = FALSE,
      fill_forward = TRUE,
      fill_backward = FALSE
    )

  # With smoothing (window = 3): carries smoothed value forward
  # MA of (100, 120, 80) = 100 for 2011, MA of (120, 80, NA) won't work
  # But 2012 has neighbours so its smoothed value = mean(120, 80, NA) = NA
  # This tests edge behaviour
  result_smooth <- edge_data |>
    fill_linear(
      value,
      year,
      interpolate = FALSE,
      fill_forward = TRUE,
      fill_backward = FALSE,
      value_smooth_window = 3
    )

  # Without smoothing, should carry 80
  testthat::expect_equal(result_no_smooth$value[4], 80)
  testthat::expect_equal(result_no_smooth$value[5], 80)

  # Original values preserved
  testthat::expect_equal(result_smooth$value[1], 100)
  testthat::expect_equal(result_smooth$value[3], 80)
})

# fill_sum --------------------------------------------------------------------

testthat::test_that("fill_sum accumulates changes while keeping originals", {
  fill_sum_fixture() |>
    fill_sum(
      value,
      change_variable,
      start_with_zero = TRUE,
      .by = "category"
    ) |>
    pointblank::expect_col_exists("source_value") |>
    pointblank::expect_col_vals_in_set(
      source_value,
      c("Original", "Filled with sum")
    ) |>
    pointblank::expect_col_vals_not_null(value) |>
    pointblank::expect_col_vals_equal(
      value,
      c(2, 5, 3, 6, 10, 0, 1),
      preconditions = \(df) df |> dplyr::filter(category == "a")
    ) |>
    pointblank::expect_col_vals_equal(
      value,
      c(1, 1, 1, 1, 5, 6),
      preconditions = \(df) df |> dplyr::filter(category == "b")
    )
})

testthat::test_that("fill_sum handles accumulation without explicit groups", {
  tibble::tribble(
    ~year, ~value, ~change_variable,
    2015, 10, 0,
    2016, NA, 2,
    2017, NA, 3,
    2018, NA, 1
  ) |>
    fill_sum(value, change_variable) |>
    pointblank::expect_col_vals_equal(value, c(10, 12, 15, 16)) |>
    pointblank::expect_col_vals_in_set(
      source_value,
      c("Original", "Filled with sum")
    )
})

testthat::test_that("fill_sum start_with_zero toggles behaviour", {
  contiguous_gaps <- tibble::tribble(
    ~value, ~change_variable,
    NA, 1,
    NA, 2,
    NA, 3,
    NA, 4
  )

  contiguous_gaps |>
    fill_sum(value, change_variable) |>
    pointblank::expect_col_vals_equal(value, c(1, 3, 6, 10)) |>
    pointblank::expect_col_vals_equal(source_value, "Filled with sum")

  contiguous_gaps |>
    fill_sum(
      value,
      change_variable,
      start_with_zero = FALSE
    ) |>
    pointblank::expect_col_vals_null(value)
})

testthat::test_that("fill_sum respects grouping keys", {
  fill_sum_fixture() |>
    fill_sum(
      value,
      change_variable,
      .by = "category"
    ) |>
    pointblank::expect_col_vals_equal(
      value,
      c(2, 5, 3, 6, 10, 0, 1),
      preconditions = \(df) df |> dplyr::filter(category == "a")
    ) |>
    pointblank::expect_col_vals_equal(
      value,
      c(1, 1, 1, 1, 5, 6),
      preconditions = \(df) df |> dplyr::filter(category == "b")
    )
})

# fill_growth ------------------------------------------------------------------

test_that("fill_growth fills missing values using proxy growth rates", {
  data <- tibble::tribble(
    ~country, ~year, ~gdp, ~population,
    "ESP", 2010, 100, 46,
    "ESP", 2011, NA, 47,
    "ESP", 2012, 120, 48,
    "ESP", 2013, NA, 49
  )

  result <- fill_growth(
    data,
    value_col = "gdp",
    proxy_col = "population",
    group_by = "country",
    verbose = FALSE
  )

  # Should have filled the NA values
  expect_false(any(is.na(result$gdp)))
})

test_that("fill_growth respects max_gap parameter", {
  data <- tibble::tribble(
    ~year, ~value, ~proxy,
    2010, 100, 1000,
    2011, NA, 1100,
    2012, NA, 1200,
    2013, NA, 1300,
    2014, 150, 1400
  )

  result <- fill_growth(
    data,
    value_col = "value",
    proxy_col = "proxy",
    max_gap = 2,
    verbose = FALSE
  )

  # With max_gap = 2, should not fill 3 consecutive NAs
  expect_true(is.na(result$value[3]))
})

test_that("fill_growth works with grouping", {
  data <- tibble::tribble(
    ~country, ~year, ~emissions, ~gdp,
    "ESP", 2010, 100, 1000,
    "ESP", 2011, NA, 1100,
    "ESP", 2012, 130, 1200,
    "FRA", 2010, 200, 2000,
    "FRA", 2011, NA, 2200,
    "FRA", 2012, 250, 2400
  )

  result <- fill_growth(
    data,
    value_col = "emissions",
    proxy_col = "gdp",
    group_by = "country",
    verbose = FALSE
  )

  # Check both groups have filled values
  esp_filled <- result |>
    dplyr::filter(country == "ESP", year == 2011) |>
    dplyr::pull(emissions)

  fra_filled <- result |>
    dplyr::filter(country == "FRA", year == 2011) |>
    dplyr::pull(emissions)

  expect_false(is.na(esp_filled))
  expect_false(is.na(fra_filled))
})

test_that("fill_growth returns same number of rows", {
  data <- tibble::tribble(
    ~year, ~value, ~proxy,
    2010, 100, 1000,
    2011, NA, 1100,
    2012, 120, 1200
  )

  result <- fill_growth(
    data,
    value_col = "value",
    proxy_col = "proxy",
    verbose = FALSE
  )

  expect_equal(nrow(result), nrow(data))
})

# Hierarchical Segmented Interpolation -----------------------------------------

test_that("fill_growth uses hierarchical segmentation with intermediate proxy data", {
  # Spain wages example: household_ppp has gap 2008-2019
  # formal_ppp has data 2010-2018 (should be used for middle segment)
  # gdp_pc_constant has complete data (fallback for edges)

  data_wages <- tibble::tribble(
    ~country, ~year, ~household_ppp, ~formal_ppp, ~gdp_pc_constant,
    "ESP", 2008, 100, NA, 50,
    "ESP", 2009, NA, NA, 51,
    "ESP", 2010, NA, 105, 52,
    "ESP", 2011, NA, 108, 53,
    "ESP", 2012, NA, 112, 54,
    "ESP", 2013, NA, 115, 55,
    "ESP", 2014, NA, 118, 56,
    "ESP", 2015, NA, 122, 57,
    "ESP", 2016, NA, 125, 58,
    "ESP", 2017, NA, 130, 59,
    "ESP", 2018, NA, 135, 60,
    "ESP", 2019, 150, NA, 61
  )

  result <- fill_growth(
    data_wages,
    value_col = "household_ppp",
    proxy_col = c("formal_ppp", "gdp_pc_constant"),
    group_by = "country",
    output_format = "detailed",
    verbose = FALSE
  )

  # All gaps should be filled
  expect_false(any(is.na(result$household_ppp)))

  # Original values should be preserved
  expect_equal(result$household_ppp[result$year == 2008], 100)
  expect_equal(result$household_ppp[result$year == 2019], 150)

  # Check that method column indicates bridge interpolation was used
  middle_methods <- result |>
    dplyr::filter(year >= 2009, year <= 2018) |>
    dplyr::pull(method_household_ppp)

  expect_true(any(grepl("bridge", middle_methods)))
})

test_that("fill_growth maintains continuity without jumps between segments", {
  # Test that segmented interpolation produces smooth transitions with bridge
  data_test <- tibble::tribble(
    ~year, ~primary, ~proxy1, ~proxy2,
    2000, 100, 100, 100,
    2001, NA, 105, 102,
    2002, NA, 110, 104,
    2003, NA, 115, 106,
    2004, NA, 120, 108,
    2005, 200, 125, 110
  )

  result <- fill_growth(
    data_test,
    value_col = "primary",
    proxy_col = c("proxy1", "proxy2"),
    verbose = FALSE
  )

  # Check for continuity: bridge should connect smoothly
  values <- result$primary

  # First and last values should match original anchors
  expect_equal(values[1], 100)
  expect_equal(values[6], 200)

  # Check that bridge method was used (not simple forward/backfill)
  expect_true(any(grepl("bridge", result$method_primary)))

  # Values should increase (since both proxies increase monotonically)
  expect_true(all(diff(values) >= 0))

  # Growth rates should be reasonable (smooth with bridge adjustment)
  growth_rates <- diff(values) / values[-length(values)]
  expect_true(all(abs(growth_rates) < 1.0))
})

test_that("fill_growth respects proxy hierarchy in segmentation", {
  # Better proxy (proxy1) should be used when available
  data_hierarchy <- tibble::tribble(
    ~year, ~value, ~proxy1, ~proxy2,
    2010, 100, NA, 50,
    2011, NA, NA, 52,
    2012, NA, 120, 54,
    2013, NA, 125, 56,
    2014, NA, NA, 58,
    2015, 180, NA, 60
  )

  result <- fill_growth(
    data_hierarchy,
    value_col = "value",
    proxy_col = c("proxy1", "proxy2"),
    output_format = "detailed",
    verbose = FALSE
  )

  # Should have used better proxy for middle segment
  expect_false(any(is.na(result$value)))
  expect_equal(nrow(result), 6)
})

test_that("fill_growth backward compatible: single proxy behaves as before", {
  # Without intermediate data, should work exactly as old version
  data_simple <- tibble::tribble(
    ~year, ~value, ~proxy,
    2010, 100, 1000,
    2011, NA, 1100,
    2012, NA, 1200,
    2013, NA, 1300,
    2014, 150, 1400
  )

  result <- fill_growth(
    data_simple,
    value_col = "value",
    proxy_col = "proxy",
    verbose = FALSE
  )

  # Should fill all gaps
  expect_false(any(is.na(result$value)))

  # Should maintain anchors
  expect_equal(result$value[result$year == 2010], 100)
  expect_equal(result$value[result$year == 2014], 150)

  # Interpolated values should be between anchors
  expect_true(all(result$value[2:4] > 100))
  expect_true(all(result$value[2:4] < 150))
})

test_that("fill_growth handles case with no intermediate data gracefully", {
  # Hierarchical proxies but none have intermediate data
  data_no_intermediate <- tibble::tribble(
    ~year, ~value, ~proxy1, ~proxy2,
    2010, 100, NA, 50,
    2011, NA, NA, 52,
    2012, NA, NA, 54,
    2013, 150, NA, 56
  )

  result <- fill_growth(
    data_no_intermediate,
    value_col = "value",
    proxy_col = c("proxy1", "proxy2"),
    verbose = FALSE
  )

  # Should fall back to proxy2 for entire gap
  expect_false(any(is.na(result$value)))
  expect_equal(result$value[result$year == 2010], 100)
  expect_equal(result$value[result$year == 2013], 150)
})

test_that("fill_growth preserves original data points when they exist", {
  data_mixed <- tibble::tribble(
    ~year, ~value, ~proxy,
    2010, 100, 1000,
    2011, NA, 1100,
    2012, 120, 1200,
    2013, NA, 1300,
    2014, 150, 1400
  )

  result <- fill_growth(
    data_mixed,
    value_col = "value",
    proxy_col = "proxy",
    verbose = FALSE
  )

  # Original data points should be exactly preserved
  expect_equal(result$value[result$year == 2010], 100)
  expect_equal(result$value[result$year == 2012], 120)
  expect_equal(result$value[result$year == 2014], 150)

  # Gaps should be filled
  expect_false(is.na(result$value[result$year == 2011]))
  expect_false(is.na(result$value[result$year == 2013]))
})

test_that("fill_growth preserves non-NA values", {
  data <- tibble::tribble(
    ~year, ~value, ~proxy,
    2010, 100, 1000,
    2011, NA, 1100,
    2012, 120, 1200
  )

  result <- fill_growth(
    data,
    value_col = "value",
    proxy_col = "proxy",
    verbose = FALSE
  )

  # Original non-NA values should be unchanged
  expect_equal(result$value[1], 100)
  expect_equal(result$value[3], 120)
})

test_that("fill_growth extrapolates at ends with hierarchical growth", {
  data <- tibble::tribble(
    ~year, ~value, ~proxy1, ~proxy2,
    2010, NA, 100, 50,
    2011, NA, 103, 51,
    2012, 120, 106, 52,
    2013, NA, 109, 53,
    2014, NA, 112, 54
  )

  res <- fill_growth(
    data,
    value_col = "value",
    proxy_col = c("proxy1", "proxy2"),
    max_gap_linear = 1,
    output_format = "detailed",
    verbose = FALSE
  )

  # Start and end should be filled (edge extrapolation)
  expect_false(any(is.na(res$value)))
  # Methods at ends should be growth_* (not *_bridge)
  expect_true(any(grepl("^growth_", res$method_value)))
})
