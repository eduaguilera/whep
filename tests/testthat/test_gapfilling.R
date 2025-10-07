# Helper fixtures --------------------------------------------------------------

linear_fill_fixture <- function() {
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

proxy_fill_fixture <- function() {
  tibble::tribble(
    ~category, ~year, ~value, ~proxy_variable,
    "a", 2015, NA, 1,
    "a", 2016, 3, 2,
    "a", 2017, NA, 2,
    "a", 2018, NA, 2,
    "a", 2019, 0, 2,
    "a", 2020, NA, 2,
    "b", 2015, 1, 1,
    "b", 2016, NA, 2,
    "b", 2017, NA, 3,
    "b", 2018, NA, 4,
    "b", 2019, 5, 5,
    "b", 2020, NA, 6
  )
}

sum_fill_fixture <- function() {
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

# linear_fill ------------------------------------------------------------------

testthat::test_that("linear_fill fills gaps and preserves originals", {
  result <- linear_fill_fixture() |>
    linear_fill(value, year, .by = "category")

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

testthat::test_that("linear_fill interpolates between anchor points, and adds flags", {
  linear_fill_fixture() |>
    linear_fill(
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

testthat::test_that("linear_fill carries values backward from first anchor, and adds flags", {
  linear_fill_fixture() |>
    linear_fill(
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

testthat::test_that("linear_fill carries values forward from last anchor, and adds flags", {
  linear_fill_fixture() |>
    linear_fill(
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

testthat::test_that("linear_fill interpolates grouped series", {
  linear_fill_fixture() |>
    linear_fill(value, year, .by = "category") |>
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

testthat::test_that("linear_fill propagates a single anchor value", {
  single_anchor_series() |>
    linear_fill(
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

# proxy_fill ------------------------------------------------------------------

testthat::test_that("proxy_fill scales gaps from proxy ratios", {
  proxy_fill_fixture() |>
    proxy_fill(
      value,
      proxy_variable,
      year,
      .by = "category"
    ) |>
    pointblank::expect_col_exists("proxy_ratio") |>
    pointblank::expect_col_exists("source_value") |>
    pointblank::expect_col_vals_in_set(
      source_value,
      c(
        "Original",
        "Proxy interpolated",
        "Proxy carried forward",
        "Proxy carried backwards"
      )
    ) |>
    pointblank::expect_col_vals_equal(
      value,
      c(3, 0, 1, 5),
      preconditions = \(df) df |> dplyr::filter(source_value == "Original")
    ) |>
    pointblank::expect_col_vals_expr(
      ~ dplyr::near(proxy_ratio, value / proxy_variable, tol = 1e-6),
      preconditions = \(df) df |> dplyr::filter(!is.na(value))
    )
})

testthat::test_that("proxy_fill works without grouping variables", {
  tibble::tribble(
    ~year, ~value, ~proxy_variable,
    2015, 10, 5,
    2016, NA, 10,
    2017, 30, 15
  ) |>
    proxy_fill(value, proxy_variable, year) |>
    pointblank::expect_col_exists("proxy_ratio") |>
    pointblank::expect_col_vals_not_null(proxy_ratio)
})

# sum_fill ---------------------------------------------------------------------

testthat::test_that("sum_fill accumulates changes while keeping originals", {
  sum_fill_fixture() |>
    sum_fill(
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

testthat::test_that("sum_fill handles accumulation without explicit groups", {
  tibble::tribble(
    ~year, ~value, ~change_variable,
    2015, 10, 0,
    2016, NA, 2,
    2017, NA, 3,
    2018, NA, 1
  ) |>
    sum_fill(value, change_variable) |>
    pointblank::expect_col_vals_equal(value, c(10, 12, 15, 16)) |>
    pointblank::expect_col_vals_in_set(
      source_value,
      c("Original", "Filled with sum")
    )
})

testthat::test_that("sum_fill start_with_zero toggles behaviour", {
  contiguous_gaps <- tibble::tribble(
    ~value, ~change_variable,
    NA, 1,
    NA, 2,
    NA, 3,
    NA, 4
  )

  contiguous_gaps |>
    sum_fill(value, change_variable) |>
    pointblank::expect_col_vals_equal(value, c(1, 3, 6, 10)) |>
    pointblank::expect_col_vals_equal(source_value, "Filled with sum")

  contiguous_gaps |>
    sum_fill(
      value,
      change_variable,
      start_with_zero = FALSE
    ) |>
    pointblank::expect_col_vals_null(value)
})

testthat::test_that("sum_fill respects grouping keys", {
  sum_fill_fixture() |>
    sum_fill(
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
