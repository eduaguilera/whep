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

# linear_fill -------------------------------------------------------------------

testthat::test_that("linear_fill fills gaps and preserves originals", {
  result <- linear_fill(linear_fill_fixture(), value, year, .by = "category")

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
    pointblank::expect_col_vals_not_null(value)

  result |>
    dplyr::filter(source_value == "Original") |>
    dplyr::pull(value) |>
    testthat::expect_equal(c(3, 0, 1, 5))

  dplyr::is_grouped_df(result) |>
    testthat::expect_false()
})

testthat::test_that("linear_fill interpolates and carries flags", {
  base <- linear_fill_fixture()

  only_interp <- linear_fill(
    base,
    value,
    year,
    interpolate = TRUE,
    fill_forward = FALSE,
    fill_backward = FALSE,
    .by = "category"
  )

  only_interp |>
    dplyr::filter(category == "a", year %in% c(2017, 2018)) |>
    dplyr::pull(source_value) |>
    testthat::expect_equal(rep("Linear interpolation", 2))

  backward_only <- linear_fill(
    base,
    value,
    year,
    interpolate = FALSE,
    fill_forward = FALSE,
    fill_backward = TRUE,
    .by = "category"
  )

  backward_only |>
    dplyr::filter(category == "a", year == 2015) |>
    dplyr::pull(source_value) |>
    testthat::expect_equal("First value carried backwards")

  forward_only <- linear_fill(
    base,
    value,
    year,
    interpolate = FALSE,
    fill_forward = TRUE,
    fill_backward = FALSE,
    .by = "category"
  )

  forward_only |>
    dplyr::filter(category == "b", year %in% 2016:2018) |>
    dplyr::pull(source_value) |>
    testthat::expect_equal(rep("Last value carried forward", 3))
})

testthat::test_that("linear_fill interpolates grouped series", {
  grouped_data <- tibble::tribble(
    ~group, ~year, ~value,
    "A", 2015, 0,
    "A", 2016, NA,
    "A", 2017, NA,
    "A", 2018, 12,
    "B", 2015, 20,
    "B", 2016, NA,
    "B", 2017, NA,
    "B", 2018, 40
  )

  result <- linear_fill(grouped_data, value, year, .by = "group")

  result |>
    dplyr::filter(group == "A") |>
    dplyr::pull(value) |>
    testthat::expect_equal(c(0, 4, 8, 12))

  result |>
    dplyr::filter(group == "B") |>
    dplyr::pull(value) |>
    testthat::expect_equal(c(20, 26.666667, 33.333333, 40), tolerance = 1e-6)
})

testthat::test_that("linear_fill propagates a single anchor value", {
  result <- linear_fill(
    single_anchor_series(),
    value,
    year,
    interpolate = FALSE,
    fill_forward = TRUE,
    fill_backward = TRUE
  )

  result |>
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

# proxy_fill --------------------------------------------------------------------

testthat::test_that("proxy_fill scales gaps from proxy ratios", {
  result <- proxy_fill(
    proxy_fill_fixture(),
    value,
    proxy_variable,
    year,
    .by = "category"
  )

  result |>
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
    )

  result |>
    dplyr::filter(source_value == "Original") |>
    dplyr::pull(value) |>
    testthat::expect_equal(c(3, 0, 1, 5))

  ratio_diff <- result |>
    dplyr::filter(!is.na(value)) |>
    dplyr::mutate(calculated_ratio = value / proxy_variable) |>
    dplyr::summarise(
      max_diff = max(abs(calculated_ratio - proxy_ratio), na.rm = TRUE)
    ) |>
    dplyr::pull(max_diff)

  testthat::expect_lt(ratio_diff, 1e-6)

  dplyr::is_grouped_df(result) |>
    testthat::expect_false()
})

testthat::test_that("proxy_fill works without grouping variables", {
  simple_proxy <- tibble::tribble(
    ~year, ~value, ~proxy_variable,
    2015, 10, 5,
    2016, NA, 10,
    2017, 30, 15
  )

  proxy_fill(simple_proxy, value, proxy_variable, year) |>
    pointblank::expect_col_exists("proxy_ratio") |>
    pointblank::expect_col_vals_not_null(proxy_ratio)
})

# sum_fill ----------------------------------------------------------------------

testthat::test_that("sum_fill accumulates changes while keeping originals", {
  result <- sum_fill(
    sum_fill_fixture(),
    value,
    change_variable,
    start_with_zero = TRUE,
    .by = "category"
  )

  result |>
    pointblank::expect_col_exists("source_value") |>
    pointblank::expect_col_vals_in_set(
      source_value,
      c("Original", "Filled with sum")
    ) |>
    pointblank::expect_col_vals_not_null(value)

  result |>
    dplyr::filter(category == "a") |>
    dplyr::pull(value) |>
    testthat::expect_equal(c(2, 5, 3, 6, 10, 0, 1))

  result |>
    dplyr::filter(category == "b") |>
    dplyr::pull(value) |>
    testthat::expect_equal(c(1, 1, 1, 1, 5, 6))
})

testthat::test_that("sum_fill handles accumulation without explicit groups", {
  simple_series <- tibble::tribble(
    ~year, ~value, ~change_variable,
    2015, 10, 0,
    2016, NA, 2,
    2017, NA, 3,
    2018, NA, 1
  )

  result <- sum_fill(simple_series, value, change_variable)

  result |>
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

  default_result <- sum_fill(contiguous_gaps, value, change_variable)

  default_result |>
    pointblank::expect_col_vals_null(value)

  zero_start <- sum_fill(
    contiguous_gaps,
    value,
    change_variable,
    start_with_zero = TRUE
  )

  zero_start |>
    pointblank::expect_col_vals_equal(value, c(1, 3, 6, 10)) |>
    pointblank::expect_col_vals_equal(source_value, rep("Filled with sum", 4))
})

testthat::test_that("sum_fill respects grouping keys", {
  grouped <- tibble::tribble(
    ~grp, ~value, ~change,
    "A", NA, 1,
    "A", NA, 2,
    "A", NA, 3,
    "B", 5, 0,
    "B", NA, 2,
    "B", NA, 4
  )

  result <- sum_fill(
    grouped,
    value,
    change,
    start_with_zero = TRUE,
    .by = "grp"
  )

  totals <- result |>
    dplyr::group_by(grp) |>
    dplyr::summarise(values = list(value)) |>
    dplyr::arrange(grp)

  testthat::expect_equal(totals$grp, c("A", "B"))
  testthat::expect_equal(totals$values[[1]], c(1, 3, 6))
  testthat::expect_equal(totals$values[[2]], c(5, 7, 11))
})

# Integration -------------------------------------------------------------------

testthat::test_that("gapfilling helpers compose in pipelines", {
  fixture_linear <- linear_fill_fixture()
  fixture_proxy  <- proxy_fill_fixture()
  fixture_sum    <- sum_fill_fixture()

  linear <- linear_fill(fixture_linear, value, year, .by = "category")
  proxy <- proxy_fill(fixture_proxy, value, proxy_variable, year, .by = "category")
  summed <- sum_fill(
    fixture_sum,
    value,
    change_variable,
    start_with_zero = FALSE,
    .by = "category"
  )
  summed_zero <- sum_fill(
    fixture_sum,
    value,
    change_variable,
    start_with_zero = TRUE,
    .by = "category"
  )

  list(linear, proxy, summed, summed_zero) |>
    purrr::walk(~ testthat::expect_s3_class(.x, "data.frame"))
})

