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

# One series with an opening gap, an interior gap and a trailing gap, so that
# all three fill directions are exercised at once.
one_series_with_gaps <- function() {
  tibble::tribble(
    ~year, ~value,
    2015, NA,
    2016, 3,
    2017, NA,
    2018, NA,
    2019, 0,
    2020, NA
  )
}

# The row orders an order-invariance check runs through: as given, fully
# reversed (which is what breaks a positional carry-forward or a centered
# moving average), and one scramble that also interleaves the groups.
hostile_row_orders <- function(data) {
  n <- nrow(data)
  scramble <- c(seq(2L, n, by = 2L), seq(1L, n, by = 2L))
  list(
    as_given = data,
    reversed = data[rev(seq_len(n)), ],
    scrambled = data[scramble, ]
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
    fill_linear(value, .by = "category")

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
    fill_linear(value, .by = "category") |>
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
    fill_linear(value, fill_forward = FALSE, fill_backward = FALSE)

  # With smoothing (window = 3): uses moving average of anchors
  result_smooth <- noisy_data |>
    fill_linear(
      value,
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
    fill_linear(value)

  result_null <- simple_linear_series() |>
    fill_linear(value, value_smooth_window = NULL)

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

testthat::test_that("fill_linear handles single non-NA value without error", {
  # Only 1 non-NA value: zoo::na.approx needs >= 2, should not error
  tibble::tribble(
    ~year, ~value,
    2015, NA,
    2016, 5,
    2017, NA
  ) |>
    fill_linear(value) |>
    pointblank::expect_col_vals_equal(value, c(5, 5, 5)) |>
    pointblank::expect_col_vals_in_set(
      source_value,
      c(
        "Original",
        "First value carried backwards",
        "Last value carried forward"
      )
    )
})

testthat::test_that("fill_linear warns on duplicate years within a group", {
  # Duplicate years are malformed time-series input and must be flagged
  # rather than silently passed to approx() which crashes in obscure ways.
  testthat::expect_warning(
    tibble::tribble(
      ~year, ~value,
      2015, 10,
      2015, 20,
      2016, NA,
      2017, NA
    ) |>
      fill_linear(value),
    "Duplicate year"
  )
})

testthat::test_that("fill_linear handles all-NA group without error", {
  tibble::tribble(
    ~year, ~value,
    2015, NA,
    2016, NA,
    2017, NA
  ) |>
    fill_linear(value) |>
    pointblank::expect_col_vals_null(value) |>
    pointblank::expect_col_vals_equal(
      source_value,
      "Gap not filled"
    )
})

testthat::test_that("fill_linear handles mixed groups with single non-NA", {
  # One group has 1 non-NA, another has 2+
  tibble::tribble(
    ~category, ~year, ~value,
    "a", 2015, NA,
    "a", 2016, 10,
    "a", 2017, NA,
    "b", 2015, 1,
    "b", 2016, NA,
    "b", 2017, 3
  ) |>
    fill_linear(value, .by = "category") |>
    pointblank::expect_col_vals_not_null(value) |>
    pointblank::expect_col_vals_equal(
      value,
      c(10, 10, 10),
      preconditions = \(df) df |> dplyr::filter(category == "a")
    ) |>
    pointblank::expect_col_vals_equal(
      value,
      c(1, 2, 3),
      preconditions = \(df) df |> dplyr::filter(category == "b")
    )
})

testthat::test_that("fill_linear handles no NAs without error", {
  tibble::tribble(
    ~year, ~value,
    2015, 1,
    2016, 2,
    2017, 3
  ) |>
    fill_linear(value) |>
    pointblank::expect_col_vals_equal(value, c(1, 2, 3)) |>
    pointblank::expect_col_vals_equal(source_value, "Original")
})

# fill_linear log_space --------------------------------------------------------

testthat::test_that("fill_linear log_space uses the geometric midpoint", {
  gap <- tibble::tribble(
    ~year, ~value,
    0, 1,
    5, NA,
    10, 1024
  )

  log_result <- gap |>
    fill_linear(value, log_space = TRUE)
  linear_result <- gap |>
    fill_linear(value)

  # Geometric (constant-growth) midpoint of 1 and 1024 is 32, not the
  # arithmetic midpoint 512.5 that linear interpolation returns.
  testthat::expect_equal(log_result$value[2], 32)
  testthat::expect_false(isTRUE(all.equal(log_result$value[2], 512.5)))
  testthat::expect_equal(log_result$source_value[2], "Log-linear interpolation")

  testthat::expect_equal(linear_result$value[2], 512.5)
  testthat::expect_equal(linear_result$source_value[2], "Linear interpolation")
})

testthat::test_that("fill_linear log_space falls back to linear on non-positive anchors", {
  # A zero anchor makes log space undefined -> linear fallback.
  tibble::tribble(
    ~year, ~value,
    0, 0,
    5, NA,
    10, 10
  ) |>
    fill_linear(value, log_space = TRUE) |>
    testthat::expect_equal(
      tibble::tribble(
        ~year, ~value, ~source_value,
        0, 0, "Original",
        5, 5, "Linear interpolation",
        10, 10, "Original"
      )
    )

  # A negative anchor is likewise undefined -> linear fallback.
  tibble::tribble(
    ~year, ~value,
    0, -4,
    5, NA,
    10, 8
  ) |>
    fill_linear(value, log_space = TRUE) |>
    testthat::expect_equal(
      tibble::tribble(
        ~year, ~value, ~source_value,
        0, -4, "Original",
        5, 2, "Linear interpolation",
        10, 8, "Original"
      )
    )
})

testthat::test_that("fill_linear log_space mixes log and linear segments in one series", {
  # First gap has positive anchors (log); second gap is bracketed by a zero
  # anchor (linear). Both segments coexist with distinct source labels.
  tibble::tribble(
    ~year, ~value,
    0, 1,
    5, NA,
    10, 1024,
    15, NA,
    20, 0
  ) |>
    fill_linear(value, log_space = TRUE) |>
    testthat::expect_equal(
      tibble::tribble(
        ~year, ~value, ~source_value,
        0, 1, "Original",
        5, 32, "Log-linear interpolation",
        10, 1024, "Original",
        15, 512, "Linear interpolation",
        20, 0, "Original"
      )
    )
})

testthat::test_that("fill_linear log_space interpolates per group", {
  tibble::tribble(
    ~category, ~year, ~value,
    "a", 0, 1,
    "a", 5, NA,
    "a", 10, 1024,
    "b", 0, 2,
    "b", 5, NA,
    "b", 10, 200
  ) |>
    fill_linear(value, log_space = TRUE, .by = "category") |>
    testthat::expect_equal(
      tibble::tribble(
        ~category, ~year, ~value, ~source_value,
        "a", 0, 1, "Original",
        "a", 5, 32, "Log-linear interpolation",
        "a", 10, 1024, "Original",
        "b", 0, 2, "Original",
        "b", 5, 20, "Log-linear interpolation",
        "b", 10, 200, "Original"
      )
    )
})

testthat::test_that("fill_linear log_space works on the smoothing (grouped) path", {
  noisy <- tibble::tribble(
    ~category, ~year, ~value,
    "a", 2010, 10,
    "a", 2011, 12,
    "a", 2012, 8,
    "a", 2013, NA,
    "a", 2014, NA,
    "a", 2015, 40,
    "a", 2016, 44,
    "a", 2017, 36
  )

  res_lin <- noisy |>
    fill_linear(value, value_smooth_window = 3, .by = "category")
  res_log <- noisy |>
    fill_linear(
      value,
      log_space = TRUE,
      value_smooth_window = 3,
      .by = "category"
    )

  # Both fill the interior gap; the log-space fill differs from the linear one
  # on a rising series and is labelled distinctly.
  testthat::expect_false(any(is.na(res_lin$value[4:5])))
  testthat::expect_false(any(is.na(res_log$value[4:5])))
  testthat::expect_false(isTRUE(all.equal(
    res_lin$value[4:5],
    res_log$value[4:5]
  )))
  testthat::expect_true(
    any(res_log$source_value == "Log-linear interpolation")
  )
})

testthat::test_that("fill_linear default arguments match linear behaviour (regression lock)", {
  # Omitting log_space must be byte-identical to log_space = FALSE, and must
  # reproduce the established linear interpolation output.
  grouped_default <- fill_linear_fixture() |>
    fill_linear(value, .by = "category")
  grouped_explicit <- fill_linear_fixture() |>
    fill_linear(value, log_space = FALSE, .by = "category")
  testthat::expect_equal(grouped_default, grouped_explicit)

  ungrouped_default <- simple_linear_series() |>
    fill_linear(value)
  ungrouped_explicit <- simple_linear_series() |>
    fill_linear(value, log_space = FALSE)
  testthat::expect_equal(ungrouped_default, ungrouped_explicit)

  grouped_default |>
    dplyr::filter(category == "b") |>
    dplyr::pull(value) |>
    testthat::expect_equal(c(1, 2, 3, 4, 5, 5))
  grouped_default |>
    dplyr::filter(category == "b") |>
    dplyr::pull(source_value) |>
    testthat::expect_equal(c(
      "Original",
      "Linear interpolation",
      "Linear interpolation",
      "Linear interpolation",
      "Original",
      "Last value carried forward"
    ))
})

# fill_linear order invariance and smoothing edge cases ------------------------

testthat::test_that("fill_linear ignores input row order without groups", {
  filled <- purrr::map(
    hostile_row_orders(one_series_with_gaps()),
    \(df) whep::fill_linear(df, value)
  )

  # Every row order gives the same answer, and that answer is the time-ordered
  # one: carrying a value forward or backward reads neighbouring rows, so a
  # reversed input used to swap the two carry directions, and a scrambled one
  # left both outer gaps unfilled.
  testthat::expect_identical(filled$reversed, filled$as_given)
  testthat::expect_identical(filled$scrambled, filled$as_given)
  testthat::expect_equal(filled$as_given$year, 2015:2020)
  testthat::expect_equal(filled$as_given$value, c(3, 3, 2, 1, 0, 0))
  testthat::expect_equal(
    filled$as_given$source_value,
    c(
      "First value carried backwards",
      "Original",
      "Linear interpolation",
      "Linear interpolation",
      "Original",
      "Last value carried forward"
    )
  )
})

testthat::test_that("fill_linear ignores input row order within groups", {
  plain <- purrr::map(
    hostile_row_orders(fill_linear_fixture()),
    \(df) whep::fill_linear(df, value, .by = "category")
  )
  testthat::expect_identical(plain$reversed, plain$as_given)
  testthat::expect_identical(plain$scrambled, plain$as_given)

  # The smoothing window takes the per-group fallback path, which sorts too.
  smoothed <- purrr::map(
    hostile_row_orders(fill_linear_fixture()),
    \(df) {
      whep::fill_linear(df, value, value_smooth_window = 3, .by = "category")
    }
  )
  testthat::expect_identical(smoothed$reversed, smoothed$as_given)
  testthat::expect_identical(smoothed$scrambled, smoothed$as_given)

  # Log space is order-invariant as well, on both grouped paths.
  logged <- purrr::map(
    hostile_row_orders(fill_linear_fixture()),
    \(df) whep::fill_linear(df, value, log_space = TRUE, .by = "category")
  )
  testthat::expect_identical(logged$reversed, logged$as_given)
  testthat::expect_identical(logged$scrambled, logged$as_given)
})

testthat::test_that("fill_linear survives a smoothing window with no anchor", {
  # The 3-year centered mean of a series whose gaps are one year apart is NA at
  # every position, so the group has nothing to interpolate between or carry.
  # The grouped path used to take `first_valid` from an empty vector and abort
  # on `if (NA > 1L)`; the ungrouped path guarded it. Both now leave the gaps.
  alternating <- tibble::tribble(
    ~category, ~year, ~value,
    "a", 2001, 1,
    "a", 2002, NA,
    "a", 2003, 2,
    "a", 2004, NA,
    "a", 2005, 3
  )
  expected_source <- c(
    "Original",
    "Gap not filled",
    "Original",
    "Gap not filled",
    "Original"
  )

  grouped <- whep::fill_linear(
    alternating,
    value,
    value_smooth_window = 3,
    .by = "category"
  )
  testthat::expect_equal(grouped$value, c(1, NA, 2, NA, 3))
  testthat::expect_equal(grouped$source_value, expected_source)

  # Grouped and ungrouped must agree: they share one filling core precisely so
  # that an edge case cannot be handled on one path and not the other.
  ungrouped <- alternating |>
    dplyr::select(-category) |>
    whep::fill_linear(value, value_smooth_window = 3)
  testthat::expect_equal(ungrouped$value, grouped$value)
  testthat::expect_equal(ungrouped$source_value, grouped$source_value)

  # A window wider than the group is the same situation, not an error either.
  wider_than_group <- whep::fill_linear(
    alternating,
    value,
    value_smooth_window = 7,
    .by = "category"
  )
  testthat::expect_equal(wider_than_group$value, c(1, NA, 2, NA, 3))
  testthat::expect_equal(wider_than_group$source_value, expected_source)
})

testthat::test_that("fill_linear re-sorts rows that moved since last call", {
  # `fill_linear()` used to stamp a `.whep_sorted_by` attribute and trust it on
  # the next call without looking at the rows. `setorderv()` drops a data.table
  # key but keeps that attribute, so a caller that reordered its rows between
  # two calls got the carry direction of the order it no longer had -- and a
  # "sorted" key stamped on rows that were not in that order.
  descending <- data.table::data.table(
    category = "a",
    year = 2004:2001,
    value = c(4, NA, NA, NA)
  )
  data.table::setattr(descending, ".whep_sorted_by", c("category", "year"))

  filled <- whep::fill_linear(
    descending,
    value,
    fill_forward = FALSE,
    .by = "category",
    .copy = FALSE
  )

  # 2004 is the only observation, so the three earlier years can only be filled
  # backwards -- which `fill_forward = FALSE` leaves as the sole option.
  testthat::expect_equal(filled$year, 2001:2004)
  testthat::expect_equal(filled$value, c(4, 4, 4, 4))
  testthat::expect_equal(
    filled$source_value,
    c(rep("First value carried backwards", 3), "Original")
  )
  # Whatever sort the result claims, the rows must actually be in it.
  testthat::expect_true(
    whep:::.is_sorted_by(filled, c("category", "year"))
  )
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
      c("Original", "Filled with sum", NA)
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
      c("Original", "Filled with sum", NA)
    )
})

testthat::test_that("fill_sum start_with_zero toggles behaviour", {
  contiguous_gaps <- tibble::tribble(
    ~year, ~value, ~change_variable,
    2015, NA, 1,
    2016, NA, 2,
    2017, NA, 3,
    2018, NA, 4
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

testthat::test_that("fill_sum creates source column with dynamic name based on value_col", {
  data <- tibble::tribble(
    ~year, ~my_variable, ~change_variable,
    2015, 10, 0,
    2016, NA, 2,
    2017, 20, 3
  )

  result <- data |>
    fill_sum(my_variable, change_variable)

  # Should create source_my_variable, not source_value
  testthat::expect_true("source_my_variable" %in% names(result))
  testthat::expect_false("source_value" %in% names(result))

  result |>
    pointblank::expect_col_vals_in_set(
      source_my_variable,
      c("Original", "Filled with sum")
    )
})

# fill_proxy_growth ------------------------------------------------------------------

test_that("fill_proxy_growth fills missing values using proxy growth rates", {
  data <- tibble::tribble(
    ~country, ~year, ~gdp, ~population,
    "ESP", 2010, 100, 46,
    "ESP", 2011, NA, 47,
    "ESP", 2012, 120, 48,
    "ESP", 2013, NA, 49
  )

  result <- fill_proxy_growth(
    data,
    value_col = gdp,
    proxy_col = "population",
    .by = "country",
    verbose = FALSE
  )

  # Should have filled the NA values
  expect_false(any(is.na(result$gdp)))
})

test_that("fill_proxy_growth respects max_gap parameter", {
  data <- tibble::tribble(
    ~year, ~value, ~proxy,
    2010, 100, 1000,
    2011, NA, 1100,
    2012, NA, 1200,
    2013, NA, 1300,
    2014, 150, 1400
  )

  result <- fill_proxy_growth(
    data,
    value_col = value,
    proxy_col = "proxy",
    max_gap = 2,
    verbose = FALSE
  )

  # With max_gap = 2, should not fill 3 consecutive NAs
  expect_true(is.na(result$value[3]))
})

test_that("fill_proxy_growth works with grouping", {
  data <- tibble::tribble(
    ~country, ~year, ~emissions, ~gdp,
    "ESP", 2010, 100, 1000,
    "ESP", 2011, NA, 1100,
    "ESP", 2012, 130, 1200,
    "FRA", 2010, 200, 2000,
    "FRA", 2011, NA, 2200,
    "FRA", 2012, 250, 2400
  )

  result <- fill_proxy_growth(
    data,
    value_col = emissions,
    proxy_col = "gdp",
    .by = "country",
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

test_that("fill_proxy_growth groups proxy growth by region (var:group)", {
  # Advanced "variable:group" syntax: growth is taken from `gdp` aggregated
  # over `region`, not from the value column's own series. ESP and FRA share
  # region "EU", so ESP's gaps are backfilled with the region-mean gdp growth
  # (mean of the two countries' growths), not ESP's own gdp growth.
  data <- tibble::tribble(
    ~region, ~country, ~year, ~value, ~gdp,
    "EU", "ESP", 2000, NA, 100,
    "EU", "ESP", 2001, NA, 120,
    "EU", "ESP", 2002, 500, 150,
    "EU", "FRA", 2000, 1000, 200,
    "EU", "FRA", 2001, 1200, 260,
    "EU", "FRA", 2002, 1400, 299
  )

  result <- fill_proxy_growth(
    data,
    value_col = value,
    proxy_col = "gdp:region",
    .by = "country",
    verbose = FALSE
  )

  # Region-mean gdp growth: 2001 = mean(0.20, 0.30) = 0.25;
  # 2002 = mean(0.25, 0.15) = 0.20. Backfill from the 2002 anchor (500):
  #   value_2001 = 500 / 1.20; value_2000 = value_2001 / 1.25.
  esp <- result |>
    dplyr::filter(country == "ESP") |>
    dplyr::arrange(year)

  expect_equal(esp$value[esp$year == 2001], 500 / 1.20, tolerance = 1e-6)
  expect_equal(
    esp$value[esp$year == 2000],
    500 / (1.20 * 1.25),
    tolerance = 1e-6
  )

  # The region-grouped result must differ from ESP's own-gdp backfill, which
  # would give 500 / 1.25 for 2001. This confirms growth is grouped by region.
  expect_false(isTRUE(all.equal(esp$value[esp$year == 2001], 500 / 1.25)))
})

test_that("fill_proxy_growth extrapolates per group, not across groups", {
  # Regression: .parse_proxy_spec used to return `group_vars` while
  # downstream code read `present_group_vars` (unset), collapsing all
  # groups into one. Under the bug, a group with a slow-growing proxy
  # would be pulled towards a neighbour's fast-growing proxy.
  data <- tibble::tribble(
    ~country, ~year, ~value, ~proxy,
    "slow",   2000,    NA,    10,
    "slow",   2001,    NA,    11,
    "slow",   2002,   100,    12,
    "fast",   2000,    NA,    10,
    "fast",   2001,    NA,    50,
    "fast",   2002,  1000,   100
  )

  result <- fill_proxy_growth(
    data,
    value_col = value,
    proxy_col = "proxy",
    .by = "country",
    verbose = FALSE
  )

  # Expected per-group backfill, walking back from the 2002 anchor
  # using the local proxy growth. The slow group's 2000 value is
  # anchor times (proxy_2000 / proxy_2002); the fast group uses its
  # own proxy series. A bug that averaged growth rates across groups
  # would pull both values toward the same (wrong) intermediate.
  slow_2000 <- result |>
    dplyr::filter(country == "slow", year == 2000L) |>
    dplyr::pull(value)
  fast_2000 <- result |>
    dplyr::filter(country == "fast", year == 2000L) |>
    dplyr::pull(value)

  expect_equal(slow_2000, 100 * (10 / 12), tolerance = 1e-6)
  expect_equal(fast_2000, 1000 * (10 / 100), tolerance = 1e-6)
})

test_that("fill_proxy_growth returns same number of rows", {
  data <- tibble::tribble(
    ~year, ~value, ~proxy,
    2010, 100, 1000,
    2011, NA, 1100,
    2012, 120, 1200
  )

  result <- fill_proxy_growth(
    data,
    value_col = value,
    proxy_col = "proxy",
    verbose = FALSE
  )

  expect_equal(nrow(result), nrow(data))
})

# Hierarchical Segmented Interpolation -----------------------------------------

test_that("fill_proxy_growth uses hierarchical segmentation with intermediate proxy data", {
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

  result <- fill_proxy_growth(
    data_wages,
    value_col = household_ppp,
    proxy_col = c("formal_ppp", "gdp_pc_constant"),
    .by = "country",
    output_format = "detailed",
    verbose = FALSE
  )

  # All gaps should be filled
  expect_false(any(is.na(result$household_ppp)))

  # Original values should be preserved
  expect_equal(result$household_ppp[result$year == 2008], 100)
  expect_equal(result$household_ppp[result$year == 2019], 150)

  # Check that source column indicates bridge interpolation was used
  middle_sources <- result |>
    dplyr::filter(year >= 2009, year <= 2018) |>
    dplyr::pull(source_household_ppp)

  expect_true(any(grepl("bridge", middle_sources)))
})

test_that("fill_proxy_growth maintains continuity without jumps between segments", {
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

  result <- fill_proxy_growth(
    data_test,
    value_col = primary,
    proxy_col = c("proxy1", "proxy2"),
    verbose = FALSE
  )

  # Check for continuity: bridge should connect smoothly
  values <- result$primary

  # First and last values should match original anchors
  expect_equal(values[1], 100)
  expect_equal(values[6], 200)

  # Check that bridge source was used (not simple forward/backfill)
  expect_true(any(grepl("bridge", result$source_primary)))

  # Values should increase (since both proxies increase monotonically)
  expect_true(all(diff(values) >= 0))

  # Growth rates should be reasonable (smooth with bridge adjustment)
  growth_rates <- diff(values) / values[-length(values)]
  expect_true(all(abs(growth_rates) < 1.0))
})

test_that("fill_proxy_growth respects proxy hierarchy in segmentation", {
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

  result <- fill_proxy_growth(
    data_hierarchy,
    value_col = value,
    proxy_col = c("proxy1", "proxy2"),
    output_format = "detailed",
    verbose = FALSE
  )

  # Should have used better proxy for middle segment
  expect_false(any(is.na(result$value)))
  expect_equal(nrow(result), 6)
})

test_that("fill_proxy_growth backward compatible: single proxy behaves as before", {
  # Without intermediate data, should work exactly as old version
  data_simple <- tibble::tribble(
    ~year, ~value, ~proxy,
    2010, 100, 1000,
    2011, NA, 1100,
    2012, NA, 1200,
    2013, NA, 1300,
    2014, 150, 1400
  )

  result <- fill_proxy_growth(
    data_simple,
    value_col = value,
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

test_that("fill_proxy_growth handles case with no intermediate data gracefully", {
  # Hierarchical proxies but none have intermediate data
  data_no_intermediate <- tibble::tribble(
    ~year, ~value, ~proxy1, ~proxy2,
    2010, 100, NA, 50,
    2011, NA, NA, 52,
    2012, NA, NA, 54,
    2013, 150, NA, 56
  )

  result <- fill_proxy_growth(
    data_no_intermediate,
    value_col = value,
    proxy_col = c("proxy1", "proxy2"),
    verbose = FALSE
  )

  # Should fall back to proxy2 for entire gap
  expect_false(any(is.na(result$value)))
  expect_equal(result$value[result$year == 2010], 100)
  expect_equal(result$value[result$year == 2013], 150)
})

test_that("fill_proxy_growth preserves original data points when they exist", {
  data_mixed <- tibble::tribble(
    ~year, ~value, ~proxy,
    2010, 100, 1000,
    2011, NA, 1100,
    2012, 120, 1200,
    2013, NA, 1300,
    2014, 150, 1400
  )

  result <- fill_proxy_growth(
    data_mixed,
    value_col = value,
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

test_that("fill_proxy_growth preserves non-NA values", {
  data <- tibble::tribble(
    ~year, ~value, ~proxy,
    2010, 100, 1000,
    2011, NA, 1100,
    2012, 120, 1200
  )

  result <- fill_proxy_growth(
    data,
    value_col = value,
    proxy_col = "proxy",
    verbose = FALSE
  )

  # Original non-NA values should be unchanged
  expect_equal(result$value[1], 100)
  expect_equal(result$value[3], 120)
})

test_that("fill_proxy_growth extrapolates at ends with hierarchical growth", {
  data <- tibble::tribble(
    ~year, ~value, ~proxy1, ~proxy2,
    2010, NA, 100, 50,
    2011, NA, 103, 51,
    2012, 120, 106, 52,
    2013, NA, 109, 53,
    2014, NA, 112, 54
  )

  res <- fill_proxy_growth(
    data,
    value_col = value,
    proxy_col = c("proxy1", "proxy2"),
    max_gap_linear = 1,
    output_format = "detailed",
    verbose = FALSE
  )

  # Start and end should be filled (edge extrapolation)
  expect_false(any(is.na(res$value)))
  # Sources at ends should be growth_* (not *_bridge)
  expect_true(any(grepl("^growth_", res$source_value)))
})

test_that("fill_proxy_growth proxy_smooth_window smooths proxy before growth calc", {
  # Noisy proxy data - smoothing should reduce volatility in growth rates

  data_noisy <- tibble::tribble(
    ~year, ~value, ~proxy,
    2010, 100, 1000,
    2011, NA, 1200,
    2012, NA, 900,
    2013, NA, 1300,
    2014, 150, 1100
  )

  result_no_smooth <- fill_proxy_growth(
    data_noisy,
    value_col = value,
    proxy_col = "proxy",
    proxy_smooth_window = 1,
    verbose = FALSE
  )

  result_smooth <- fill_proxy_growth(
    data_noisy,
    value_col = value,
    proxy_col = "proxy",
    proxy_smooth_window = 3,
    verbose = FALSE
  )

  # Both should fill gaps
  expect_false(any(is.na(result_no_smooth$value)))
  expect_false(any(is.na(result_smooth$value)))

  # Original values preserved in both

  expect_equal(result_no_smooth$value[1], 100)
  expect_equal(result_smooth$value[1], 100)
  expect_equal(result_no_smooth$value[5], 150)
  expect_equal(result_smooth$value[5], 150)
})

test_that("fill_proxy_growth value_smooth_window smooths value before filling", {
  # Data with high variability in value column
  data_noisy_value <- tibble::tribble(
    ~year, ~value, ~proxy,
    2010, 100, 1000,
    2011, 130, 1050,
    2012, 90, 1100,
    2013, NA, 1150,
    2014, NA, 1200,
    2015, 110, 1250,
    2016, 140, 1300,
    2017, 95, 1350
  )

  result_no_smooth <- fill_proxy_growth(
    data_noisy_value,
    value_col = value,
    proxy_col = "proxy",
    value_smooth_window = NULL,
    verbose = FALSE
  )

  result_smooth <- fill_proxy_growth(
    data_noisy_value,
    value_col = value,
    proxy_col = "proxy",
    value_smooth_window = 3,
    verbose = FALSE
  )

  # Both should fill gaps
  expect_false(any(is.na(result_no_smooth$value)))
  expect_false(any(is.na(result_smooth$value)))

  # Original non-NA values must be preserved exactly
  expect_equal(result_no_smooth$value[1], 100)
  expect_equal(result_smooth$value[1], 100)
  expect_equal(result_no_smooth$value[2], 130)
  expect_equal(result_smooth$value[2], 130)
  expect_equal(result_no_smooth$value[6], 110)
  expect_equal(result_smooth$value[6], 110)
})

test_that("fill_proxy_growth value_smooth_window NULL is default behavior", {
  data <- tibble::tribble(
    ~year, ~value, ~proxy,
    2010, 100, 1000,
    2011, NA, 1100,
    2012, 120, 1200
  )

  result_default <- fill_proxy_growth(
    data,
    value_col = value,
    proxy_col = "proxy",
    verbose = FALSE
  )

  result_null <- fill_proxy_growth(
    data,
    value_col = value,
    proxy_col = "proxy",
    value_smooth_window = NULL,
    verbose = FALSE
  )

  expect_equal(result_default$value, result_null$value)
})

test_that("fill_proxy_growth both smooth windows can be used together", {
  data <- tibble::tribble(
    ~year, ~value, ~proxy,
    2010, 100, 1000,
    2011, 120, 1200,
    2012, 80, 900,
    2013, NA, 1300,
    2014, NA, 1100,
    2015, 110, 1250,
    2016, 90, 950,
    2017, 130, 1350
  )

  result <- fill_proxy_growth(
    data,
    value_col = value,
    proxy_col = "proxy",
    value_smooth_window = 3,
    proxy_smooth_window = 2,
    verbose = FALSE
  )

  # Gaps should be filled
  expect_false(any(is.na(result$value)))

  # Original values preserved
  expect_equal(result$value[1], 100)
  expect_equal(result$value[2], 120)
  expect_equal(result$value[3], 80)
  expect_equal(result$value[6], 110)
})

test_that("fill_proxy_growth value_smooth_window preserves original non-NA values", {
  # Critical test: smoothing should NOT turn original values into NA
  # even when smoothing window extends into gaps
  data <- tibble::tribble(
    ~year, ~value, ~proxy,
    2010, 100, 1000,
    2011, NA, 1100,
    2012, NA, 1200,
    2013, 150, 1300,
    2014, NA, 1400,
    2015, NA, 1500,
    2016, 200, 1600
  )

  result <- fill_proxy_growth(
    data,
    value_col = value,
    proxy_col = "proxy",
    value_smooth_window = 3,
    verbose = FALSE
  )

  # Original values at 2010, 2013, 2016 MUST be preserved exactly
  expect_equal(result$value[result$year == 2010], 100)
  expect_equal(result$value[result$year == 2013], 150)
  expect_equal(result$value[result$year == 2016], 200)

  # All gaps should be filled
  expect_false(any(is.na(result$value)))

  # Source should indicate original for non-NA inputs
  expect_equal(result$source_value[result$year == 2010], "original")
  expect_equal(result$source_value[result$year == 2013], "original")
  expect_equal(result$source_value[result$year == 2016], "original")
})

test_that("fill_proxy_growth works with capitalized time column", {
  data <- tibble::tribble(
    ~country, ~Year, ~gdp, ~population,
    "ESP", 2010, 100, 46,
    "ESP", 2011, NA, 47,
    "ESP", 2012, 120, 48,
    "ESP", 2013, NA, 49
  )

  result <- fill_proxy_growth(
    data,
    value_col = gdp,
    proxy_col = "population",
    time_col = Year,
    .by = "country",
    verbose = FALSE
  )

  expect_false(any(is.na(result$gdp)))
  expect_equal(nrow(result), 4)
})

# Sort reuse tests ---------------------------------------------------------

test_that(".is_sorted_by detects sorted and unsorted data", {
  sorted <- data.frame(a = c(1, 1, 2, 2), b = c(1, 2, 1, 2))
  unsorted <- data.frame(a = c(2, 1, 1, 2), b = c(1, 2, 1, 2))
  ties <- data.frame(a = c(1, 1, 1), b = c(3, 2, 1))

  expect_true(whep:::.is_sorted_by(sorted, c("a", "b")))
  expect_false(whep:::.is_sorted_by(unsorted, c("a", "b")))
  expect_false(whep:::.is_sorted_by(ties, c("a", "b")))
  expect_true(whep:::.is_sorted_by(ties, "a"))

  # A missing value has no place in an order, and every comparison against it
  # answers NA: `c(2, NA, 1)` used to come back certified as sorted. A column
  # that cannot be judged is not certified, so its caller sorts.
  missing_middle <- data.frame(a = c(2, NA, 1))
  expect_false(whep:::.is_sorted_by(missing_middle, "a"))
  # Once earlier columns have broken every tie, later ones cannot change the
  # answer, so a missing value there is still fine.
  expect_true(
    whep:::.is_sorted_by(data.frame(a = c(1, 2), b = c(NA, 1)), c("a", "b"))
  )
})

test_that("fill_proxy_growth gives identical results regardless of input order", {
  # Same data, two row orders.
  df_sorted <- tibble::tribble(
    ~area, ~year, ~food, ~feed, ~proxy,
    "A",   2000,   10,    5,    100,
    "A",   2001,   NA,   NA,    110,
    "A",   2002,   12,    6,    120,
    "B",   2000,   20,   10,    200,
    "B",   2001,   NA,   NA,    210,
    "B",   2002,   22,   11,    220,
  )
  df_unsorted <- df_sorted[c(4:6, 1:3), ]

  run_chain <- function(df) {
    r <- whep::fill_proxy_growth(
      df,
      food,
      proxy_col = "proxy",
      time_col = year,
      .by = "area",
      verbose = FALSE
    )
    whep::fill_proxy_growth(
      r,
      feed,
      proxy_col = "proxy",
      time_col = year,
      .by = "area",
      verbose = FALSE
    )
  }

  res_sorted <- run_chain(df_sorted)
  res_unsorted <- run_chain(df_unsorted)

  # Align row order for comparison — results should match by key.
  key <- c("area", "year")
  compare <- function(x) {
    x <- x[do.call(order, x[key]), ]
    rownames(x) <- NULL
    x
  }

  expect_equal(compare(res_sorted), compare(res_unsorted))
})

test_that("fill_proxy_growth weights a proxy by the previous period", {
  # `pop` is the growth proxy, aggregated over the region and weighted by `pop`
  # itself, which is the documented `"variable:group[weight]"` syntax. Country
  # b's proxy has a hole in 2011, so b has no 2012 growth rate and that row is
  # dropped. The weights used to be lagged *after* that drop, which made b's
  # 2013 weight country a's 2014 population, and left the first surviving row of
  # each group with no weight at all.
  regional <- tibble::tribble(
    ~country, ~region, ~year, ~gdp, ~pop,
    "a",      "eu",    2010,    10,   100,
    "a",      "eu",    2011,    11,   110,
    "a",      "eu",    2012,    12,   120,
    "a",      "eu",    2013,    NA,   130,
    "a",      "eu",    2014,    NA,   140,
    "b",      "eu",    2010,    20,  1000,
    "b",      "eu",    2011,    21,    NA,
    "b",      "eu",    2012,    22,  1000,
    "b",      "eu",    2013,    23,  2000,
    "b",      "eu",    2014,    24,  2000
  )

  filled <- whep::fill_proxy_growth(
    regional,
    gdp,
    proxy_col = "pop:region[pop]",
    time_col = year,
    .by = "country",
    output_format = "detailed",
    verbose = FALSE
  )

  # 2013 growth: each country's own rate, weighted by its own 2012 population.
  growth_a <- (130 - 120) / 120
  growth_b <- (2000 - 1000) / 1000
  expected <- (growth_a * 120 + growth_b * 1000) / (120 + 1000)
  # What the previous-surviving-row weight gave instead: a's 2014 population.
  wrong <- (growth_a * 120 + growth_b * 140) / (120 + 140)

  growth_2013 <- filled$growth_1_pop_region_w[filled$year == 2013]
  testthat::expect_equal(growth_2013, rep(expected, 2))
  testthat::expect_false(isTRUE(all.equal(growth_2013[1], wrong)))

  # And the number a user reads: a's 2013 gap grows off its 2012 value.
  testthat::expect_equal(
    filled$gdp[filled$country == "a" & filled$year == 2013],
    12 * (1 + expected)
  )
  testthat::expect_equal(nrow(filled), nrow(regional))
})

test_that(".fg_weighted_growth falls back to the plain mean", {
  # Weighted, and reporting how many rows carried a weight.
  weighted <- whep:::.fg_weighted_growth(c(0.1, 1), c(100, 900))
  testthat::expect_equal(weighted$g_val, (0.1 * 100 + 1 * 900) / 1000)
  testthat::expect_equal(weighted$o_val, 2L)

  # A missing, non-finite or non-positive weight carries nothing, so it drops
  # out of both the mean and the count.
  partial <- whep:::.fg_weighted_growth(c(0.1, 1, 2, 3), c(100, NA, Inf, 0))
  testthat::expect_equal(partial$g_val, 0.1)
  testthat::expect_equal(partial$o_val, 1L)

  # With no usable weight left there is nothing to weight by, and the unweighted
  # mean stands in rather than the cell going missing.
  none <- whep:::.fg_weighted_growth(c(0.2, 0.4), c(NA, -1))
  testthat::expect_equal(none$g_val, 0.3)
  testthat::expect_equal(none$o_val, 2L)
})

test_that("fill_proxy_growth preserves sort order through chained calls", {
  df <- tibble::tribble(
    ~area, ~year, ~food, ~feed, ~proxy,
    "A",   2000,   10,    5,    100,
    "A",   2001,   NA,   NA,    110,
    "A",   2002,   12,    6,    120,
    "B",   2000,   20,   10,    200,
    "B",   2001,   NA,   NA,    210,
    "B",   2002,   22,   11,    220,
  )

  sort_cols <- c("area", "year")
  expect_true(whep:::.is_sorted_by(df, sort_cols))

  r1 <- whep::fill_proxy_growth(
    df,
    food,
    proxy_col = "proxy",
    time_col = year,
    .by = "area",
    verbose = FALSE
  )
  expect_true(whep:::.is_sorted_by(r1, sort_cols))

  r2 <- whep::fill_proxy_growth(
    r1,
    feed,
    proxy_col = "proxy",
    time_col = year,
    .by = "area",
    verbose = FALSE
  )
  expect_true(whep:::.is_sorted_by(r2, sort_cols))
})

# interp_vec -------------------------------------------------------------------

# Shared fixture for the anti-drift checks: one series whose gaps are bracketed
# by strictly positive anchors, expressed both as a data frame (for
# `fill_linear()`) and as anchor/output vectors (for `interp_vec()`).
interp_vec_series <- function() {
  tibble::tribble(
    ~year, ~value,
    2000, 2,
    2001, NA,
    2002, NA,
    2003, NA,
    2004, 32,
    2005, NA,
    2006, 200
  )
}

# The same series but with a zero anchor, which makes log space undefined and
# forces the linear fallback on both entry points.
interp_vec_series_zero <- function() {
  tibble::tribble(
    ~year, ~value,
    2000, 0,
    2001, NA,
    2002, NA,
    2003, NA,
    2004, 32,
    2005, NA,
    2006, 200
  )
}

testthat::test_that("interp_vec log space uses the constant growth rate", {
  # Geometric (constant-growth) midpoint of 1 and 1024 is 32, not the
  # arithmetic midpoint 512.5 that linear interpolation returns.
  log_result <- whep::interp_vec(
    c(2000, 2010),
    c(1, 1024),
    xout = 2005,
    log_space = TRUE
  )
  testthat::expect_equal(log_result$y, 32)
  testthat::expect_equal(log_result$method, "loglinear")

  linear_result <- whep::interp_vec(c(2000, 2010), c(1, 1024), xout = 2005)
  testthat::expect_equal(linear_result$y, 512.5)
  testthat::expect_equal(linear_result$method, "linear")

  # Off-midpoint positions follow the closed-form constant-growth path.
  xout <- c(2002, 2005, 2008)
  fraction <- (xout - 2000) / 10
  whep::interp_vec(c(2000, 2010), c(1, 1024), xout, log_space = TRUE)$y |>
    testthat::expect_equal(exp(log(1) + fraction * (log(1024) - log(1))))
})

testthat::test_that("interp_vec falls back to linear on non-positive anchors", {
  # A zero anchor makes log space undefined -> linear fallback.
  zero_anchor <- whep::interp_vec(
    c(0, 10),
    c(0, 10),
    xout = 5,
    log_space = TRUE
  )
  testthat::expect_equal(zero_anchor$y, 5)
  testthat::expect_equal(zero_anchor$method, "linear")

  # A negative anchor is likewise undefined -> linear fallback.
  negative_anchor <- whep::interp_vec(
    c(0, 10),
    c(-4, 8),
    xout = 5,
    log_space = TRUE
  )
  testthat::expect_equal(negative_anchor$y, 2)
  testthat::expect_equal(negative_anchor$method, "linear")

  # Log and linear segments coexist in one call, each labelled on its own.
  mixed <- whep::interp_vec(
    c(0, 10, 20),
    c(1, 1024, 0),
    xout = c(5, 15),
    log_space = TRUE
  )
  testthat::expect_equal(mixed$y, c(32, 512))
  testthat::expect_equal(mixed$method, c("loglinear", "linear"))
})

testthat::test_that("interp_vec matches fill_linear(log_space = TRUE)", {
  # Anti-drift lock: both entry points must resolve the same gaps to the same
  # values, because both must route the log-space math through the same
  # internal helper.
  gap_years <- c(2001, 2002, 2003, 2005)

  filled <- interp_vec_series() |>
    whep::fill_linear(value, log_space = TRUE)
  anchors <- interp_vec_series() |>
    dplyr::filter(!is.na(value))
  direct <- whep::interp_vec(
    anchors$year,
    anchors$value,
    xout = gap_years,
    log_space = TRUE
  )

  testthat::expect_identical(
    direct$y,
    filled$value[match(gap_years, filled$year)]
  )
  testthat::expect_equal(direct$method, rep("loglinear", 4))
  testthat::expect_equal(
    filled$source_value[match(gap_years, filled$year)],
    rep("Log-linear interpolation", 4)
  )

  # The linear fallback must agree too, so a zero anchor cannot make the two
  # entry points diverge.
  filled_zero <- interp_vec_series_zero() |>
    whep::fill_linear(value, log_space = TRUE)
  anchors_zero <- interp_vec_series_zero() |>
    dplyr::filter(!is.na(value))
  direct_zero <- whep::interp_vec(
    anchors_zero$year,
    anchors_zero$value,
    xout = gap_years,
    log_space = TRUE
  )

  testthat::expect_identical(
    direct_zero$y,
    filled_zero$value[match(gap_years, filled_zero$year)]
  )
  testthat::expect_equal(
    direct_zero$method,
    c("linear", "linear", "linear", "loglinear")
  )
})

testthat::test_that("interp_vec matches fill_linear when log space is off", {
  gap_years <- c(2001, 2002, 2003, 2005)

  filled <- interp_vec_series() |>
    whep::fill_linear(value)
  anchors <- interp_vec_series() |>
    dplyr::filter(!is.na(value))

  whep::interp_vec(anchors$year, anchors$value, xout = gap_years)$y |>
    testthat::expect_identical(filled$value[match(gap_years, filled$year)])
})

testthat::test_that("interp_vec sorts anchors and keeps the xout order", {
  unsorted <- whep::interp_vec(
    x = c(2010, 2000, 2005),
    y = c(400, 100, 200),
    xout = c(2007, 2002),
    log_space = TRUE
  )
  sorted <- whep::interp_vec(
    x = c(2000, 2005, 2010),
    y = c(100, 200, 400),
    xout = c(2007, 2002),
    log_space = TRUE
  )

  testthat::expect_identical(unsorted, sorted)
  testthat::expect_equal(unsorted$method, c("loglinear", "loglinear"))
  # Output order follows `xout`, so the 2007 value comes first and is the
  # larger of the two.
  testthat::expect_true(unsorted$y[1] > unsorted$y[2])
})

testthat::test_that("interp_vec handles degenerate anchor sets", {
  # Fewer than two usable anchors: nothing to interpolate between.
  single <- whep::interp_vec(2000, 5, xout = c(2000, 2001), log_space = TRUE)
  testthat::expect_equal(single$y, c(NA_real_, NA_real_))
  testthat::expect_equal(single$method, c(NA_character_, NA_character_))

  # All-NA values leave no anchor at all.
  all_na <- whep::interp_vec(
    c(2000, 2005, 2010),
    rep(NA_real_, 3),
    xout = 2003,
    log_space = TRUE
  )
  testthat::expect_equal(all_na$y, NA_real_)
  testthat::expect_equal(all_na$method, NA_character_)

  # Missing values and non-finite positions drop out of the anchor set; the
  # two survivors still bracket the output point.
  partial <- whep::interp_vec(
    c(2000, 2005, Inf, 2010),
    c(1, NA, 7, 1024),
    xout = 2005,
    log_space = TRUE
  )
  testthat::expect_equal(partial$y, 32)
  testthat::expect_equal(partial$method, "loglinear")

  # Empty input is empty output, not an error.
  empty <- whep::interp_vec(
    c(2000, 2010),
    c(1, 1024),
    xout = numeric(0),
    log_space = TRUE
  )
  testthat::expect_equal(empty$y, numeric(0))
  testthat::expect_equal(empty$method, character(0))
})

testthat::test_that("interp_vec collapses tied anchor positions", {
  # A zero-length span cannot define a growth rate. Tied positions are averaged
  # once, up front, so the linear and log-space paths see the same anchors.
  # The tied 2010 anchors average to 1024, so 2005 is the geometric midpoint of
  # 1 and 1024, and 2015 the geometric midpoint of 1024 and 1048576.
  tied <- whep::interp_vec(
    x = c(2000, 2010, 2010, 2020),
    y = c(1, 1000, 1048, 1048576),
    xout = c(2005, 2010, 2015),
    log_space = TRUE
  )
  # 2010 is itself an anchor position once the tie is averaged, so it is
  # returned as that averaged value rather than interpolated.
  testthat::expect_equal(tied$y, c(32, 1024, 32768))
  testthat::expect_equal(tied$method, c("loglinear", "linear", "loglinear"))

  # Every anchor at the same position leaves a single usable anchor.
  degenerate <- whep::interp_vec(
    c(2000, 2000),
    c(4, 6),
    xout = 2000,
    log_space = TRUE
  )
  testthat::expect_equal(degenerate$y, NA_real_)
  testthat::expect_equal(degenerate$method, NA_character_)
})

testthat::test_that("interp_vec returns anchor positions bit-exactly", {
  # A caller that densifies a whole grid of positions passes the anchor
  # positions along with the gaps. Log space must not rebuild the anchor values
  # it was handed: `exp(log(3))` is 3.0000000000000004, not 3, which would
  # break a downstream bit-identical comparison. `expect_identical()`, not
  # `expect_equal()`, is the point of this test.
  anchor_x <- c(2000, 2010, 2020, 2030)
  anchor_y <- c(3, 7, 300, 11)
  on_anchors <- whep::interp_vec(
    anchor_x,
    anchor_y,
    xout = anchor_x,
    log_space = TRUE
  )
  testthat::expect_identical(on_anchors$y, anchor_y)
  testthat::expect_identical(on_anchors$method, rep("linear", 4))

  # A flat segment is the same trap: the midpoint of 7 and 7 is 7 exactly, but
  # a log-space round trip returns 6.999999999999999.
  flat <- whep::interp_vec(
    c(2000, 2010),
    c(7, 7),
    xout = c(2000, 2005, 2010),
    log_space = TRUE
  )
  testthat::expect_identical(flat$y[c(1L, 3L)], c(7, 7))

  # Interior positions are still interpolated in log space, so pinning the
  # anchors has not disabled the feature.
  whep::interp_vec(anchor_x, anchor_y, xout = 2005, log_space = TRUE)$method |>
    testthat::expect_equal("loglinear")
})

testthat::test_that("interp_vec applies rule outside the anchor range", {
  outside <- c(1990, 2020)

  # rule = 1 (default): no value outside the anchor range.
  ruled_na <- whep::interp_vec(
    c(2000, 2010),
    c(1, 1024),
    xout = outside,
    log_space = TRUE
  )
  testthat::expect_equal(ruled_na$y, c(NA_real_, NA_real_))
  testthat::expect_equal(ruled_na$method, c(NA_character_, NA_character_))

  # rule = 2: carry the nearest anchor, never extrapolate in log space.
  ruled_carry <- whep::interp_vec(
    c(2000, 2010),
    c(1, 1024),
    xout = outside,
    log_space = TRUE,
    rule = 2
  )
  testthat::expect_equal(ruled_carry$y, c(1, 1024))
  testthat::expect_equal(ruled_carry$method, c("linear", "linear"))

  # A missing output position yields a missing value, not an error.
  missing_xout <- whep::interp_vec(
    c(2000, 2010),
    c(1, 1024),
    xout = c(NA, 2005),
    log_space = TRUE
  )
  testthat::expect_equal(missing_xout$y, c(NA_real_, 32))
  testthat::expect_equal(missing_xout$method, c(NA_character_, "loglinear"))
})

testthat::test_that("interp_vec rejects invalid arguments", {
  testthat::expect_error(
    whep::interp_vec(c(2000, 2010), 1, xout = 2005),
    "same length"
  )
  testthat::expect_error(
    whep::interp_vec(c(2000, 2010), c(1, 2), xout = 2005, log_space = "yes"),
    "log_space"
  )
  testthat::expect_error(
    whep::interp_vec(c(2000, 2010), c(1, 2), xout = 2005, rule = 3),
    "rule"
  )
})

# Weighted proxy-growth specs ------------------------------------------------
#
# Ported from #324 (PR #606), whose fix for the weighted branch landed via
# #171 (PR #617) as a character-identical `.fg_weighted_growth()`. These four
# cases exercise it independently of the tests that came with that PR, and
# cover the ungrouped `"var:[weight]"` form which they do not.

weighted_proxy_fixture <- function() {
  tibble::tribble(
    ~region, ~country, ~year, ~gdp, ~pop, ~value,
    "eu",    "ESP",     2000,  100,   10,     50,
    "eu",    "ESP",     2001,  110,   10,     NA,
    "eu",    "ESP",     2002,  121,   10,     NA,
    "eu",    "FRA",     2000,  200,   90,     80,
    "eu",    "FRA",     2001,  200,   90,     NA,
    "eu",    "FRA",     2002,  200,   90,     NA,
    "am",    "USA",     2000,  300,   50,     30,
    "am",    "USA",     2001,  360,   50,     NA,
    "am",    "USA",     2002,  432,   50,     NA
  )
}

test_that("fill_proxy_growth accepts a weighted proxy spec", {
  # Regression: the weighted branch of .fg_growth_aggregate() renamed the
  # aggregated columns from "V1"/"V2", but data.table names them after the
  # symbols in the `list()`, so every weighted spec aborted with
  # "Items of 'old' not found in column names: [V1, V2]".
  data <- weighted_proxy_fixture()

  expect_no_error(
    whep::fill_proxy_growth(
      data,
      value_col = value,
      proxy_col = "gdp[pop]",
      .by = "country",
      verbose = FALSE
    )
  )
  expect_no_error(
    whep::fill_proxy_growth(
      data,
      value_col = value,
      proxy_col = "gdp:region[pop]",
      .by = "country",
      verbose = FALSE
    )
  )

  # "gdp[pop]" aggregates within `.by`, so each group holds a single member and
  # the weight cannot change the result: it must match the unweighted spec.
  weighted <- whep::fill_proxy_growth(
    data,
    value_col = value,
    proxy_col = "gdp[pop]",
    .by = "country",
    verbose = FALSE
  )
  unweighted <- whep::fill_proxy_growth(
    data,
    value_col = value,
    proxy_col = "gdp",
    .by = "country",
    verbose = FALSE
  )

  expect_equal(weighted$value, unweighted$value, tolerance = 1e-9)
})

test_that("fill_proxy_growth weights grouped growth by own-series weight", {
  # Regression: the weight lag was taken after the rows without an individual
  # growth had been dropped, and grouped by the coarse aggregation group. The
  # first member of each group therefore lost its weight entirely and the next
  # member picked up the previous member's weight.
  result <- whep::fill_proxy_growth(
    weighted_proxy_fixture(),
    value_col = value,
    proxy_col = "gdp:region[pop]",
    .by = "country",
    verbose = FALSE
  )

  # Weighted region growth, weights being the previous year's `pop`:
  #   eu = (0.10 * 10 + 0.00 * 90) / 100 = 0.01 in both 2001 and 2002,
  #   am = 0.20 in both years (USA is alone in its region).
  expect_equal(
    result |>
      dplyr::filter(country == "ESP") |>
      dplyr::arrange(year) |>
      dplyr::pull(value),
    c(50, 50 * 1.01, 50 * 1.01^2),
    tolerance = 1e-9
  )
  expect_equal(
    result |>
      dplyr::filter(country == "FRA") |>
      dplyr::arrange(year) |>
      dplyr::pull(value),
    c(80, 80 * 1.01, 80 * 1.01^2),
    tolerance = 1e-9
  )
  expect_equal(
    result |>
      dplyr::filter(country == "USA") |>
      dplyr::arrange(year) |>
      dplyr::pull(value),
    c(30, 36, 43.2),
    tolerance = 1e-9
  )

  # Under the bug ESP's 2001 weight was NA, so its 0.10 growth dropped out of
  # the region mean and eu grew by FRA's 0.00 alone, leaving 2001 at 50.
  expect_gt(
    result |>
      dplyr::filter(country == "ESP", year == 2001) |>
      dplyr::pull(value),
    50
  )
})

test_that("fill_proxy_growth weights an ungrouped proxy over all series", {
  # The "var:[weight]" form has no grouping columns, so the weighted mean runs
  # over every series in the year and must differ from the region-grouped one.
  result <- whep::fill_proxy_growth(
    weighted_proxy_fixture(),
    value_col = value,
    proxy_col = "gdp:[pop]",
    .by = "country",
    verbose = FALSE
  )

  # (0.10 * 10 + 0.00 * 90 + 0.20 * 50) / 150 = 11 / 150, in both years.
  global_growth <- 1 + 11 / 150
  expect_equal(
    result |>
      dplyr::filter(country == "ESP") |>
      dplyr::arrange(year) |>
      dplyr::pull(value),
    c(50, 50 * global_growth, 50 * global_growth^2),
    tolerance = 1e-9
  )
  expect_equal(
    result |>
      dplyr::filter(country == "USA") |>
      dplyr::arrange(year) |>
      dplyr::pull(value),
    c(30, 30 * global_growth, 30 * global_growth^2),
    tolerance = 1e-9
  )
})

test_that("fill_proxy_growth takes the weight from the previous year", {
  # ESP has no 2002 `gdp`, so it contributes a growth rate in 2001 and 2004
  # only. Lagging the weight after the intervening rows were dropped handed
  # 2004 the 2001 weight instead of the 2003 one.
  data <- tibble::tribble(
    ~region, ~country, ~year, ~gdp, ~pop, ~value,
    "eu",    "ESP",     2000,  100,    1,     50,
    "eu",    "ESP",     2001,  110,    2,     NA,
    "eu",    "ESP",     2002,   NA,    3,     NA,
    "eu",    "ESP",     2003,  130,    4,     NA,
    "eu",    "ESP",     2004,  143,    5,     NA,
    "eu",    "FRA",     2000,  100,   10,     80,
    "eu",    "FRA",     2001,  100,   10,     NA,
    "eu",    "FRA",     2002,  100,   10,     NA,
    "eu",    "FRA",     2003,  100,   10,     NA,
    "eu",    "FRA",     2004,  100,   10,     NA
  )

  esp <- whep::fill_proxy_growth(
    data,
    value_col = value,
    proxy_col = "gdp:region[pop]",
    .by = "country",
    verbose = FALSE
  ) |>
    dplyr::filter(country == "ESP") |>
    dplyr::arrange(year) |>
    dplyr::pull(value)

  # 2001 growth = (0.10 * 1 + 0.00 * 10) / 11; 2002 and 2003 have FRA only, so
  # they grow by 0; 2004 growth = (0.10 * 4 + 0.00 * 10) / 14.
  step_2001 <- 1 + 0.1 / 11
  step_2004 <- 1 + 0.4 / 14
  expect_equal(
    esp,
    50 * c(1, step_2001, step_2001, step_2001, step_2001 * step_2004),
    tolerance = 1e-9
  )
})
