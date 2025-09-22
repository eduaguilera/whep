# Tests for linear_fill function
testthat::test_that("linear_fill works with default method (fill_everything)", {
  test_data <- tibble::tibble(
    category = c("a", "a", "a", "a", "a", "a", "b", "b", "b", "b", "b", "b"),
    year = c(
      2015,
      2016,
      2017,
      2018,
      2019,
      2020,
      2015,
      2016,
      2017,
      2018,
      2019,
      2020
    ),
    value = c(NA, 3, NA, NA, 0, NA, 1, NA, NA, NA, 5, NA),
    change_variable = c(1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0)
  )

  result <- linear_fill(test_data, value, year, category)
  # Check that function returns a data frame
  testthat::expect_s3_class(result, "data.frame")
  # Check that no NAs remain in the value column
  testthat::expect_false(any(is.na(result$value)))
  # Check that source column is created
  testthat::expect_true("source_value" %in% names(result))
  # Check that original values are preserved
  testthat::expect_equal(
    result$value[result$source_value == "Original"],
    c(3, 0, 1, 5)
  )

  # Check that result is ungrouped
  testthat::expect_false(dplyr::is.grouped_df(result))
})

testthat::test_that("linear_fill interpolate = TRUE only interpolates", {
  test_data <- tibble::tibble(
    category = c("a", "a", "a", "a", "a", "a", "b", "b", "b", "b", "b", "b"),
    year = c(
      2015,
      2016,
      2017,
      2018,
      2019,
      2020,
      2015,
      2016,
      2017,
      2018,
      2019,
      2020
    ),
    value = c(NA, 3, NA, NA, 0, NA, 1, NA, NA, NA, 5, NA),
    proxy_variable = c(1, 2, 2, 2, 2, 2, 1, 2, 3, 4, 5, 6),
    change_variable = c(1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0)
  )

  result <- linear_fill(
    test_data,
    value,
    year,
    category,
    interpolate = TRUE,
    fill_forward = FALSE,
    fill_backward = FALSE
  )

  # Check that some NAs remain (no carrying forward/backward)
  testthat::expect_true(any(is.na(result$value)))
  # Check for interpolation in category "a" between years 2016 and 2019
  category_a <- result[result$category == "a", ]
  interpolated_values <- category_a$value[
    category_a$source_value == "Linear interpolation"
  ]
  testthat::expect_true(length(interpolated_values) > 0)
})

testthat::test_that("linear_fill interpolate + fill_backward interpolates and carries backward", {
  test_data <- tibble::tibble(
    category = c("a", "a", "a", "a", "a", "a", "b", "b", "b", "b", "b", "b"),
    year = c(
      2015,
      2016,
      2017,
      2018,
      2019,
      2020,
      2015,
      2016,
      2017,
      2018,
      2019,
      2020
    ),
    value = c(NA, 3, NA, NA, 0, NA, 1, NA, NA, NA, 5, NA),
    proxy_variable = c(1, 2, 2, 2, 2, 2, 1, 2, 3, 4, 5, 6),
    change_variable = c(1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0)
  )

  result <- linear_fill(
    test_data,
    value,
    year,
    category,
    interpolate = TRUE,
    fill_forward = FALSE,
    fill_backward = TRUE
  )

  # Should have both interpolation and carrying backward
  testthat::expect_true("Linear interpolation" %in% result$source_value)
  testthat::expect_true(
    "First value carried backwards" %in% result$source_value
  )
  testthat::expect_false(
    "Last value carried forward" %in% result$source_value
  )
})

testthat::test_that("linear_fill interpolate + fill_forward interpolates and carries forward", {
  test_data <- tibble::tibble(
    category = c("a", "a", "a", "a", "a", "a", "b", "b", "b", "b", "b", "b"),
    year = c(
      2015,
      2016,
      2017,
      2018,
      2019,
      2020,
      2015,
      2016,
      2017,
      2018,
      2019,
      2020
    ),
    value = c(NA, 3, NA, NA, 0, NA, 1, NA, NA, NA, 5, NA),
    proxy_variable = c(1, 2, 2, 2, 2, 2, 1, 2, 3, 4, 5, 6),
    change_variable = c(1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0)
  )

  result <- linear_fill(
    test_data,
    value,
    year,
    category,
    interpolate = TRUE,
    fill_forward = TRUE,
    fill_backward = FALSE
  )

  # Should have both interpolation and carrying forward
  testthat::expect_true("Linear interpolation" %in% result$source_value)
  testthat::expect_true(
    "Last value carried forward" %in% result$source_value
  )
  testthat::expect_false(
    "First value carried backwards" %in% result$source_value
  )
})

testthat::test_that("linear_fill carry only (no interpolation)", {
  test_data <- tibble::tibble(
    category = c("a", "a", "a", "a", "a", "a", "b", "b", "b", "b", "b", "b"),
    year = c(
      2015,
      2016,
      2017,
      2018,
      2019,
      2020,
      2015,
      2016,
      2017,
      2018,
      2019,
      2020
    ),
    value = c(NA, 3, NA, NA, 0, NA, 1, NA, NA, NA, 5, NA),
    proxy_variable = c(1, 2, 2, 2, 2, 2, 1, 2, 3, 4, 5, 6),
    change_variable = c(1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0)
  )
  result <- linear_fill(
    test_data,
    value,
    year,
    category,
    interpolate = FALSE,
    fill_forward = TRUE,
    fill_backward = TRUE
  )
  # Should only have carrying methods, no interpolation
  testthat::expect_false("Linear interpolation" %in% result$source_value)
  testthat::expect_true(any(
    c("Last value carried forward", "First value carried backwards") %in%
      result$source_value
  ))
})

testthat::test_that("linear_fill validates method parameter", {
  test_data <- tibble::tibble(
    category = c("a", "a", "a", "a", "a", "a", "b", "b", "b", "b", "b", "b"),
    year = c(
      2015,
      2016,
      2017,
      2018,
      2019,
      2020,
      2015,
      2016,
      2017,
      2018,
      2019,
      2020
    ),
    value = c(NA, 3, NA, NA, 0, NA, 1, NA, NA, NA, 5, NA),
    proxy_variable = c(1, 2, 2, 2, 2, 2, 1, 2, 3, 4, 5, 6),
    change_variable = c(1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0)
  )
  # Test function works with all FALSE parameters
  result_none <- linear_fill(
    test_data,
    value,
    year,
    category,
    interpolate = FALSE,
    fill_forward = FALSE,
    fill_backward = FALSE
  )
  testthat::expect_true(all(is.na(result_none$value[is.na(test_data$value)])))
})

testthat::test_that("linear_fill works without grouping variables", {
  simple_data <- tibble::tibble(
    year = c(2015, 2016, 2017, 2018, 2019),
    value = c(1, NA, NA, NA, 5)
  )
  result <- linear_fill(simple_data, value, year)
  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_false(any(is.na(result$value)))
  testthat::expect_true("source_value" %in% names(result))
})

# Tests for linear_fill value correctness
testthat::test_that("linear_fill interpolation produces correct values", {
  # Simple case with known interpolation values
  simple_data <- tibble::tibble(
    year = c(2015, 2016, 2017, 2018, 2019),
    value = c(10, NA, NA, NA, 20)
  )
  result <- linear_fill(
    simple_data,
    value,
    year,
    interpolate = TRUE,
    fill_forward = FALSE,
    fill_backward = FALSE
  )
  # Check interpolated values are correct
  # Should be 10, 12.5, 15, 17.5, 20
  # Original
  testthat::expect_equal(result$value[1], 10)
  # Should be: 10 + (20-10) * 1/4
  testthat::expect_equal(result$value[2], 12.5)
  # Should be: 10 + (20-10) * 2/4
  testthat::expect_equal(result$value[3], 15)
  # Should be: 10 + (20-10) * 3/4
  testthat::expect_equal(result$value[4], 17.5)
  # Original
  testthat::expect_equal(result$value[5], 20)
  # Check source labels
  testthat::expect_equal(result$source_value[1], "Original")
  testthat::expect_equal(result$source_value[2], "Linear interpolation")
  testthat::expect_equal(result$source_value[3], "Linear interpolation")
  testthat::expect_equal(result$source_value[4], "Linear interpolation")
  testthat::expect_equal(result$source_value[5], "Original")
})

testthat::test_that("linear_fill carry forward produces correct values", {
  # Test carrying forward from last value
  carry_data <- tibble::tibble(
    year = c(2015, 2016, 2017, 2018),
    value = c(NA, 15, NA, NA)
  )
  result <- linear_fill(
    carry_data,
    value,
    year,
    interpolate = TRUE,
    fill_forward = TRUE,
    fill_backward = FALSE
  )
  # All NA values after 15 should be filled with 15 (last value carried forward)
  testthat::expect_equal(result$value, c(NA, 15, 15, 15))

  testthat::expect_equal(
    result$source_value[3],
    "Last value carried forward"
  )
  testthat::expect_equal(
    result$source_value[4],
    "Last value carried forward"
  )
  testthat::expect_equal(result$source_value[2], "Original")
})

testthat::test_that("linear_fill carry backward produces correct values", {
  # Test carrying backward (first observed value carried backward)
  carry_data <- tibble::tibble(
    year = c(2015, 2016, 2017, 2018),
    value = c(NA, NA, NA, 25)
  )
  result <- linear_fill(
    carry_data,
    value,
    year,
    interpolate = TRUE,
    fill_forward = FALSE,
    fill_backward = TRUE
  )
  # All NA values should be filled with 25 (first observed value carried forward)
  testthat::expect_equal(result$value, c(25, 25, 25, 25))
  testthat::expect_equal(
    result$source_value[1],
    "First value carried backwards"
  )
  testthat::expect_equal(
    result$source_value[2],
    "First value carried backwards"
  )
  testthat::expect_equal(
    result$source_value[3],
    "First value carried backwards"
  )
  testthat::expect_equal(result$source_value[4], "Original")
})

testthat::test_that("linear_fill carry forwards from first value produces correct values", {
  # Test carrying forwards from first value
  carry_data <- tibble::tibble(
    year = c(2015, 2016, 2017, 2018),
    value = c(30, NA, NA, NA)
  )
  result <- linear_fill(carry_data, value, year, method = "carry_only")
  # All NA values should be filled with 30 (first value carried forwards)
  testthat::expect_equal(result$value, c(30, 30, 30, 30))
  testthat::expect_equal(result$source_value[1], "Original")
  testthat::expect_equal(result$source_value[2], "Last value carried forward")
  testthat::expect_equal(result$source_value[3], "Last value carried forward")
  testthat::expect_equal(result$source_value[4], "Last value carried forward")
})

testthat::test_that("linear_fill carry backwards from last value produces correct values", {
  # Test carrying backwards from last value
  carry_data <- tibble::tibble(
    year = c(2015, 2016, 2017, 2018),
    value = c(NA, NA, NA, 50)
  )
  result <- linear_fill(carry_data, value, year, method = "carry_only")
  # All NA values should be filled with 50 (last value carried backwards)
  testthat::expect_equal(result$value, c(50, 50, 50, 50))
  testthat::expect_equal(
    result$source_value[1],
    "First value carried backwards"
  )
  testthat::expect_equal(
    result$source_value[2],
    "First value carried backwards"
  )
  testthat::expect_equal(
    result$source_value[3],
    "First value carried backwards"
  )
  testthat::expect_equal(result$source_value[4], "Original")
})

testthat::test_that("linear_fill handles complex interpolation with groups", {
  # Test interpolation with multiple groups
  grouped_data <- tibble::tibble(
    group = c("A", "A", "A", "A", "B", "B", "B", "B"),
    year = c(2015, 2016, 2017, 2018, 2015, 2016, 2017, 2018),
    value = c(0, NA, NA, 12, 20, NA, NA, 40)
  )

  result <- linear_fill(
    grouped_data,
    value,
    year,
    group,
    method = "interpolate"
  )

  # Check Group A: interpolation from 0 to 12 over 3 intervals
  group_a <- result[result$group == "A", ]
  # Original
  testthat::expect_equal(group_a$value[1], 0)
  # Should be: 0 + (12-0) * 1/3
  testthat::expect_equal(group_a$value[2], 4)
  # Should be: 0 + (12-0) * 2/3
  testthat::expect_equal(group_a$value[3], 8)
  # Original
  testthat::expect_equal(group_a$value[4], 12)
  # Check Group B: interpolation from 20 to 40 over 3 intervals
  group_b <- result[result$group == "B", ]
  # Original
  testthat::expect_equal(group_b$value[1], 20)
  # Should be: 20 + (40-20) * 1/3
  testthat::expect_equal(group_b$value[2], 26.666667, tolerance = 1e-6)
  # Should be: 20 + (40-20) * 2/3
  testthat::expect_equal(group_b$value[3], 33.333333, tolerance = 1e-6)
  # Original
  testthat::expect_equal(group_b$value[4], 40)
})

testthat::test_that("linear_fill handles non-uniform time intervals", {
  # Test with irregular year spacing
  irregular_data <- tibble::tibble(
    year = c(2015, 2018, 2020, 2025),
    value = c(100, NA, NA, 200)
  )
  result <- linear_fill(
    irregular_data,
    value,
    year,
    interpolate = TRUE,
    fill_forward = FALSE,
    fill_backward = FALSE
  )
  # Interpolation should be based on actual year differences
  # 2015 to 2025 = 10 years total
  # 2018 is 3 years from 2015, so should be 100 + (200-100) * 3/10 = 130
  # 2020 is 5 years from 2015, so should be 100 + (200-100) * 5/10 = 150
  # Original
  testthat::expect_equal(result$value[1], 100)
  # Interpolated
  testthat::expect_equal(result$value[2], 130)
  # Interpolated
  testthat::expect_equal(result$value[3], 150)
  # Original
  testthat::expect_equal(result$value[4], 200)
})

testthat::test_that("linear_fill handles edge cases with single values", {
  # Test with only one non-NA value
  single_value_data <- tibble::tibble(
    year = c(2015, 2016, 2017),
    value = c(NA, 42, NA)
  )

  result <- linear_fill(
    single_value_data,
    value,
    year,
    method = "fill_everything"
  )

  # Both NAs should be filled with 42 (carried in both directions)
  testthat::expect_equal(result$value, c(42, 42, 42))
  testthat::expect_equal(
    result$source_value[1],
    "First value carried backwards"
  )
  testthat::expect_equal(result$source_value[2], "Original")
  testthat::expect_equal(result$source_value[3], "Last value carried forward")
})

# Tests for proxy_fill function
testthat::test_that("proxy_fill works correctly", {
  test_data <- tibble::tibble(
    category = c("a", "a", "a", "a", "a", "a", "b", "b", "b", "b", "b", "b"),
    year = c(
      2015,
      2016,
      2017,
      2018,
      2019,
      2020,
      2015,
      2016,
      2017,
      2018,
      2019,
      2020
    ),
    value = c(NA, 3, NA, NA, 0, NA, 1, NA, NA, NA, 5, NA),
    proxy_variable = c(1, 2, 2, 2, 2, 2, 1, 2, 3, 4, 5, 6),
    change_variable = c(1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0)
  )
  result <- proxy_fill(test_data, value, proxy_variable, year, category)
  # Check that function returns a data frame
  testthat::expect_s3_class(result, "data.frame")
  # Check that proxy_ratio column is created
  testthat::expect_true("proxy_ratio" %in% names(result))

  # Check that source column is created for the main variable
  testthat::expect_true("source_value" %in% names(result))

  # Check that original values are preserved
  original_rows <- result[result$source_value == "Original", ]
  testthat::expect_equal(original_rows$value, c(3, 0, 1, 5))

  # Check that proxy-based source labels exist
  proxy_sources <- c(
    "Proxy interpolated",
    "Proxy carried forward",
    "Proxy carried backwards"
  )
  testthat::expect_true(any(proxy_sources %in% result$source_value))

  # Check that result is ungrouped
  testthat::expect_false(dplyr::is.grouped_df(result))
})

testthat::test_that("proxy_fill calculates proxy ratios correctly", {
  simple_data <- tibble::tibble(
    year = c(2015, 2016, 2017),
    value = c(10, NA, 30),
    proxy_variable = c(5, 10, 15)
  )
  result <- proxy_fill(simple_data, value, proxy_variable, year)
  # Check that proxy ratios are calculated correctly for original values
  # Should be: 10/5
  testthat::expect_equal(result$proxy_ratio[1], 2)
  # Should be: 30/15
  testthat::expect_equal(result$proxy_ratio[3], 2)
})

testthat::test_that("proxy_fill works without grouping variables", {
  simple_data <- tibble::tibble(
    year = c(2015, 2016, 2017),
    value = c(10, NA, 30),
    proxy_variable = c(5, 10, 15)
  )
  result <- proxy_fill(simple_data, value, proxy_variable, year)

  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_true("proxy_ratio" %in% names(result))
  testthat::expect_true("source_value" %in% names(result))
})

# Tests for sum_fill function
testthat::test_that("sum_fill works correctly", {
  test_data <- tibble::tibble(
    category = c("a", "a", "a", "a", "a", "a", "b", "b", "b", "b", "b", "b"),
    year = c(
      2015,
      2016,
      2017,
      2018,
      2019,
      2020,
      2015,
      2016,
      2017,
      2018,
      2019,
      2020
    ),
    value = c(NA, 3, NA, NA, 0, NA, 1, NA, NA, NA, 5, NA),
    proxy_variable = c(1, 2, 2, 2, 2, 2, 1, 2, 3, 4, 5, 6),
    change_variable = c(1, 2, 3, 4, 1, 1, 0, 0, 0, 0, 0, 1)
  )
  result <- sum_fill(test_data, value, change_variable, FALSE, category)
  # Check that function returns a data frame
  testthat::expect_s3_class(result, "data.frame")
  # Check that Source column is created
  testthat::expect_true("source_value" %in% names(result))

  # Check that original values are preserved
  original_rows <- result[
    result$source_value == "Original" & !is.na(result$source_value),
  ]
  testthat::expect_equal(original_rows$value, c(3, 0, 1, 5))

  # Check that result is ungrouped
  testthat::expect_false(dplyr::is.grouped_df(result))
})

testthat::test_that("sum_fill fills gaps by summing with change variable", {
  simple_data <- tibble::tibble(
    year = c(2015, 2016, 2017, 2018),
    value = c(10, NA, NA, NA),
    change_variable = c(0, 2, 3, 1)
  )
  result <- sum_fill(simple_data, value, change_variable)
  # Check the cumulative sum behavior
  # Original value
  testthat::expect_equal(result$value[1], 10)
  # Should be: 10 + 2
  testthat::expect_equal(result$value[2], 12)
  # Should be: 12 + 3
  testthat::expect_equal(result$value[3], 15)
  # Should be: 15 + 1
  testthat::expect_equal(result$value[4], 16)

  # Check source labels
  testthat::expect_equal(result$source_value[1], "Original")
  testthat::expect_equal(result$source_value[2], "Filled with sum")
  testthat::expect_equal(result$source_value[3], "Filled with sum")
  testthat::expect_equal(result$source_value[4], "Filled with sum")
})

testthat::test_that("sum_fill handles multiple groups correctly", {
  grouped_data <- tibble::tibble(
    group = c("A", "A", "A", "B", "B", "B"),
    year = c(2015, 2016, 2017, 2015, 2016, 2017),
    value = c(10, NA, NA, 20, NA, NA),
    change_variable = c(0, 1, 2, 0, 3, 4)
  )
  result <- sum_fill(grouped_data, value, change_variable, FALSE, group)
  # Check group A
  group_a <- result[result$group == "A", ]
  testthat::expect_equal(group_a$value, c(10, 11, 13))
  # Check group B
  group_b <- result[result$group == "B", ]
  testthat::expect_equal(group_b$value, c(20, 23, 27))
})

testthat::test_that("sum_fill works without grouping variables", {
  simple_data <- tibble::tibble(
    value = c(5, NA, NA),
    change_variable = c(0, 2, 3)
  )
  result <- sum_fill(simple_data, value, change_variable)

  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_equal(result$value, c(5, 7, 10))
  testthat::expect_true("source_value" %in% names(result))
})

testthat::test_that("sum_fill handles edge cases", {
  # Test with all NAs except first value
  all_na_data <- tibble::tibble(
    value = c(10, NA, NA, NA),
    change_variable = c(1, 2, 3, 4)
  )
  result <- sum_fill(all_na_data, value, change_variable)
  testthat::expect_equal(result$value, c(10, 12, 15, 19))
  # Test with no NAs
  no_na_data <- tibble::tibble(
    value = c(1, 2, 3, 4),
    change_variable = c(1, 1, 1, 1)
  )
  result <- sum_fill(no_na_data, value, change_variable)
  # Should remain unchanged
  testthat::expect_equal(result$value, c(1, 2, 3, 4))
  testthat::expect_true(all(result$source_value == "Original"))
})

testthat::test_that("sum_fill handles starting NA with start_with_zero = FALSE (default)", {
  start_na_data <- tibble::tibble(
    value = c(NA, NA, NA, NA),
    change_variable = c(1, 2, 3, 4)
  )
  result <- sum_fill(start_na_data, value, change_variable)
  # With default behavior, starting NA should remain unfilled
  testthat::expect_true(is.na(result$value[1]))
  testthat::expect_true(is.na(result$source_value[1]))
  # All subsequent values should also remain NA since no starting point
  testthat::expect_true(all(is.na(result$value)))
})

testthat::test_that("sum_fill handles starting NA with start_with_zero = TRUE", {
  start_na_data <- tibble::tibble(
    value = c(NA, NA, NA, NA),
    change_variable = c(1, 2, 3, 4)
  )
  result <- sum_fill(start_na_data, value, change_variable, TRUE)
  # With start_with_zero = TRUE, assumes invisible 0 before first observation
  # First value should be the first change_var value (1)
  testthat::expect_equal(result$value[1], 1)
  # Should be: 1 + 2 = 3
  testthat::expect_equal(result$value[2], 3)
  # Should be: 3 + 3 = 6
  testthat::expect_equal(result$value[3], 6)
  # Should be: 6 + 4 = 10
  testthat::expect_equal(result$value[4], 10)
  # Check source labels - all should be "Filled with sum"
  testthat::expect_equal(result$source_value[1], "Filled with sum")
  testthat::expect_equal(result$source_value[2], "Filled with sum")
  testthat::expect_equal(result$source_value[3], "Filled with sum")
  testthat::expect_equal(result$source_value[4], "Filled with sum")
})

testthat::test_that("sum_fill with start_with_zero works with groups", {
  grouped_start_na_data <- tibble::tibble(
    group = c("A", "A", "A", "B", "B", "B"),
    value = c(NA, NA, NA, 5, NA, NA),
    change_variable = c(1, 2, 3, 0, 2, 4)
  )
  result <- sum_fill(grouped_start_na_data, value, change_variable, TRUE, group)
  # Check group A (starts with NA, should be filled with cumsum starting from change_var)
  group_a <- result[result$group == "A", ]
  testthat::expect_equal(group_a$value, c(1, 3, 6)) # 1, 1+2, 3+3
  testthat::expect_equal(group_a$source_value[1], "Filled with sum")
  # Check group B (starts with 5, should remain original)
  group_b <- result[result$group == "B", ]
  testthat::expect_equal(group_b$value, c(5, 7, 11))
  testthat::expect_equal(group_b$source_value[1], "Original")
})

# Integration tests
testthat::test_that("All functions work together in a pipeline", {
  test_data <- tibble::tibble(
    category = c("a", "a", "a", "a", "a", "a", "b", "b", "b", "b", "b", "b"),
    year = c(
      2015,
      2016,
      2017,
      2018,
      2019,
      2020,
      2015,
      2016,
      2017,
      2018,
      2019,
      2020
    ),
    value = c(NA, 3, NA, NA, 0, NA, 1, NA, NA, NA, 5, NA),
    proxy_variable = c(1, 2, 2, 2, 2, 2, 1, 2, 3, 4, 5, 6),
    change_variable = c(1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0)
  )

  # Test that functions can be chained together
  result1 <- linear_fill(
    test_data,
    value,
    year,
    category,
    method = "interpolate"
  )
  result2 <- proxy_fill(test_data, value, proxy_variable, year, category)
  result3 <- sum_fill(test_data, value, change_variable, FALSE, category)
  result4 <- sum_fill(test_data, value, change_variable, TRUE, category)

  testthat::expect_s3_class(result1, "data.frame")
  testthat::expect_s3_class(result2, "data.frame")
  testthat::expect_s3_class(result3, "data.frame")
  testthat::expect_s3_class(result4, "data.frame")
})
