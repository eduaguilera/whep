# test_livestock_cohorts.R ------------------------------------------------------

testthat::test_that("calculate_cohorts_systems expands rows", {
  input <- tibble::tibble(
    species = "Dairy Cattle",
    heads = 1000
  )
  result <- calculate_cohorts_systems(input)

  # Should have more rows than input (expanded by systems/cohorts)
  testthat::expect_gt(nrow(result), nrow(input))
})

testthat::test_that("cohort heads sum to original heads", {
  input <- tibble::tibble(
    species = "Dairy Cattle",
    heads = 1000
  )
  result <- calculate_cohorts_systems(input)

  total_heads <- sum(result$cohort_heads, na.rm = TRUE)
  testthat::expect_equal(total_heads, 1000, tolerance = 1)
})

testthat::test_that("result has required columns", {
  input <- tibble::tibble(
    species = "Sheep",
    heads = 500
  )
  result <- calculate_cohorts_systems(input)

  result |>
    pointblank::expect_col_exists(
      c("system", "cohort", "cohort_heads")
    )
})
