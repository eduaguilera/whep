# test_livestock_uncertainty.R --------------------------------------------------

testthat::test_that("uncertainty bounds returns lower and upper", {
  input <- tibble::tibble(
    species = "Dairy Cattle",
    enteric_ch4_tier2 = 12000,
    heads = 100
  )
  result <- calculate_uncertainty_bounds(input)

  result |>
    pointblank::expect_col_exists(
      c("enteric_ch4_tier2_lower", "enteric_ch4_tier2_upper")
    )
})

testthat::test_that("lower bound < central < upper bound", {
  input <- tibble::tibble(
    species = "Dairy Cattle",
    enteric_ch4_tier2 = 12000,
    heads = 100
  )
  result <- calculate_uncertainty_bounds(input)

  lower <- result |> dplyr::pull(enteric_ch4_tier2_lower)
  upper <- result |> dplyr::pull(enteric_ch4_tier2_upper)

  testthat::expect_lt(lower, 12000)
  testthat::expect_gt(upper, 12000)
})
