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

testthat::test_that("dairy commodity routes only to the Dairy system", {
  # Regression for #109: the cohort split must follow the commodity, not just
  # the general species. A "Cattle, dairy" herd is entirely dairy-system cohorts.
  result <- tibble::tibble(species = "Cattle, dairy", heads = 1000) |>
    whep::calculate_cohorts_systems()

  testthat::expect_setequal(unique(result$system), "Dairy")
  testthat::expect_equal(sum(result$cohort_heads), 1000, tolerance = 1)
})

testthat::test_that("non-dairy commodity routes only to the Beef system", {
  result <- tibble::tibble(species = "Cattle, non-dairy", heads = 1000) |>
    whep::calculate_cohorts_systems()

  testthat::expect_setequal(unique(result$system), "Beef")
  testthat::expect_equal(sum(result$cohort_heads), 1000, tolerance = 1)
})

testthat::test_that("single-commodity species keep the generic system blend", {
  # "Buffalo" is one commodity for all buffalo, so it must not collapse to a
  # single system the way the cattle dairy/non-dairy commodities do.
  result <- tibble::tibble(species = "Buffalo", heads = 1000) |>
    whep::calculate_cohorts_systems()

  testthat::expect_setequal(unique(result$system), c("Dairy", "Other"))
  testthat::expect_equal(sum(result$cohort_heads), 1000, tolerance = 1)
})

testthat::test_that("supplied system_shares bypass commodity routing", {
  custom <- tibble::tribble(
    ~species_gen, ~system, ~system_share,
    "Cattle", "Dairy", 0.5,
    "Cattle", "Beef", 0.5
  )
  result <- tibble::tibble(species = "Cattle, dairy", heads = 1000) |>
    whep::calculate_cohorts_systems(system_shares = custom)

  testthat::expect_setequal(unique(result$system), c("Dairy", "Beef"))
  testthat::expect_equal(sum(result$cohort_heads), 1000, tolerance = 1)
})
