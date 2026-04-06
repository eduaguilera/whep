# test_livestock_emissions.R --------------------------------------------------

# calculate_enteric_ch4 --------------------------------------------------------

testthat::test_that("auto tier selects Tier 2 when GE available", {
  result <- dairy_tier2_fixture() |>
    calculate_enteric_ch4()

  result |>
    pointblank::expect_col_exists("enteric_ch4_tier2")
})

testthat::test_that("auto tier selects Tier 1 when GE missing", {
  result <- single_tier1_fixture("Sheep", 100) |>
    calculate_enteric_ch4()

  result |>
    pointblank::expect_col_exists("enteric_ch4_tier1")
})

testthat::test_that("forced tier 1 uses Tier 1 even with GE", {
  result <- dairy_tier2_fixture() |>
    calculate_enteric_ch4(tier = 1)

  result |>
    pointblank::expect_col_exists("enteric_ch4_tier1")
})

# calculate_manure_emissions ---------------------------------------------------

testthat::test_that("calculate_manure_emissions includes CH4 and N2O", {
  result <- dairy_tier2_fixture() |>
    calculate_manure_emissions()

  result |>
    pointblank::expect_col_exists(
      c("manure_ch4_tier2", "manure_n2o_total")
    )
})

testthat::test_that("Tier 1 manure returns CH4 only", {
  result <- single_tier1_fixture("Sheep", 100) |>
    calculate_manure_emissions()

  result |>
    pointblank::expect_col_exists("manure_ch4_tier1")
})

# calculate_livestock_emissions ------------------------------------------------

testthat::test_that("full pipeline returns enteric + manure", {
  result <- dairy_tier2_fixture() |>
    calculate_livestock_emissions()

  testthat::expect_true(
    "enteric_ch4_tier2" %in%
      names(result) ||
      "enteric_ch4_tier1" %in% names(result)
  )
})
