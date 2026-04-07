# test_livestock_energy.R ------------------------------------------------------

# estimate_energy_demand -------------------------------------------------------

testthat::test_that("estimate_energy_demand returns expected columns", {
  result <- dairy_tier2_fixture() |>
    estimate_energy_demand()

  result |>
    pointblank::expect_col_exists(
      c(
        "ne_maintenance",
        "ne_activity",
        "ne_lactation",
        "ne_work",
        "ne_pregnancy",
        "ne_growth",
        "ne_total_maintenance",
        "ne_total_growth",
        "rem",
        "reg",
        "gross_energy"
      )
    )
})

testthat::test_that("GE for dairy cattle is in IPCC expected range", {
  result <- dairy_tier2_fixture() |>
    estimate_energy_demand()

  ge <- result |> dplyr::pull(gross_energy)
  # IPCC Table 10.10 reference range for dairy: ~250-350 MJ/day

  testthat::expect_gt(ge, 200)
  testthat::expect_lt(ge, 400)
})

testthat::test_that("GE for beef is lower than dairy", {
  dairy <- dairy_tier2_fixture() |>
    estimate_energy_demand() |>
    dplyr::pull(gross_energy)

  beef <- beef_tier2_fixture() |>
    estimate_energy_demand() |>
    dplyr::pull(gross_energy)

  testthat::expect_gt(dairy, beef)
})

testthat::test_that("default weight is used when not provided", {
  input <- tibble::tibble(
    species = "Dairy Cattle",
    cohort = "Adult Female",
    diet_quality = "High",
    heads = 100
  )
  result <- estimate_energy_demand(input)

  # Should get a GLEAM default weight, not NA
  result |>
    pointblank::expect_col_vals_not_null("weight")
})

# .calc_energy_maintenance ------------------------------------------------------

testthat::test_that("NEm scales with body weight", {
  light <- dairy_tier2_fixture() |>
    dplyr::mutate(weight = 400) |>
    estimate_energy_demand() |>
    dplyr::pull(ne_maintenance)

  heavy <- dairy_tier2_fixture() |>
    dplyr::mutate(weight = 700) |>
    estimate_energy_demand() |>
    dplyr::pull(ne_maintenance)

  testthat::expect_gt(heavy, light)
})

# .calc_energy_lactation --------------------------------------------------------

testthat::test_that("NEl is zero for non-lactating animals", {
  result <- beef_tier2_fixture() |>
    estimate_energy_demand()

  nel <- result |> dplyr::pull(ne_lactation)
  testthat::expect_equal(nel, 0)
})

testthat::test_that("NEl increases with milk yield", {
  low_milk <- dairy_tier2_fixture() |>
    dplyr::mutate(milk_yield_kg_day = 10) |>
    estimate_energy_demand() |>
    dplyr::pull(ne_lactation)

  high_milk <- dairy_tier2_fixture() |>
    dplyr::mutate(milk_yield_kg_day = 30) |>
    estimate_energy_demand() |>
    dplyr::pull(ne_lactation)

  testthat::expect_gt(high_milk, low_milk)
})

# .calc_energy_growth -----------------------------------------------------------

testthat::test_that("NEg is zero when weight_gain is zero", {
  result <- dairy_tier2_fixture() |>
    estimate_energy_demand()

  neg <- result |> dplyr::pull(ne_growth)
  testthat::expect_equal(neg, 0)
})

testthat::test_that("NEg is positive for growing animals", {
  result <- beef_tier2_fixture() |>
    estimate_energy_demand()

  neg <- result |> dplyr::pull(ne_growth)
  testthat::expect_gt(neg, 0)
})

# .calc_energy_wool -------------------------------------------------------------

testthat::test_that("NEwool is zero for non-sheep", {
  result <- dairy_tier2_fixture() |>
    estimate_energy_demand()

  newool <- result |> dplyr::pull(ne_wool)
  testthat::expect_equal(newool, 0)
})

# .estimate_gross_energy --------------------------------------------------------

testthat::test_that("REM and REG are between 0 and 1", {
  result <- dairy_tier2_fixture() |>
    estimate_energy_demand()

  rem <- result |> dplyr::pull(rem)
  reg <- result |> dplyr::pull(reg)

  testthat::expect_gt(rem, 0)
  testthat::expect_lt(rem, 1)
  testthat::expect_gt(reg, 0)
  testthat::expect_lt(reg, 1)
})

testthat::test_that("custom de_percent overrides default", {
  default <- dairy_tier2_fixture() |>
    estimate_energy_demand() |>
    dplyr::pull(gross_energy)

  custom <- dairy_tier2_fixture() |>
    dplyr::mutate(de_percent = 80) |>
    estimate_energy_demand() |>
    dplyr::pull(gross_energy)

  # Higher DE% means less GE needed for same NE
  testthat::expect_lt(custom, default)
})

testthat::test_that("Beef cattle gets default weight gain", {
  result <- tibble::tibble(
    species = "Beef Cattle",
    cohort = "Adult Male",
    weight = 500,
    diet_quality = "Medium",
    heads = 100
  ) |>
    estimate_energy_demand()

  wg <- result |> dplyr::pull(weight_gain_kg_day)
  neg <- result |> dplyr::pull(ne_growth)
  # Should receive default 0.5 kg/day from production defaults.
  testthat::expect_equal(wg, 0.5)
  testthat::expect_gt(neg, 0)
})
