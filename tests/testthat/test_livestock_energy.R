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

# .calc_energy_maintenance: species-distinct Cfi -------------------------------

testthat::test_that("Goats use IPCC Table 10.4 Cfi (0.315), distinct from sheep", {
  goat <- tibble::tibble(
    species = "Goats", cohort = "All", weight = 40,
    diet_quality = "Low", heads = 1
  ) |>
    estimate_energy_demand()
  sheep <- tibble::tibble(
    species = "Sheep", cohort = "All", weight = 40,
    diet_quality = "Low", heads = 1
  ) |>
    estimate_energy_demand()

  # NEm = Cfi * BW^0.75; at equal weight the goat/sheep ratio is the Cfi ratio.
  testthat::expect_equal(
    goat$ne_maintenance / sheep$ne_maintenance,
    0.315 / 0.217,
    tolerance = 1e-3
  )
})

# .calc_energy_activity: sheep/goats use Eq 10.5 (Ca * BW) ----------------------

testthat::test_that("sheep/goat activity uses Ca * body weight (IPCC Eq 10.5)", {
  ewe <- tibble::tibble(
    species = "Sheep", cohort = "All", weight = 50,
    diet_quality = "Low", heads = 1, grazing_distance_km = 0
  ) |>
    estimate_energy_demand()

  # ca_pasture for sheep is 0.0107 MJ day^-1 kg^-1; NEa = 0.0107 * 50 = 0.535,
  # NOT 0.0107 * NEm (~0.041). Guards against reverting to the cattle form.
  testthat::expect_equal(ewe$ne_activity, 0.0107 * 50, tolerance = 1e-6)
  testthat::expect_gt(ewe$ne_activity, ewe$ne_maintenance * 0.0107 * 2)
})

testthat::test_that("cattle activity still uses Ca * NEm (IPCC Eq 10.4)", {
  cow <- beef_tier2_fixture() |>
    dplyr::mutate(grazing_distance_km = 0) |>
    estimate_energy_demand()

  testthat::expect_equal(
    cow$ne_activity,
    cow$activity_coef * cow$ne_maintenance,
    tolerance = 1e-6
  )
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

# .calc_energy_work / work_coef override -----------------------------------

testthat::test_that("work_coef override activates NE_work for cattle (whep's own cw is 0)", {
  no_override <- working_oxen_tier2_fixture(work_coef = NA_real_) |>
    estimate_energy_demand()
  with_override <- working_oxen_tier2_fixture(work_coef = 0.10) |>
    estimate_energy_demand()

  testthat::expect_equal(no_override$ne_work, 0)
  testthat::expect_gt(with_override$ne_work, 0)
  testthat::expect_gt(with_override$gross_energy, no_override$gross_energy)
})

testthat::test_that("work_coef is NA-safe and does not affect rows without it", {
  no_col <- tibble::tibble(
    species = "Beef Cattle",
    cohort = "Adult Male",
    weight = 420,
    work_hours_day = 6,
    diet_quality = "Medium",
    heads = 10
  ) |>
    estimate_energy_demand()
  testthat::expect_equal(no_col$ne_work, 0)
})

# .calc_energy_maintenance / cfi override ----------------------------------

testthat::test_that("cfi override lowers NEm and GE for a housed dairy herd", {
  default <- dairy_tier2_fixture() |>
    estimate_energy_demand()
  # 0.332 is the Zootecnicas/NIR housed-dairy maintenance coefficient, below
  # whep's generic dairy default (0.386).
  override <- dairy_tier2_fixture() |>
    dplyr::mutate(cfi = 0.332) |>
    estimate_energy_demand()

  testthat::expect_lt(override$ne_maintenance, default$ne_maintenance)
  testthat::expect_lt(override$gross_energy, default$gross_energy)
  # NEm = cfi * BW^0.75 * (1 + temp_adj); at equal weight/temp the override/
  # default ratio is exactly the Cfi ratio.
  testthat::expect_equal(
    override$ne_maintenance / default$ne_maintenance,
    0.332 / 0.386,
    tolerance = 1e-3
  )
})

testthat::test_that("cfi is NA-safe and does not affect rows without it", {
  with_na <- dairy_tier2_fixture() |>
    dplyr::mutate(cfi = NA_real_) |>
    estimate_energy_demand()
  no_col <- dairy_tier2_fixture() |>
    estimate_energy_demand()
  testthat::expect_equal(with_na$ne_maintenance, no_col$ne_maintenance)
})
