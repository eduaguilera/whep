.water_cohort_fixture <- function() {
  tibble::tribble(
    ~year, ~reporting_polity_code, ~item_cbs_code, ~cohort,
    ~production_system, ~animal_days, ~rate_id,
    2010L, "fixture-polity", 961L, "adult", "mixed", 100, "measured-adult",
    2010L, "fixture-polity", 961L, "calf", "mixed", 40, "measured-calf"
  ) |>
    dplyr::mutate(
      assessment_period = "synthetic annual interval",
      exposure_source = "synthetic fixture",
      exposure_basis = "assumed"
    )
}
.water_rates_fixture <- function() {
  tibble::tribble(
    ~rate_id, ~item_cbs_code, ~cohort, ~production_system,
    ~drinking_l_day, ~service_l_day, ~rate_source, ~rate_basis, ~quantity_basis,
    "measured-adult", 961L, "adult", "mixed", 30, 8, "synthetic fixture", "assumed", "withdrawal",
    "measured-calf", 961L, "calf", "mixed", 5, 1, "synthetic fixture", "assumed", "withdrawal"
  )
}

testthat::test_that("cohort exposure does not silently become adult head-years", {
  x <- .water_cohort_fixture()
  out <- whep::calculate_livestock_water(x, .water_rates_fixture())
  testthat::expect_equal(out$drinking_m3, c(3, 0.2))
  testthat::expect_equal(out$service_m3, c(0.8, 0.04))
  testthat::expect_identical(out$animal_days, x$animal_days)
  testthat::expect_identical(out$cohort, x$cohort)
  testthat::expect_true(all(is.na(out$consumption_m3)))
  testthat::expect_identical(out$rate_source, rep("synthetic fixture", 2))
})

.dairy_water_fixture <- function() {
  # Cardot et al. 2008, Tables 1 and 4, rounded study means; not raw cow data.
  tibble::tibble(
    animal_days = 1837,
    item_cbs_code = 960L,
    cohort = "lactating",
    production_system = "free_stall_tmr",
    dry_matter_intake_kg_day = 20.6,
    milk_kg_day = 26.5,
    diet_dry_matter_percent = 48.1,
    minimum_temperature_c = 0.1,
    rainfall_mm_day = 1.3
  )
}

testthat::test_that("actual published cow-day exposure has explicit alternatives", {
  x <- .dairy_water_fixture()
  table5 <- whep::estimate_dairy_drinking(x)
  abstract <- whep::estimate_dairy_drinking(x, "cardot_abstract")
  legacy <- whep::estimate_dairy_drinking(x, "legacy_adult")
  testthat::expect_equal(table5$drinking_l_day, 83.796)
  testthat::expect_equal(abstract$drinking_l_day, 83.589)
  testthat::expect_equal(legacy$drinking_l_day, 55)
  testthat::expect_equal(table5$drinking_m3, 83.796 * 1837 / 1000)
  testthat::expect_equal(table5$drinking_l_day - abstract$drinking_l_day, 0.207)
  testthat::expect_identical(table5$quantity_basis, "free_drinking_intake")
  testthat::expect_identical(legacy$quantity_basis, "requirement")
  testthat::expect_false(table5$water_extrapolated)
})

testthat::test_that("splitting exposure conserves extensive water", {
  x <- .dairy_water_fixture()
  split <- dplyr::bind_rows(x, x) |>
    dplyr::mutate(animal_days = .data$animal_days / 2)
  testthat::expect_equal(
    sum(whep::estimate_dairy_drinking(split)$drinking_m3),
    whep::estimate_dairy_drinking(x)$drinking_m3
  )
  rates <- .water_rates_fixture()
  exposure <- .water_cohort_fixture()
  split <- dplyr::bind_rows(exposure, exposure) |>
    dplyr::mutate(animal_days = .data$animal_days / 2)
  testthat::expect_equal(
    sum(whep::calculate_livestock_water(split, rates)$direct_water_m3),
    sum(whep::calculate_livestock_water(exposure, rates)$direct_water_m3)
  )
})

testthat::test_that("coverage and invalid source rows cannot cancel or vanish", {
  x <- .water_cohort_fixture()
  rates <- .water_rates_fixture()
  testthat::expect_error(
    whep::calculate_livestock_water(x, rates[1, ]),
    class = "whep_livestock_water_coverage"
  )
  testthat::expect_error(
    whep::calculate_livestock_water(x, dplyr::bind_rows(rates, rates)),
    class = "whep_livestock_water_input"
  )
  rates$cohort[1] <- "calf"
  testthat::expect_error(
    whep::calculate_livestock_water(x, rates),
    class = "whep_livestock_water_coverage"
  )
  rates <- .water_rates_fixture()
  rates$drinking_l_day[1] <- -1
  testthat::expect_error(
    whep::calculate_livestock_water(x, rates),
    class = "whep_livestock_water_input"
  )
  x$animal_days[1] <- NA_real_
  testthat::expect_error(
    whep::calculate_livestock_water(x, .water_rates_fixture()),
    class = "whep_livestock_water_input"
  )
})

testthat::test_that("consumption requires explicit fractions and withdrawal basis", {
  x <- .water_cohort_fixture()
  rates <- .water_rates_fixture()
  fractions <- tibble::tibble(
    rate_id = rates$rate_id,
    drinking_fraction = c(0.8, 0.8),
    service_fraction = c(0.05, 0.05),
    consumption_source = "Wisser 2024 assumption, scenario fixture only",
    consumption_basis = "assumed"
  )
  out <- whep::calculate_livestock_water(x, rates, fractions)
  testthat::expect_equal(out$consumption_m3, c(2.44, 0.162))
  testthat::expect_true(all(out$consumption_m3 <= out$direct_water_m3))
  testthat::expect_identical(out$consumption_basis, rep("assumed", 2))
  testthat::expect_error(
    whep::calculate_livestock_water(x, rates, fractions[1, ]),
    class = "whep_livestock_water_consumption"
  )
  fractions$service_fraction[1] <- 1.1
  testthat::expect_error(
    whep::calculate_livestock_water(x, rates, fractions),
    class = "whep_livestock_water_consumption"
  )
  fractions$service_fraction[1] <- 0.05
  rates$quantity_basis <- "requirement"
  testthat::expect_error(
    whep::calculate_livestock_water(x, rates, fractions),
    class = "whep_livestock_water_consumption"
  )
})

testthat::test_that("source scope and extrapolation are explicit", {
  x <- .dairy_water_fixture()
  x$cohort <- "calf"
  testthat::expect_error(
    whep::estimate_dairy_drinking(x),
    class = "whep_livestock_water_scope"
  )
  x <- .dairy_water_fixture()
  x$minimum_temperature_c <- 20
  testthat::expect_error(
    whep::estimate_dairy_drinking(x),
    class = "whep_livestock_water_extrapolation"
  )
  testthat::expect_warning(
    out <- whep::estimate_dairy_drinking(x, extrapolation = "allow"),
    class = "whep_livestock_water_extrapolation"
  )
  testthat::expect_true(out$water_extrapolated)
  testthat::expect_error(whep::estimate_dairy_drinking(x, method = "unknown"))
  x$animal_days <- Inf
  testthat::expect_error(
    whep::estimate_dairy_drinking(x, "legacy_adult"),
    class = "whep_livestock_water_input"
  )
})

testthat::test_that("empty, zero and provenance inputs preserve their meaning", {
  x <- .water_cohort_fixture()
  rates <- .water_rates_fixture()
  testthat::expect_equal(
    nrow(whep::calculate_livestock_water(x[0, ], rates)),
    0
  )
  x$animal_days <- 0
  testthat::expect_equal(
    whep::calculate_livestock_water(x, rates)$direct_water_m3,
    c(0, 0)
  )
  x$exposure_source <- " "
  testthat::expect_error(
    whep::calculate_livestock_water(x, rates),
    class = "whep_livestock_water_input"
  )
  x <- .water_cohort_fixture()
  x$rate_source <- "conflicts with rate table"
  testthat::expect_error(
    whep::calculate_livestock_water(x, rates),
    class = "whep_livestock_water_input"
  )
  x <- .dairy_water_fixture()
  x$rainfall_mm_day <- -1
  testthat::expect_error(
    whep::estimate_dairy_drinking(x, extrapolation = "allow"),
    class = "whep_livestock_water_input"
  )
})

.water_intake_case <- function() {
  .dairy_water_fixture() |>
    dplyr::mutate(
      rate_id = "study_means",
      assessment_period = "2003–2005 winter trials",
      exposure_source = "Cardot 2008 Tables 1 and 3",
      exposure_basis = "observed cow-days"
    )
}
.water_intake_rates <- function(x) {
  whep::estimate_dairy_drinking(x) |>
    dplyr::transmute(
      rate_id,
      item_cbs_code,
      cohort,
      production_system,
      drinking_l_day,
      service_l_day = NA_real_,
      rate_source = "Cardot 2008 Table 5",
      rate_basis = "modelled",
      quantity_basis
    )
}

testthat::test_that("empirical intake composes without invented annual attribution", {
  x <- .water_intake_case()
  rates <- .water_intake_rates(x)
  out <- whep::calculate_livestock_water(x, rates)
  testthat::expect_identical(out$quantity_basis, "free_drinking_intake")
  testthat::expect_identical(out$assessment_period, x$assessment_period)
  testthat::expect_identical(out$animal_days, x$animal_days)
  testthat::expect_false(any(
    c("year", "reporting_polity_code") %in% names(out)
  ))
  testthat::expect_equal(out$drinking_m3, 153.933252)
  testthat::expect_true(is.na(out$service_m3))
  testthat::expect_true(is.na(out$direct_water_m3))
  testthat::expect_true(is.na(out$consumption_m3))
  testthat::expect_identical(out$coverage_direct_water, "service_unknown")
})

testthat::test_that("optional unresolved identities stay unresolved", {
  x <- .water_intake_case() |>
    dplyr::mutate(year = NA_integer_, reporting_polity_code = NA_character_)
  out <- whep::calculate_livestock_water(x, .water_intake_rates(x))
  testthat::expect_identical(out$year, x$year)
  testthat::expect_identical(out$reporting_polity_code, x$reporting_polity_code)
})

testthat::test_that("intake cannot be combined with a service or consumption basis", {
  x <- .water_intake_case()
  rates <- .water_intake_rates(x)
  rates$service_l_day <- 0
  testthat::expect_error(
    whep::calculate_livestock_water(x, rates),
    class = "whep_livestock_water_input"
  )
  rates$service_l_day <- NA_real_
  fractions <- tibble::tibble(
    rate_id = "study_means",
    drinking_fraction = 0.8,
    service_fraction = 0.05,
    consumption_source = "assumed fixture",
    consumption_basis = "assumed"
  )
  testthat::expect_error(
    whep::calculate_livestock_water(x, rates, fractions),
    class = "whep_livestock_water_consumption"
  )
  rates$quantity_basis <- "withdrawal"
  testthat::expect_error(
    whep::calculate_livestock_water(x, rates),
    class = "whep_livestock_water_input"
  )
})

testthat::test_that("empirical rates retain their own source and modelling basis", {
  out <- whep::estimate_dairy_drinking(.water_intake_case())
  testthat::expect_match(
    out$drinking_rate_source,
    "10.3168/jds.2007-0204",
    fixed = TRUE
  )
  testthat::expect_identical(out$drinking_rate_basis, "modelled")
  legacy <- whep::estimate_dairy_drinking(.water_intake_case(), "legacy_adult")
  testthat::expect_identical(legacy$drinking_rate_basis, "assumed")
})
