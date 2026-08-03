# Tests for read_population().

.popf_raw <- function() {
  tibble::tribble(
    ~Year,
    ~area,
    ~area_code,
    ~pop,
    2010L,
    "Spain",
    "ESP",
    46.6,
    2010L,
    "Germany",
    "DEU",
    81.8,
    2010L,
    "Ethiopia",
    "ETH",
    87.6,
    2010L,
    "Sudan",
    "SDN",
    35.0,
    2010L,
    "Africa Other",
    "RAFR",
    0.2,
    2011L,
    "Spain",
    "ESP",
    46.7
  )
}

testthat::test_that("the example fixture matches the documented contract", {
  out <- whep::read_population(example = TRUE)
  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_named(out, c("year", "area_code", "population"))
  testthat::expect_true(all(out$population > 0))
  testthat::expect_type(out$area_code, "integer")
})

testthat::test_that("ISO3 becomes a numeric area code and thousands persons", {
  testthat::expect_message(
    whep::read_population(data = list(gdp_population = .popf_raw())),
    "no numeric"
  )
  out <- suppressMessages(
    whep::read_population(data = list(gdp_population = .popf_raw()))
  )
  esp <- dplyr::filter(out, .data$year == 2010L, .data$area_code == 203L)
  testthat::expect_equal(esp$population, 46600)
  deu <- dplyr::filter(out, .data$year == 2010L, .data$area_code == 79L)
  testthat::expect_equal(deu$population, 81800)
})

testthat::test_that("historical twin ISO3 codes resolve to one area code each", {
  # ETH is both 238 (Ethiopia) and 62 (Ethiopia PDR) in regions_full$code, and
  # SDN both 276 and 206. Mapping through polity_area_code collapses each pair,
  # so neither country is duplicated nor lands on its predecessor.
  out <- suppressMessages(
    whep::read_population(data = list(gdp_population = .popf_raw()))
  )
  testthat::expect_equal(sum(out$area_code == 238L & out$year == 2010L), 1L)
  testthat::expect_equal(sum(out$area_code == 206L & out$year == 2010L), 1L)
  testthat::expect_false(any(out$area_code %in% c(62L, 276L)))
})

testthat::test_that("regional residual aggregates are dropped and reported", {
  testthat::expect_message(
    whep::read_population(data = list(gdp_population = .popf_raw())),
    "RAFR"
  )
  out <- suppressMessages(
    whep::read_population(data = list(gdp_population = .popf_raw()))
  )
  testthat::expect_false(any(is.na(out$area_code)))
  # The residual's 200 people are gone, so the total is countries-only.
  testthat::expect_equal(sum(out$population[out$year == 2010L]), 251000)
})

testthat::test_that("years filter the result", {
  out <- suppressMessages(
    whep::read_population(
      years = 2011L,
      data = list(gdp_population = .popf_raw())
    )
  )
  testthat::expect_setequal(out$year, 2011L)
  testthat::expect_equal(nrow(out), 1L)
})

testthat::test_that("missing required columns abort", {
  testthat::expect_error(
    whep::read_population(data = list(gdp_population = tibble::tibble(x = 1))),
    "gdp_population"
  )
})
