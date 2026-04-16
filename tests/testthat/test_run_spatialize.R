testthat::test_that("lpjml preset disables type-aware allocation", {
  config <- getFromNamespace(".resolve_spatialize_config", "whep")(
    "lpjml",
    list()
  )
  testthat::expect_false(config$use_type_constraint)
  testthat::expect_true(config$aggregate_to_cft)
})

testthat::test_that("whep preset enables type-aware allocation", {
  config <- getFromNamespace(".resolve_spatialize_config", "whep")(
    "whep",
    list()
  )
  testthat::expect_true(config$use_type_constraint)
})

testthat::test_that("overrides take precedence over preset defaults", {
  config <- getFromNamespace(".resolve_spatialize_config", "whep")(
    "lpjml",
    list(use_type_constraint = TRUE, max_iterations = 50L)
  )
  testthat::expect_true(config$use_type_constraint)
  testthat::expect_equal(config$max_iterations, 50L)
})

testthat::test_that("unknown override keys are rejected", {
  testthat::expect_error(
    getFromNamespace(".validate_overrides", "whep")(
      list(not_a_real_flag = TRUE)
    ),
    "not_a_real_flag"
  )
})

testthat::test_that("unnamed overrides are rejected", {
  testthat::expect_error(
    getFromNamespace(".validate_overrides", "whep")(list(TRUE)),
    "named"
  )
})

testthat::test_that("lpjml default years intersect benchmark years with availability", {
  country_areas <- tibble::tribble(
      ~year, ~area_code, ~item_prod_code, ~harvested_area_ha,
      1995L,         1L,             15L,               500,
      2000L,         1L,             15L,              1000,
      2005L,         1L,             15L,              1100,
      2010L,         1L,             15L,              1200
    )
  picked <- getFromNamespace(".resolve_years", "whep")(
    years = NULL,
    preset = "lpjml",
    country_areas = country_areas
  )
  testthat::expect_setequal(picked, 2000L)
})

testthat::test_that("lpjml falls back to all available years if none of the samples match", {
  country_areas <- tibble::tribble(
      ~year, ~area_code, ~item_prod_code, ~harvested_area_ha,
      2015L,         1L,             15L,               500,
      2020L,         1L,             15L,               510
    )
  picked <- getFromNamespace(".resolve_years", "whep")(
    years = NULL,
    preset = "lpjml",
    country_areas = country_areas
  )
  testthat::expect_setequal(picked, c(2015L, 2020L))
})

testthat::test_that("whep default years use all available years", {
  country_areas <- tibble::tribble(
    ~year, ~area_code, ~item_prod_code, ~harvested_area_ha,
    1995L,         1L,             15L,               500,
    2000L,         1L,             15L,              1000,
    2005L,         1L,             15L,              1100
  )
  picked <- getFromNamespace(".resolve_years", "whep")(
    years = NULL,
    preset = "whep",
    country_areas = country_areas
  )
  testthat::expect_setequal(picked, c(1995L, 2000L, 2005L))
})

testthat::test_that("custom overrides produce a distinct default output directory", {
  fn <- getFromNamespace(".default_spatialize_out_dir", "whep")
  base <- fn("/tmp/l", "lpjml", list())
  custom <- fn("/tmp/l", "lpjml", list(use_type_constraint = TRUE))
  testthat::expect_false(base == custom)
  testthat::expect_match(custom, "_custom$")
})

testthat::test_that("lpjml default years are the 25y benchmark sequence", {
  benchmarks <- getFromNamespace(".benchmark_years", "whep")()
  testthat::expect_setequal(
    benchmarks,
    c(1850L, 1875L, 1900L, 1925L, 1950L, 1975L, 2000L)
  )

  country_areas <- tibble::tibble(year = 1850L:2020L)
  picked <- getFromNamespace(".resolve_years", "whep")(
    years = NULL,
    preset = "lpjml",
    country_areas = country_areas
  )
  testthat::expect_setequal(picked, benchmarks)
})

testthat::test_that("unknown components are rejected", {
  testthat::expect_error(
    getFromNamespace(".validate_components", "whep")("soil"),
    "soil"
  )
})

testthat::test_that("empty components vector is rejected", {
  testthat::expect_error(
    getFromNamespace(".validate_components", "whep")(character()),
    "empty"
  )
})

testthat::test_that("components de-duplicate preserving known names", {
  fn <- getFromNamespace(".validate_components", "whep")
  testthat::expect_setequal(
    fn(c("landuse", "landuse", "livestock")),
    c("landuse", "livestock")
  )
})
