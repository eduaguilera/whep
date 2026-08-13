# Every test injects `data`, so the whole path stays offline. The download and
# manifest branches are exercised through the injected fetcher rather than the
# network -- a test that reaches population.un.org would turn an outage into a
# hard R CMD check ERROR (#490).

.wpp_raw <- function() {
  tibble::tribble(
    ~ISO3_code, ~LocTypeName,   ~Time, ~AgeGrpStart, ~AgeGrpSpan, ~PopMale, ~PopFemale,
    "ESP",      "Country/Area", 2010L, 0L,           5L,          1170.5,   1103.2,
    "ESP",      "Country/Area", 2010L, 5L,           5L,          1148.0,   1082.0,
    "ESP",      "Country/Area", 2010L, 100L,         -1L,         0.4,      2.1,
    "ESP",      "Country/Area", 2011L, 0L,           5L,          1160.0,   1095.0,
    "FRA",      "Country/Area", 2010L, 0L,           5L,          2050.0,   1960.0,
    "",         "Region",       2010L, 0L,           5L,          9999.0,   9999.0,
    "XXX",      "Income group", 2010L, 0L,           5L,          8888.0,   8888.0
  )
}

testthat::test_that("aggregates and unlabelled rows never reach the output", {
  # WPP mixes countries with regional and income-group aggregates in one file.
  # Summing them into a per-capita denominator would double-count the world.
  out <- whep::read_wpp_population(data = .wpp_raw())
  testthat::expect_setequal(unique(out$iso3c), c("ESP", "FRA"))
  testthat::expect_false(any(out$population > 1e7))
})

testthat::test_that("population is converted from thousands to persons", {
  out <- whep::read_wpp_population(
    years = 2010,
    data = .wpp_raw()
  )
  esp <- dplyr::filter(out, .data$iso3c == "ESP")
  # 1170.5 + 1103.2 + 1148.0 + 1082.0 + 0.4 + 2.1 thousand.
  testthat::expect_equal(esp$population, 4506.2 * 1000)
})

testthat::test_that("age_sex keeps the grain the requirement builder needs", {
  out <- whep::read_wpp_population(by = "age_sex", data = .wpp_raw())
  testthat::expect_true(
    all(c("age_start", "age_span", "sex") %in% names(out))
  )
  testthat::expect_setequal(unique(out$sex), c("m", "f"))
  # It must be directly consumable, with no reshaping in between.
  req <- whep::build_protein_requirement(
    data = list(population_age = dplyr::filter(out, .data$year == 2010))
  )
  testthat::expect_true(all(req$requirement_g_cap_day > 0))
})

testthat::test_that("the open-ended top age group gets a usable span", {
  # WPP writes the 100+ group with a negative span. Left as-is it would make
  # seq() run backwards and silently mis-weight the oldest cohort.
  out <- whep::read_wpp_population(by = "age_sex", data = .wpp_raw())
  top <- dplyr::filter(out, .data$age_start == 100L)
  testthat::expect_equal(unique(top$age_span), 1L)
})

testthat::test_that("years filters and totals collapse the age detail", {
  detail <- whep::read_wpp_population(
    by = "age_sex",
    years = 2010,
    data = .wpp_raw()
  )
  total <- whep::read_wpp_population(years = 2010, data = .wpp_raw())
  testthat::expect_setequal(unique(detail$year), 2010L)
  testthat::expect_equal(nrow(total), 2L)
  testthat::expect_equal(sum(total$population), sum(detail$population))
})

testthat::test_that("zero and missing populations are dropped, not zero-filled", {
  raw <- dplyr::mutate(.wpp_raw(), PopMale = 0, PopFemale = 0)
  out <- whep::read_wpp_population(data = raw)
  testthat::expect_equal(nrow(out), 0L)
})

testthat::test_that("a missing WPP column aborts", {
  testthat::expect_error(
    whep::read_wpp_population(
      data = dplyr::select(.wpp_raw(), -"PopFemale")
    ),
    "PopFemale"
  )
})

testthat::test_that("an unknown grain is rejected", {
  testthat::expect_error(
    whep::read_wpp_population(data = .wpp_raw(), by = "nope"),
    "arg_match|must be one of|nope"
  )
})

testthat::test_that("a manifest mismatch aborts and discards the file", {
  # The manifest is WHEP-recorded, not published by UN DESA, so a mismatch most
  # likely means the upstream file was revised. Either way the file must not be
  # used, and must not be left behind to be picked up as a cache hit.
  dir <- withr::local_tempdir()
  fetch <- function(url, path) writeLines("not the real file", path)
  testthat::expect_error(
    whep:::.wpp_download(dir, fetch = fetch),
    "manifest"
  )
  testthat::expect_false(
    file.exists(file.path(dir, whep:::.wpp_file_name()))
  )
})

testthat::test_that("the resolver prefers an explicit dir over the cache", {
  dir <- withr::local_tempdir()
  testthat::expect_equal(whep:::.resolve_wpp_dir(dir), dir)
  testthat::expect_equal(
    whep:::.resolve_wpp_dir(NULL),
    whep:::.wpp_cache_dir()
  )
})
