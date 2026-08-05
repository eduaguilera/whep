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

# The rows of the real `gdp-population` pin for 2015 whose ISO3 codes share an
# `area_code` with another, plus Spain as an unfolded control and one regional
# residual. Real pin values (thousands), so the folded totals this fixture
# produces are the ones a real read_population(years = 2015L) prints: 999 =
# 28,278,673 and 206 = 49,365,472.
.popf_folded <- function() {
  tibble::tribble(
    ~Year, ~area,               ~area_code, ~pop,
    2015L, "Spain",             "ESP",      46431.344,
    2015L, "Sudan",             "SDN",      38171.172,
    2015L, "South Sudan",       "SSD",      11194.300,
    2015L, "Syria",             "SYR",      19205.178,
    2015L, "North Macedonia",   "MKD",      2107.965,
    2015L, "Palestine",         "PSE",      4484.615,
    2015L, "Eswatini",          "SWZ",      1133.941,
    2015L, "Equatorial Guinea", "GNQ",      1346.974,
    2015L, "Africa Other",      "RAFR",     2498.894
  )
}

testthat::test_that("the example fixture matches the documented contract", {
  out <- whep::read_population(example = TRUE)
  testthat::expect_s3_class(out, "tbl_df")
  # The polity columns are part of the contract now, not an extra: `read_population()`
  # resolves ISO3 to a numeric `area_code` and carries a `year`, so it is an area-keyed
  # export with a year and #424 requires it to say which polity each row belongs to.
  # `expect_named()` is exact, so it has to name them -- and asserting the exact set is
  # the point: a silently dropped polity column should fail here.
  testthat::expect_named(
    out,
    c(
      "year",
      "area_code",
      "polity_area_code",
      "reporting_polity_code",
      "reporting_polity_name",
      "reporting_polity_has_geometry",
      "population"
    )
  )
  testthat::expect_true(all(out$population > 0))
  testthat::expect_type(out$area_code, "integer")
  # Populated, not merely present.
  testthat::expect_false(anyNA(out$reporting_polity_code))
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

# ---- The many-to-one fold onto aggregate buckets ---------------------------
#
# `area_code` is `polity_area_code`, a bucket rather than an identity, so these
# tests pin the aggregation BY VALUE: it has to read as deliberate, and a change
# to which ISO3 codes land on 999 or 206 has to fail something (#482).

testthat::test_that("the Rest-of-World fold is reported, not silent", {
  testthat::expect_message(
    whep::read_population(data = list(gdp_population = .popf_folded())),
    "aggregate"
  )
  testthat::expect_message(
    whep::read_population(data = list(gdp_population = .popf_folded())),
    "999 \\(GNQ \\+ MKD \\+ PSE \\+ SWZ \\+ SYR\\)"
  )
  # Two buckets, seven ISO3 codes between them: 5 on 999 and SDN + SSD on 206.
  testthat::expect_message(
    whep::read_population(data = list(gdp_population = .popf_folded())),
    "Folded 7 ISO3 codes into 2 aggregate"
  )
})

testthat::test_that("the folded rows carry the summed population", {
  out <- suppressMessages(
    whep::read_population(data = list(gdp_population = .popf_folded()))
  )
  # These are the real 2015 pin values: reading them off the fixture must give
  # the same numbers a real read_population(years = 2015L) prints.
  row_of <- function(code) {
    dplyr::pull(dplyr::filter(out, .data$area_code == code), "population")
  }
  testthat::expect_equal(row_of(999L), 28278673)
  testthat::expect_equal(row_of(206L), 49365472)
  testthat::expect_equal(row_of(203L), 46431344)
  # One row per bucket, and no member territory survives as its own row.
  testthat::expect_equal(nrow(out), 3L)
  testthat::expect_setequal(out$area_code, c(203L, 206L, 999L))
  # The output itself says 999 is not a country.
  testthat::expect_equal(
    dplyr::pull(
      dplyr::filter(out, .data$area_code == 999L),
      "polity_area_code"
    ),
    999L
  )
})

testthat::test_that("the fold-bucket summary lists members per bucket", {
  parsed <- whep:::.pop_parse(.popf_folded(), NULL)
  folded <- whep:::.pop_folded_buckets(whep:::.pop_folded_cells(parsed))
  testthat::expect_equal(folded$area_code, c(206L, 999L))
  testthat::expect_equal(folded$n_iso3, c(2L, 5L))
  testthat::expect_equal(folded$codes[[1]], "SDN + SSD")
  # An unfolded pin says nothing.
  unfolded <- whep:::.pop_parse(
    tibble::tibble(Year = 2015L, area_code = "ESP", pop = 46431.344),
    NULL
  )
  testthat::expect_equal(nrow(whep:::.pop_folded_cells(unfolded)), 0L)
  testthat::expect_null(whep:::.pop_report_folded(unfolded))
})

testthat::test_that("a bucket with one member in a year is not a fold", {
  # 206 carries Sudan alone before South Sudan exists, so that row IS a country
  # and must not be counted as an aggregate. Counting per bucket instead of per
  # bucket-year would report every pre-2012 Sudan row as folded.
  pre <- tibble::tribble(
    ~Year, ~area_code, ~pop,
    2005L, "SDN",      33.0,
    2005L, "ESP",      43.0,
    2015L, "SDN",      38.0,
    2015L, "SSD",      11.0,
    2015L, "ESP",      46.0
  )
  cells <- whep:::.pop_folded_cells(whep:::.pop_parse(pre, NULL))
  testthat::expect_setequal(cells$year, 2015L)
  testthat::expect_setequal(cells$iso3c, c("SDN", "SSD"))
  # 49,000 of the 171,000 persons in range, not the 82,000 that counting whole
  # buckets (SDN 2005 + SDN 2015 + SSD 2015) would claim.
  testthat::expect_equal(sum(cells$population), 49000)
})
