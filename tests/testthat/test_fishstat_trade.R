# FishStat trade preparation (whep#1618). Fixtures are FishStat's own codes:
# `UN_Code` is the reporter, `Identifier` FAO's country code.

.fishstat_countries <- function() {
  tibble::tribble(
    ~UN_Code, ~Identifier, ~ISO3_Code,
    690L,     196L,        "SYC",
    583L,     145L,        "FSM",
    254L,     69L,         "GUF",
    250L,     68L,         "FRA",
    234L,     64L,         "FRO",
    736L,     206L,        "",
    729L,     298L,        "SDN",
    531L,     286L,        "CUW",
    344L,     96L,         "HKG",
    156L,     41L,         "CHN",
    999L,     9999L,       ""
  )
}

.fishstat_trade_raw <- function(rows) {
  rows |>
    dplyr::mutate(
      MEASURE = "Q_tpw",
      COMMODITY.FAO_CODE = "034.1.1.1.10"
    )
}

.fishstat_commodities <- function() {
  tibble::tibble(Code = "034.1.1.1.10", ISSCAAP = 35L)
}

.fishstat_bridge <- function() {
  tibble::tibble(isscaap_group = 35L, item_cbs_code = 2763L)
}

.prepare_fixture <- function(rows, countries = .fishstat_countries()) {
  whep:::.prepare_fishstat_trade(
    .fishstat_trade_raw(rows),
    .fishstat_commodities(),
    countries,
    .fishstat_bridge()
  )
}

.status_of <- function(pin, reporter, year) {
  pin |>
    dplyr::filter(
      .data$`Reporter Code` == reporter,
      .data$Year == year
    ) |>
    dplyr::pull("Coverage Status")
}

testthat::test_that("every source reporter reaches the pin, held or adopted", {
  testthat::skip_if_not_installed("pointblank")
  rows <- tibble::tribble(
    ~COUNTRY_REPORTER.UN_CODE, ~TRADE_FLOW.ALPHA_CODE, ~PERIOD, ~VALUE,
    690L,                      "I",                    2010L,   100,
    234L,                      "E",                    2010L,   40,
    736L,                      "I",                    1990L,   7,
    583L,                      "I",                    1990L,   3,
    999L,                      "I",                    2010L,   2
  )
  pin <- .prepare_fixture(rows)

  # The identity the old pin satisfied too: what was kept still sums. It holds
  # just as well with a reporter missing, so it is not the test.
  testthat::expect_equal(sum(pin$Value), sum(rows$VALUE))
  testthat::expect_setequal(
    pin$`Reporter Code`,
    sprintf("%03d", rows$COUNTRY_REPORTER.UN_CODE)
  )
  # Seychelles had no row in the lookup the 2026-03-30 pin was built with;
  # the Faroe Islands and the blank-ISO3 `Sudan [former]` were dropped by it.
  testthat::expect_equal(.status_of(pin, "690", 2010L), "adopted")
  testthat::expect_equal(.status_of(pin, "234", 2010L), "adopted")
  testthat::expect_equal(.status_of(pin, "736", 1990L), "adopted")
  testthat::expect_equal(
    pin |>
      dplyr::filter(.data$`Reporter Code` == "736") |>
      dplyr::pull("Polity Code"),
    "SUD-1956-2011"
  )
  testthat::expect_equal(.status_of(pin, "999", 2010L), "area_unresolved")
  pointblank::expect_col_vals_in_set(
    pin,
    "Coverage Status",
    c("adopted", "polity_not_live", "area_unresolved")
  )
})

testthat::test_that("Micronesia before 1991 is held, not given the 1991 polity", {
  rows <- tibble::tribble(
    ~COUNTRY_REPORTER.UN_CODE, ~TRADE_FLOW.ALPHA_CODE, ~PERIOD, ~VALUE,
    583L,                      "I",                    1990L,   5,
    583L,                      "I",                    1991L,   6
  )
  pin <- .prepare_fixture(rows)

  testthat::expect_equal(.status_of(pin, "583", 1990L), "polity_not_live")
  testthat::expect_equal(.status_of(pin, "583", 1991L), "adopted")
  held <- dplyr::filter(pin, .data$Year == 1990L)
  testthat::expect_true(is.na(held$`Polity Code`))
  testthat::expect_equal(held$`Area Code`, 145L)
  testthat::expect_equal(held$Value, 5)
})

testthat::test_that("FishStat identifiers FAOSTAT numbers differently map by ISO3", {
  rows <- tibble::tribble(
    ~COUNTRY_REPORTER.UN_CODE, ~TRADE_FLOW.ALPHA_CODE, ~PERIOD, ~VALUE,
    729L,                      "E",                    2015L,   1,
    531L,                      "I",                    2015L,   2
  )
  pin <- .prepare_fixture(rows)

  testthat::expect_equal(
    pin |>
      dplyr::arrange(.data$`Reporter Code`) |>
      dplyr::pull("Area Code"),
    c(279L, 276L)
  )
  testthat::expect_true(all(pin$`Coverage Status` == "adopted"))
})

testthat::test_that("a reporter mapped to another country's area aborts", {
  # FishStat gives Zanzibar identifier 276, which is FAOSTAT's Sudan.
  countries <- tibble::tribble(
    ~UN_Code, ~Identifier, ~ISO3_Code,
    836L,     276L,        "EAZ"
  )
  rows <- tibble::tribble(
    ~COUNTRY_REPORTER.UN_CODE, ~TRADE_FLOW.ALPHA_CODE, ~PERIOD, ~VALUE,
    836L,                      "I",                    2015L,   1
  )
  testthat::expect_error(
    .prepare_fixture(rows, countries),
    class = "whep_fishstat_area_mismatch"
  )
  # Listed but not trading, it is not checked.
  testthat::expect_no_error(
    whep:::.fishstat_reporter_areas(countries, character())
  )
})

testthat::test_that("a reporter missing from the country list aborts", {
  rows <- tibble::tribble(
    ~COUNTRY_REPORTER.UN_CODE, ~TRADE_FLOW.ALPHA_CODE, ~PERIOD, ~VALUE,
    123L,                      "I",                    2015L,   1
  )
  testthat::expect_error(.prepare_fixture(rows), "does not carry")
})

testthat::test_that("French Guiana reports to 1995 and is inside France from 1996", {
  ok <- tibble::tribble(
    ~COUNTRY_REPORTER.UN_CODE, ~TRADE_FLOW.ALPHA_CODE, ~PERIOD, ~VALUE,
    254L,                      "I",                    1995L,   9,
    250L,                      "I",                    1996L,   50
  )
  pin <- .prepare_fixture(ok)
  testthat::expect_equal(.status_of(pin, "254", 1995L), "adopted")

  twice <- dplyr::bind_rows(
    ok,
    tibble::tibble(
      COUNTRY_REPORTER.UN_CODE = 254L,
      TRADE_FLOW.ALPHA_CODE = "I",
      PERIOD = 1996L,
      VALUE = 4
    )
  )
  testthat::expect_error(
    .prepare_fixture(twice),
    class = "whep_fishstat_double_count"
  )
})

testthat::test_that("Sudan reports separately only from 2012", {
  rows <- tibble::tribble(
    ~COUNTRY_REPORTER.UN_CODE, ~TRADE_FLOW.ALPHA_CODE, ~PERIOD, ~VALUE,
    736L,                      "I",                    2011L,   3,
    729L,                      "I",                    2012L,   2
  )
  testthat::expect_no_error(.prepare_fixture(rows))
  rows$PERIOD[[2]] <- 2011L
  testthat::expect_error(
    .prepare_fixture(rows),
    class = "whep_fishstat_double_count"
  )
})

testthat::test_that("two reporters on one polity abort unless one is empty", {
  countries <- tibble::tribble(
    ~UN_Code, ~Identifier, ~ISO3_Code,
    690L,     196L,        "SYC",
    691L,     196L,        ""
  )
  rows <- tibble::tribble(
    ~COUNTRY_REPORTER.UN_CODE, ~TRADE_FLOW.ALPHA_CODE, ~PERIOD, ~VALUE,
    690L,                      "I",                    2010L,   1,
    691L,                      "I",                    2010L,   0
  )
  # A closing zero row, the shape of `Ethiopia PDR [former]` in 1992.
  testthat::expect_no_error(.prepare_fixture(rows, countries))
  rows$VALUE[[2]] <- 1
  testthat::expect_error(
    .prepare_fixture(rows, countries),
    class = "whep_fishstat_double_count"
  )
})

testthat::test_that("a member and its container may both report only if FAO excludes it", {
  rows <- tibble::tribble(
    ~reporter_code, ~polity_code,    ~year, ~value, ~coverage_status,
    "344",          "HKG-1842-2025", 2010L, 1,      "adopted",
    "156",          "CHN-1950-2025", 2010L, 5,      "adopted"
  )
  edges <- tibble::tribble(
    ~member_code,    ~container_code, ~start_year, ~end_year,
    "HKG-1842-2025", "CHN-1950-2025", 1997L,       2025L
  )
  testthat::expect_no_error(
    whep:::.check_fishstat_containment(rows, edges)
  )
  rows$reporter_code[[1]] <- "345"
  testthat::expect_error(
    whep:::.check_fishstat_containment(rows, edges),
    class = "whep_fishstat_double_count"
  )
  # Outside the edge's years the two are separate territories.
  rows$year <- 1990L
  testthat::expect_no_error(
    whep:::.check_fishstat_containment(rows, edges)
  )
})

testthat::test_that("the CBS reader refuses held rows by name", {
  pin <- data.table::data.table(
    Year = c(1990L, 1991L),
    `Reporter Code` = "583",
    `Area Code` = 145L,
    `Polity Code` = c(NA, "FSM-1991-2025"),
    `Coverage Status` = c("polity_not_live", "adopted"),
    `Item Code` = 2763L,
    Element = "Import quantity",
    Value = c(5, 6),
    Unit = "tonnes"
  )
  testthat::local_mocked_bindings(
    .read_input = function(pin_alias, years = NULL, year_col = NULL) {
      data.table::copy(pin)
    },
    .package = "whep"
  )
  testthat::expect_warning(
    out <- whep:::.read_fishstat_trade(),
    class = "whep_fishstat_unadopted"
  )
  testthat::expect_equal(out$year, 1991L)
  testthat::expect_equal(out$value, 6)
})

testthat::test_that("a pin without the coverage column reads as before", {
  pin <- data.table::data.table(
    Year = 2010L,
    `Area Code` = 196L,
    `Item Code` = 2763L,
    Element = "Export quantity",
    Value = 4,
    Unit = "tonnes"
  )
  testthat::expect_identical(
    whep:::.fishstat_adopted_rows(data.table::copy(pin)),
    pin
  )
})

testthat::test_that("the coverage summary names held reporters", {
  rows <- tibble::tribble(
    ~COUNTRY_REPORTER.UN_CODE, ~TRADE_FLOW.ALPHA_CODE, ~PERIOD, ~VALUE,
    583L,                      "I",                    1989L,   2,
    583L,                      "E",                    1990L,   3,
    690L,                      "I",                    2010L,   1
  )
  coverage <- whep:::.fishstat_trade_coverage(.prepare_fixture(rows))
  held <- dplyr::filter(coverage, .data$`Coverage Status` != "adopted")
  testthat::expect_equal(held$`Reporter Code`, "583")
  testthat::expect_equal(c(held$first_year, held$last_year), c(1989L, 1990L))
  testthat::expect_equal(held$tonnes, 5)
})
