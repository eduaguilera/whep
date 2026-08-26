# Tests for read_fbs_population(): the FAOSTAT Food Balance Sheet population,
# the one population source WHEP has that is not keyed on a present-day ISO3
# and so can reach a dissolved reporting area (#862, #787).

# The pins' own long FAOSTAT layout. Real `faostat-fbs-old` values (thousands)
# for area 186 Serbia and Montenegro, the territory neither ISO3-keyed source
# reaches, plus Spain as a control, one non-population item that must not be
# counted as one, and one FAOSTAT regional aggregate that must be dropped.
.fbsp_old <- function() {
  tibble::tribble(
    ~`Area Code`, ~Area,                   ~`Item Code`, ~`Element Code`, ~Year,  ~Value,
    186L,         "Serbia and Montenegro", 2501L,        511L,            1992L,  10429,
    186L,         "Serbia and Montenegro", 2501L,        511L,            2000L,  10801,
    186L,         "Serbia and Montenegro", 2501L,        511L,            2005L,  10471,
    203L,         "Spain",                 2501L,        511L,            2000L,  40283,
    203L,         "Spain",                 2501L,        511L,            2010L,  46071,
    203L,         "Spain",                 2901L,        664L,            2000L,  3350,
    5000L,        "World",                 2501L,        511L,            2000L,  6143494
  )
}

.fbsp_new <- function() {
  tibble::tribble(
    ~`Area Code`, ~Area,   ~`Item Code`, ~`Element Code`, ~Year, ~Value,
    203L,         "Spain", 2501L,        511L,            2010L, 46840.47,
    203L,         "Spain", 2501L,        511L,            2020L, 47363.80
  )
}

.fbsp_read <- function(...) {
  suppressMessages(
    whep::read_fbs_population(
      data = list(fbs_old = .fbsp_old(), fbs_new = .fbsp_new()),
      ...
    )
  )
}

testthat::test_that("the example fixture matches the documented contract", {
  out <- whep::read_fbs_population(example = TRUE)
  testthat::expect_s3_class(out, "tbl_df")
  testthat::expect_true(
    all(
      c("year", "area_code", "population", "source_pop") %in% names(out)
    )
  )
  # The polity columns are part of the contract for any area-keyed export with
  # a year (#424), exactly as for read_population().
  testthat::expect_true(
    all(
      c("polity_area_code", "reporting_polity_code") %in% names(out)
    )
  )
})

testthat::test_that("thousands become persons", {
  # The single conversion the whole reader exists to get right: both pins
  # publish item 2501 in thousands.
  out <- .fbsp_read()
  scg <- dplyr::filter(out, .data$area_code == 186L, .data$year == 2000L)
  testthat::expect_equal(scg$population, 10801000)
})

testthat::test_that("area 186 is covered for exactly its reporting years", {
  # This is #862: the `gdp-population` pin stops YUG at 1991 and starts
  # SRB/MNE at 2006, and UN WPP has no SCG record ever, so 1992-2005 has no
  # denominator at all. FAOSTAT is keyed on the reporting area, so it does.
  out <- .fbsp_read()
  scg <- dplyr::filter(out, .data$area_code == 186L)
  testthat::expect_setequal(scg$year, c(1992L, 2000L, 2005L))
  testthat::expect_equal(unique(scg$source_pop), "FAOSTAT FBS old")
})

testthat::test_that("only the population item and element are read", {
  # Item 2901 element 664 is kcal/capita/day in the same pin. Reading it as a
  # population would put a three-digit denominator under a country's food.
  out <- .fbsp_read()
  esp <- dplyr::filter(out, .data$area_code == 203L, .data$year == 2000L)
  testthat::expect_equal(nrow(esp), 1L)
  testthat::expect_equal(esp$population, 40283000)
})

testthat::test_that("FAOSTAT's own aggregates are dropped", {
  # `World` (5000) resolves to no polity. Summed into a denominator it would
  # double count every country in the file.
  out <- .fbsp_read()
  testthat::expect_false(any(out$area_code >= 5000L))
  testthat::expect_false(any(out$population > 1e9))
})

testthat::test_that("the newer pin wins an overlapping year", {
  # 2010 is in both, at different values, because the two vintages differ.
  # `faostat-fbs-new` has to win, matching the order the same two pins get on
  # the food side, or a per-capita ratio mixes two vintages of one year.
  out <- .fbsp_read()
  esp <- dplyr::filter(out, .data$area_code == 203L, .data$year == 2010L)
  testthat::expect_equal(nrow(esp), 1L)
  testthat::expect_equal(esp$population, 46840470)
  testthat::expect_equal(esp$source_pop, "FAOSTAT FBS new")
})

testthat::test_that("years filters both pins", {
  out <- .fbsp_read(years = 2000L)
  testthat::expect_equal(unique(out$year), 2000L)
})

testthat::test_that("a table missing a required column aborts", {
  testthat::expect_error(
    suppressMessages(
      whep::read_fbs_population(
        data = list(
          fbs_old = dplyr::select(.fbsp_old(), -"Value"),
          fbs_new = .fbsp_new()
        )
      )
    ),
    regexp = "Value"
  )
})

testthat::test_that("an input with no population rows returns no rows", {
  empty <- dplyr::filter(.fbsp_old(), .data$`Item Code` == 2901L)
  out <- suppressMessages(
    whep::read_fbs_population(
      data = list(fbs_old = empty, fbs_new = dplyr::filter(.fbsp_new(), FALSE))
    )
  )
  testthat::expect_equal(nrow(out), 0L)
})
