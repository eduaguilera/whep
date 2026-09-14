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

# The China block, real `faostat-fbs-old` values (thousands) for 2000: the four
# areas that report separately and the aggregate over them. Area 351 is
# numbered below 5000 and DOES land on a `polity_area_code` of its own, so a
# filter on the bucket alone admitted it beside its own members (#939). Kept out
# of `.fbsp_old()` because area 41 alone is over a billion people, which is what
# the `World` assertion there uses as its tell.
.fbsp_china <- function() {
  tibble::tribble(
    ~`Area Code`, ~Area,                  ~`Item Code`, ~`Element Code`, ~Year, ~Value,
    41L,          "China, mainland",      2501L,        511L,            2000L, 1280429,
    96L,          "China, Hong Kong SAR", 2501L,        511L,            2000L, 6835,
    128L,         "China, Macao SAR",     2501L,        511L,            2000L, 432,
    214L,         "China, Taiwan",        2501L,        511L,            2000L, 21935,
    351L,         "China",                2501L,        511L,            2000L, 1309631
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

testthat::test_that("an aggregate with a bucket but no polity is dropped", {
  # #939. Area 351 "China" is FAOSTAT's aggregate over 41, 96, 128 and 214. It
  # is numbered below 5000, so the `>= 5000` reasoning this reader documented
  # does not reach it, and it resolves to `polity_area_code` 351 with
  # `polity_code` NA -- the crosswalk marks it "unmapped". Filtering on the
  # bucket alone therefore kept it, and read_population()'s FBS fill then added
  # it beside its four members in every year 1961-2023.
  out <- suppressMessages(
    whep::read_fbs_population(
      data = list(fbs_old = .fbsp_china(), fbs_new = .fbsp_new())
    )
  )
  testthat::expect_false(any(out$area_code == 351L))
  testthat::expect_setequal(
    dplyr::filter(out, .data$year == 2000L)$area_code,
    c(41L, 96L, 128L, 214L)
  )
  # The invariant, not a hand-picked equality: what is left sums to EXACTLY the
  # aggregate the dropped row carried, so dropping it removes a duplicate
  # rather than a territory. It holds on the real pins too, to within 0.011%
  # over all 63 years.
  testthat::expect_equal(
    sum(dplyr::filter(out, .data$year == 2000L)$population),
    1309631 * 1000
  )
})

testthat::test_that("the dropped aggregate is named, not dropped in silence", {
  testthat::expect_message(
    whep::read_fbs_population(
      data = list(fbs_old = .fbsp_china(), fbs_new = .fbsp_new())
    ),
    "351"
  )
  # And an input with no such aggregate says nothing about one.
  testthat::expect_equal(
    nrow(
      dplyr::filter(
        whep::add_polity_code(
          tibble::tibble(area_code = c(41L, 203L), year = c(2000L, 2000L))
        ),
        !is.na(.data$polity_area_code),
        is.na(.data$polity_code)
      )
    ),
    0L
  )
  testthat::expect_null(
    whep:::.fbs_pop_report_aggregates(
      whep::add_polity_code(
        tibble::tibble(area_code = 203L, year = 2000L)
      )
    )
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
