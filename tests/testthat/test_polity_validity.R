# Tests for the shared year-validity helpers the gridded cell_polity consumers
# route through (whep#462 introduced them for the water balance, whep#675
# promoted them here so ~8 entry points cannot spell the same thing differently).
#
# Area 277 (South Sudan) is the sharpest case: its only polity SSD-2011-2025
# postdates every historical year, so a cell the present-day crosswalk labels
# 277 is out of span for the whole pre-2011 record. Area 203 (Spain) is in span
# throughout and is the control.

.pv_fixture <- function() {
  tibble::tribble(
    ~area_code, ~year, ~value,
    203L, 1990L, 1,
    203L, 2015L, 2,
    277L, 1990L, 4,
    277L, 2015L, 8
  )
}

testthat::test_that("the gap set names the out-of-span pair and nothing else", {
  gaps <- whep:::.polity_validity_gaps(.pv_fixture())

  testthat::expect_equal(nrow(gaps), 1L)
  testthat::expect_equal(gaps$area_code, 277L)
  testthat::expect_equal(gaps$year, 1990L)
})

testthat::test_that("a table with no area_code or year has an empty gap set", {
  gaps <- whep:::.polity_validity_gaps(tibble::tibble(value = 1))

  testthat::expect_equal(nrow(gaps), 0L)
  testthat::expect_named(gaps, c("area_code", "year"))
})

testthat::test_that("keep names the stand-in rows instead of staying silent", {
  testthat::expect_warning(
    out <- whep:::.resolve_polity_validity(.pv_fixture(), "keep"),
    "did not exist in that row's year"
  )

  # Every row survives and every value is untouched: "keep" is today's output
  # plus a warning, which is what makes it a safe default.
  testthat::expect_equal(nrow(out), 4L)
  testthat::expect_equal(sum(out$value), 15)
  # And the anachronism it warns about is real: 1990 South Sudan.
  testthat::expect_equal(
    out$reporting_polity_code[out$area_code == 277L & out$year == 1990L],
    "SSD-2011-2025"
  )
})

testthat::test_that("the warning names the count, the codes and the fate", {
  testthat::expect_warning(
    whep:::.resolve_polity_validity(.pv_fixture(), "keep"),
    "1 row over 1 area code"
  )
  testthat::expect_warning(
    whep:::.resolve_polity_validity(.pv_fixture(), "keep"),
    "Area codes: 277"
  )
  testthat::expect_warning(
    whep:::.resolve_polity_validity(.pv_fixture(), "keep"),
    "years 1990-1990.*kept as-is"
  )
  testthat::expect_warning(
    whep:::.resolve_polity_validity(.pv_fixture(), "drop"),
    "they are dropped"
  )
  testthat::expect_warning(
    whep:::.resolve_polity_validity(.pv_fixture(), "flag"),
    "kept and flagged in reporting_polity_out_of_span"
  )
})

testthat::test_that("an all-in-span table warns about nothing", {
  in_span <- dplyr::filter(.pv_fixture(), area_code == 203L)

  testthat::expect_no_warning(
    out <- whep:::.resolve_polity_validity(in_span, "keep")
  )
  testthat::expect_equal(nrow(out), 2L)
})

testthat::test_that("flag marks exactly the out-of-span rows, moving nothing", {
  testthat::expect_warning(
    kept <- whep:::.resolve_polity_validity(.pv_fixture(), "keep")
  )
  testthat::expect_warning(
    flagged <- whep:::.resolve_polity_validity(.pv_fixture(), "flag")
  )

  pointblank::expect_col_exists(flagged, "reporting_polity_out_of_span")
  testthat::expect_equal(
    flagged$reporting_polity_out_of_span,
    flagged$area_code == 277L & flagged$year == 1990L
  )
  # Same rows, same numbers: "flag" is "keep" plus one logical column.
  testthat::expect_equal(
    dplyr::select(flagged, -"reporting_polity_out_of_span"),
    kept
  )
})

testthat::test_that("drop removes only the out-of-span rows", {
  testthat::expect_warning(
    out <- whep:::.resolve_polity_validity(.pv_fixture(), "drop")
  )

  testthat::expect_equal(nrow(out), 3L)
  testthat::expect_equal(sum(out$value), 11)
  testthat::expect_false(any(out$area_code == 277L & out$year == 1990L))
  # 277 does not vanish: its 2015 row is in span and stays.
  testthat::expect_true(any(out$area_code == 277L))
})

testthat::test_that("one area_code still maps to one polity bucket", {
  # The whep#561/#563 failure mode: a change that conserves mass while
  # splitting a bucket. None of the three modes may split one.
  modes <- c("keep", "flag", "drop")
  buckets <- purrr::map(modes, function(mode) {
    suppressWarnings(
      whep:::.resolve_polity_validity(.pv_fixture(), mode)
    ) |>
      dplyr::distinct(area_code, polity_area_code) |>
      dplyr::count(area_code)
  })

  purrr::walk(buckets, function(b) testthat::expect_true(all(b$n == 1L)))
})

testthat::test_that("attach_flag serves a table with no polity columns", {
  testthat::expect_warning(
    out <- whep:::.apply_polity_validity(
      .pv_fixture(),
      "flag",
      attach_flag = TRUE
    )
  )

  # The flag arrives, but no reporting-polity column is bolted onto an output
  # whose published schema does not have them.
  testthat::expect_equal(
    out$reporting_polity_out_of_span,
    out$area_code == 277L & out$year == 1990L
  )
  testthat::expect_false("reporting_polity_code" %in% names(out))
  testthat::expect_equal(nrow(out), 4L)
})

testthat::test_that("attach_flag is all-FALSE when nothing is out of span", {
  in_span <- dplyr::filter(.pv_fixture(), area_code == 203L)

  out <- whep:::.apply_polity_validity(in_span, "flag", attach_flag = TRUE)

  testthat::expect_false(any(out$reporting_polity_out_of_span))
})

testthat::test_that("keep leaves a bare table byte-identical", {
  bare <- .pv_fixture()

  testthat::expect_warning(
    out <- whep:::.apply_polity_validity(bare, "keep", attach_flag = TRUE)
  )

  testthat::expect_identical(out, bare)
})
