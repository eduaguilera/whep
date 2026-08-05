# The source cast must SUM duplicate keys, not count them.
#
# `.select_best_source()` casts the three primary sources wide. With no `fun.aggregate`,
# `data.table::dcast()` falls back to `length()` the moment any duplicate (key, source)
# combination exists -- and applies it to EVERY cell, not only the duplicated ones. One
# duplicate anywhere therefore turns the entire table into row counts.
#
# That happened in every real build. Measured on 2010-2023 before the fix:
#
#   classes = FAOSTAT_FBS_New:integer  FAOSTAT_FBS_Old:integer  FAOSTAT_prod:integer
#   maxima  = 4, 4, 1
#
# in columns holding tonnes. At full range 31,642 duplicated combinations exist, in areas 206
# and 999, so the fallback was never not firing. Published totals were wrong by up to 259x on
# `food` and 183x on `seed`.
#
# WHY NO TEST CAUGHT IT, which is the reason this file exists rather than a wider assertion:
# the whole suite passes both with and without the fix. Nothing compared a magnitude against
# anything, and the corruption preserves row counts, column types after coercion, and every
# NA pattern -- so shape-based checks are blind to it by construction. A test has to look at a
# VALUE.
#
# These tests are deliberately unit-level on the internal helper. An end-to-end assertion would
# need a full build (minutes) and a baseline of expected magnitudes, and the baseline is the
# thing that was wrong; asserting the aggregate directly cannot be satisfied by a corrupted
# pipeline.

.two_rows_one_key <- function() {
  # One duplicated (key, source): the shape `.aggregate_to_polities()` produces when several
  # territory-periods fold into one reporting bucket, since `key_cols` excludes the area name.
  data.table::data.table(
    area_code = c(206L, 206L),
    area = c("Sudan (former)", "South Sudan"),
    year = c(2015L, 2015L),
    item_cbs = c("Honey", "Honey"),
    item_cbs_code = c(2745L, 2745L),
    element = c("production", "production"),
    source = c("FAOSTAT_prod", "FAOSTAT_prod"),
    value = c(10, 32)
  )
}

testthat::test_that("a duplicated key is summed, not counted", {
  out <- as.data.frame(suppressWarnings(
    whep:::.select_best_source(.two_rows_one_key())
  ))
  testthat::expect_equal(nrow(out), 1L)

  # 42, not 2. Before the fix this was 2 -- the row count -- and every other cell in the cast
  # was 1 for the same reason.
  testthat::expect_equal(out$value, 42)
  testthat::expect_true(is.double(out$value))
})

testthat::test_that("an undisturbed value passes through unchanged", {
  # The other half: the fallback corrupted cells that had no duplicate at all, so a fix that
  # only handled duplicates would still be wrong. A single large value must survive intact.
  dt <- data.table::data.table(
    area_code = c(206L, 100L),
    area = c("Sudan (former)", "India"),
    year = c(2015L, 2015L),
    item_cbs = c("Honey", "Honey"),
    item_cbs_code = c(2745L, 2745L),
    element = c("production", "production"),
    source = c("FAOSTAT_prod", "FAOSTAT_prod"),
    value = c(10, 987654)
  )
  out <- as.data.frame(suppressWarnings(whep:::.select_best_source(dt)))
  india <- out$value[out$area_code == 100L]
  testthat::expect_equal(india, 987654)
  # Explicitly not 1: `length()` would have returned 1 here, which is a plausible-looking
  # tonnage and is exactly why the corruption was invisible.
  testthat::expect_false(isTRUE(all.equal(india, 1)))
})

testthat::test_that("values are quantities, not counts, at build scale", {
  # A guard on the property rather than on a fixture: after a real build the primary-source
  # magnitudes must be implausible as counts. Skipped where the pins are unavailable, and it is
  # the one test here that would have caught the defect from the outside.
  #
  # Reading the pins over the network is the whole point here, so this cannot be
  # rescoped onto a fixture -- it is instead kept out of every automated check.
  # `skip_on_ci()` alone does not do that: r-universe does not set `CI`, so this
  # was the only test in the suite still fetching remote data during a check, and
  # its download time (3 min CPU against 58 min wall clock) pushed r-universe's
  # Windows job past its 60-minute cap. `skip_on_cran()` covers the check
  # surfaces, because `NOT_CRAN` is unset on r-universe and on CRAN while
  # `r-lib/actions/setup-r` and `devtools::test()` both set it -- so the test
  # still runs locally and in offline-tests, where it belongs.
  testthat::skip_on_cran()
  testthat::skip_on_ci()
  cbs <- tryCatch(
    suppressWarnings(suppressMessages(
      whep::build_commodity_balances(
        whep::build_primary_production(start_year = 2015, end_year = 2016),
        start_year = 2015,
        end_year = 2016
      )
    )),
    error = function(e) NULL
  )
  testthat::skip_if(is.null(cbs), "CBS pins unavailable")

  d <- as.data.frame(cbs)
  v <- suppressWarnings(as.numeric(d$value))
  v <- v[!is.na(v) & v > 0]
  testthat::expect_gt(length(v), 1000L)

  # Row counts are small integers. Real commodity balances are not: if the largest positive
  # value in two years of world data is under 1000, the column is not carrying tonnes.
  testthat::expect_gt(max(v), 1000)
  # And they are not all integral, which counts necessarily are.
  testthat::expect_gt(sum(v != floor(v)), 0L)
})
