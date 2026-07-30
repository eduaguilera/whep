# Folded reporting areas must SUM into their bucket, not sit beside each other in it.
#
# `.aggregate_to_polities()` aggregates to `polity_area_code`, the reporting bucket the
# polities database defines, and then renames it to `area_code`. It used to group by
# `polity_name` alongside the code. Several FAOSTAT areas can fold into one bucket, and when
# they carry different names the label split the bucket, so the folded areas were never
# summed and the output held two rows for one key.
#
# Measured at 1990-2023 before the fix: 1,525 duplicate
# `(area_code, year, item_cbs, item_cbs_code, element, source)` keys, every one in bucket 206
# — which folds FAOSTAT 206 "Sudan (former)", 276 "Sudan" and 277 "South Sudan". From 2014
# Sudan and South Sudan both report, so the split fires from 2014 to 2023.
#
# The downstream cost was worse than the duplicate. `.select_best_source()` casts these rows
# wide with no `fun.aggregate`, so data.table falls back to `length()` and the values become
# ROW COUNTS. Whether anyone notices is luck: counts are integer, and `FBS_Old_scaled` is
# double only when `scale_new_old` applies, which depends on the build window. At 1990-2023
# the build died in `fcoalesce` with a type clash; over the full range it completed, with
# counts standing in for quantities. The crash was the good case.
#
# Pre-existing, and identical on main: both failed at the same window with the same 1,525
# keys. Fixed here because it is the same inconsistency this work is about — the bucket is
# the polity-derived key, and summing into it is what the bucket means.

test_that("two areas folding into one bucket produce one summed row", {
  cw <- as.data.frame(whep::polity_area_crosswalk)
  folded <- unique(cw[
    !is.na(cw$polity_area_code) & !is.na(cw$area_code),
    c("area_code", "polity_area_code")
  ])
  # Find a bucket that genuinely folds more than one reporting area, rather than assuming
  # 206 does: if upstream ever unfolds Sudan this test must still exercise the behaviour.
  # The pair must be two areas whose polities have DIFFERENT names in the test year, which
  # is the condition that makes the old grouping split the bucket. Picking merely "two areas
  # in a folding bucket" is not enough, and this test passed against the unfixed code until
  # that was fixed: bucket 206 folds areas 206, 276 and 277, and the two lowest codes (206
  # and 276) both resolve to the polity "Sudan", so nothing split. The split needs 277,
  # "South Sudan". Searching for the condition also means the test survives upstream
  # renaming or unfolding Sudan — it will find another such bucket, or skip.
  named <- function(codes) {
    probe <- data.table::data.table(
      year = 2015L,
      area_code = as.integer(codes),
      unit = "tonnes",
      element = "production",
      item_cbs_code = 15L,
      value = 1
    )
    out <- whep:::.add_polity_columns_dt(
      probe,
      code_col = "area_code",
      year_col = "year",
      include_unmapped = FALSE
    )
    out[!is.na(out$polity_code), c("area_code", "polity_name")]
  }

  pair <- NULL
  for (b in sort(unique(folded$polity_area_code))) {
    codes <- unique(folded$area_code[folded$polity_area_code == b])
    if (length(codes) < 2L) {
      next
    }
    nm <- named(codes)
    keep <- nm[!duplicated(nm$polity_name), ]
    if (nrow(keep) > 1L) {
      pair <- list(bucket = as.integer(b), areas = head(keep$area_code, 2L))
      break
    }
  }
  testthat::skip_if(
    is.null(pair),
    "no bucket folds areas whose polities are differently named"
  )
  bucket <- pair$bucket
  areas <- pair$areas

  input <- data.table::data.table(
    year = 2015L,
    area_code = as.integer(areas),
    unit = "tonnes",
    element = "production",
    item_cbs_code = 15L,
    value = c(10, 32)
  )
  out <- whep:::.aggregate_to_polities(input, item_cbs_code)

  # One row per (year, bucket, unit, element, item) — not one per folded area.
  expect_equal(nrow(out), 1L)
  expect_equal(out$area_code, bucket)
  # The bucket's value is the sum of what folded into it.
  expect_equal(out$value, 42)
  # And it still carries a name, so downstream labels are not NA.
  expect_true(!is.na(out$area) && nzchar(out$area))
})

test_that("the cast refuses duplicate keys instead of counting them", {
  # The other half of the fix. Fixing the two aggregations removes the duplicates that exist
  # today; this guard removes the TRAP, because `dcast()` without `fun.aggregate` answers a
  # duplicate key with `length()` and the value silently becomes a row count. It has to name
  # what it found, or a future occurrence is just as hard to diagnose as this one was.
  dupe <- data.table::data.table(
    area_code = 206L,
    area = "Sudan",
    year = 2015L,
    item_cbs = "Honey",
    item_cbs_code = 2745L,
    element = "production",
    source = "FAOSTAT_FBS_New",
    value = c(10, 32)
  )
  err <- tryCatch(
    whep:::.select_best_source(dupe),
    error = function(e) conditionMessage(e)
  )
  expect_true(is.character(err))
  expect_match(err, "row counts")
  expect_match(err, "206")
})

test_that("areas in different buckets stay separate", {
  # The complement: grouping by the key must not collapse areas that belong to different
  # buckets. Armenia (1) and Afghanistan (2) each keep their own reporting code.
  input <- data.table::data.table(
    year = 2015L,
    area_code = c(1L, 2L),
    unit = "tonnes",
    element = "production",
    item_cbs_code = 15L,
    value = c(10, 32)
  )
  out <- whep:::.aggregate_to_polities(input, item_cbs_code)
  expect_equal(nrow(out), 2L)
  expect_setequal(out$value, c(10, 32))
})
