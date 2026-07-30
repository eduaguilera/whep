# Folded reporting areas must NOT be summed together just because they share a bucket.
#
# This file asserted the opposite until the totals were measured, and the reversal is the
# point of keeping the history in the comment.
#
# `.aggregate_to_polities()` aggregates to `polity_area_code` — the reporting bucket the
# polities database defines — grouping by `polity_name` alongside the code. Several FAOSTAT
# areas can fold into one bucket, and when they carry different names the bucket holds several
# rows. That LOOKS like a defect: at 1990-2023 it produces 1,525 duplicate
# `(area_code, year, item_cbs, item_cbs_code, element, source)` keys, all in bucket 206, which
# folds FAOSTAT 206 "Sudan (former)", 276 "Sudan" and 277 "South Sudan".
#
# Dropping `polity_name` from the grouping to collapse them is WRONG. The name distinguishes
# territory-periods, and rows for a member and rows that already aggregate that member both
# land in the bucket, so summing double-counts. Measured on a full-range `get_wide_cbs()`
# against main:
#
#   food             266x    domestic_supply  1.9x    feed  12x    import  8.3x
#
# Row counts barely moved (2.16M vs 2.81M) while values exploded, which is why 5151 passing
# tests said nothing: no test compared a magnitude with anything.
#
# The downstream TRAP is real and stays guarded. `.select_best_source()` casts these rows wide
# with no `fun.aggregate`, so data.table falls back to `length()` and values silently become
# ROW COUNTS. Whether anyone notices is luck: counts are integer, and `FBS_Old_scaled` is
# double only when `scale_new_old` applies, which depends on the build window — at 1990-2023
# the build dies in `fcoalesce` with a type clash, over the full range it completes with counts
# standing in for quantities. The crash is the good case. main has the same duplicates, so the
# guard warns rather than aborts; choosing sum-vs-first at the cast changes published numbers
# and is whep#418.

test_that("territory-periods sharing a bucket are kept apart, not summed", {
  cw <- as.data.frame(whep::polity_area_crosswalk)
  folded <- unique(cw[
    !is.na(cw$polity_area_code) & !is.na(cw$area_code),
    c("area_code", "polity_area_code")
  ])
  # Find a bucket that genuinely folds two areas whose polities are DIFFERENTLY NAMED in the
  # test year, rather than assuming 206 does. Merely "two areas in a folding bucket" is not
  # enough: bucket 206 folds 206, 276 and 277, and the two lowest codes both resolve to the
  # polity "Sudan", so a pair drawn from those exercises nothing. Searching for the condition
  # also means the test survives upstream renaming or unfolding Sudan.
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

  input <- data.table::data.table(
    year = 2015L,
    area_code = as.integer(pair$areas),
    unit = "tonnes",
    element = "production",
    item_cbs_code = 15L,
    value = c(10, 32)
  )
  out <- whep:::.aggregate_to_polities(input, item_cbs_code)

  # Two distinct territory-periods, two rows. Collapsing them to one row of 42 is the
  # double-count that inflated `food` 266x.
  expect_equal(nrow(out), 2L)
  expect_setequal(out$value, c(10, 32))
  expect_true(all(out$area_code == pair$bucket))
  # Both rows still carry a label, so downstream joins on `area` do not see NA.
  expect_true(all(!is.na(out$area) & nzchar(out$area)))
  # And the labels differ, which is what keeps them apart.
  expect_equal(length(unique(out$area)), 2L)
})

test_that("the cast reports duplicate keys instead of silently counting them", {
  # A warning, not an abort: these duplicates exist on main too, so aborting refuses to build
  # a pipeline that has always had them. It must NAME what it found, or a recurrence is as
  # hard to diagnose as this one was.
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
  # `suppressWarnings()` INSIDE `withCallingHandlers()` muffles the warning before the handler
  # can see it, so the handler must do the muffling itself.
  msg <- character()
  withCallingHandlers(
    whep:::.select_best_source(dupe),
    warning = function(w) {
      msg <<- c(msg, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_true(length(msg) > 0L)
  joined <- paste(msg, collapse = " ")
  expect_match(joined, "row counts")
  expect_match(joined, "206")
})

test_that("areas in different buckets stay separate", {
  # The complement: grouping must not collapse areas that belong to different buckets.
  # Armenia (1) and Afghanistan (2) each keep their own reporting code.
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

test_that("the `area` label agrees with what the residues path emits", {
  # This is the invariant whose violation cost the longest hunt on this branch, so it is
  # asserted rather than assumed.
  #
  # `area` is a JOIN KEY, not decoration: read_raw_inputs.R (three joins) and
  # build_production.R join on `c("area", "area_code", ...)`, and `.read_crop_residues()`
  # labels its rows with the polity name. Relabelling the aggregator's output with the
  # bucket's own stable `area_name` — which reads like an improvement, since a bucket then has
  # one name in every year — created TWO vocabularies for one key. The joins are inner, so for
  # 19 of 45 affected areas the names simply failed to match and the rows vanished: 702,166 of
  # them, silently, across areas whose two spellings differ like "Albania" vs
  # "Albania (1913-2025)", "Cabo Verde" vs "Cape Verde (Portuguese colony)".
  #
  # So the aggregator must emit the POLITY name for the (area, year) it is aggregating, which
  # is what the residues path emits. Checked against `.add_polity_columns_dt()`, the same
  # resolver the residues path uses, rather than against a hand-written expectation.
  input <- data.table::data.table(
    year = c(2015L, 2015L),
    area_code = c(3L, 4L),
    unit = "tonnes",
    element = "production",
    item_cbs_code = 15L,
    value = 1
  )
  out <- whep:::.aggregate_to_polities(data.table::copy(input), item_cbs_code)

  resolved <- whep:::.add_polity_columns_dt(
    data.table::copy(input),
    code_col = "area_code",
    year_col = "year",
    include_unmapped = FALSE
  )
  resolved <- resolved[!is.na(resolved$polity_name), ]
  testthat::skip_if(nrow(resolved) == 0L, "probe areas resolve to no polity")

  # Every label the aggregator emits must be a polity name the resolver would produce for the
  # same rows — not a bucket `area_name` from the crosswalk.
  expect_true(all(out$area %in% resolved$polity_name))
})
