# Folded reporting areas must not be summed HERE. They must be summed at the cast.
#
# Where the bucket's total is computed is the whole subject of this file, and it has moved
# twice, so the history is kept deliberately.
#
# `.aggregate_to_polities()` aggregates to `polity_area_code` — the reporting bucket the
# polities database defines — grouping by `polity_name` alongside the code. Several FAOSTAT
# areas can fold into one bucket, and when they carry different names the bucket holds several
# rows. That LOOKS like a defect: at 1990-2023 it produces 1,525 duplicate
# `(area_code, year, item_cbs, item_cbs_code, element, source)` keys, all in bucket 206, which
# folds FAOSTAT 206 "Sudan (former)", 276 "Sudan" and 277 "South Sudan".
#
# Dropping `polity_name` from the grouping is still WRONG, but the reason is narrower than an
# earlier version of this comment claimed, and the difference is worth stating.
#
# What is certain: `area` is a JOIN KEY (see the last test in this file), and the residues path
# emits the polity name, so the aggregator must too. Keeping the periods apart is also what
# `main` does, and the bucket's total is computed later, at the cast in `.select_best_source()`,
# where the rows are unambiguously one bucket's members.
#
# WHAT THIS TEST PINS IS THE STATUS QUO, NOT AN IDEAL, and whep#425 proposes changing it.
#
# I claimed that summing HERE double-counts, on the strength of measuring `food` inflate 266x
# when I tried it. That change bundled TWO things -- dropping `polity_name` from the grouping
# AND relabelling the output with the bucket's stable `area_name` -- and the relabelling alone
# dropped 702,166 rows by creating two vocabularies for a join key. Isolated properly, with the
# label vocabulary left untouched, summing here is not harmful: it removes the cast's 10,835
# duplicate keys entirely, and its full-range totals match fixing the cast to within a few
# percent (`food` 1.0003, `seed` 1.0000, rows 1.0005).
#
# So summing upstream was one of TWO candidate fixes for whep#425, not a mistake. #425 was
# RESOLVED on the cast side instead (whep#429), which is the equivalent of the two and touches
# one expression rather than an aggregation grouping. This test still asserts what the package
# does -- periods kept apart, the bucket totalled at the cast -- and that is now the shipped
# design rather than a pin awaiting a decision.
#
# Isolating it takes care: `.aggregate_to_polities()` has two aggregation branches, flagged and
# flagless, and every FAOSTAT pin carries a Flag. A probe patching only the flagless branch
# reports "no change" because it makes none -- I published that non-result once. Confirm a probe
# ran by checking the duplicate warning disappears.
#
# Row counts barely moved (2.16M vs 2.81M) while values exploded, which is why 5151 passing
# tests said nothing: no test compared a magnitude with anything.
#
# The downstream trap that made this delicate is now CLOSED. `.select_best_source()` used to
# cast these rows wide with no `fun.aggregate`, so data.table fell back to `length()` and every
# value -- not only the duplicated ones -- became a ROW COUNT: all three primary source columns
# came back integer with maxima of 4, 4 and 1 where tonnes belong, at a cost of up to 259x on
# `food`. whep#429 supplies `sum`, because the duplicated rows are one bucket's folded members.
#
# What that means for THIS test: the duplicates it deliberately creates are now summed at the
# cast rather than corrupted there, so the arithmetic downstream of the grouping is sound and
# the grouping is the only thing under test. The warning that used to flag the corruption was
# repurposed -- it now fires only for duplicates in a bucket that folds no other area -- and the
# last test in this file asserts both halves of that.

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

  # Two distinct territory-periods, two rows. Collapsing them here would also collapse the
  # `area` label they carry, which four inner joins key on -- and the bucket's total belongs at
  # the cast, not here (whep#425).
  expect_equal(nrow(out), 2L)
  expect_setequal(out$value, c(10, 32))
  expect_true(all(out$area_code == pair$bucket))
  # Both rows still carry a label, so downstream joins on `area` do not see NA.
  expect_true(all(!is.na(out$area) & nzchar(out$area)))
  # And the labels differ, which is what keeps them apart.
  expect_equal(length(unique(out$area)), 2L)
})

test_that("the cast SUMS duplicate keys and stays silent when folding explains them", {
  # REWRITTEN for whep#429. This test used to assert that the cast warned about duplicates
  # becoming "row counts". That was the correct assertion while whep#425 was open: `dcast()`
  # with no `fun.aggregate` fell back to `length()`. The cast now supplies `fun.aggregate` and
  # sums them, so the old message would be a false alarm and the guard was repurposed -- it
  # fires only for duplicates in a bucket that folds no other FAOSTAT area.
  #
  # 206 folds three areas (206, 276, 277), so this is the explained case: sum, no warning.
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
  out <- withCallingHandlers(
    whep:::.select_best_source(dupe),
    warning = function(w) {
      msg <<- c(msg, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_length(msg, 0L)
  # 42, not 2. The row count is what the old behaviour returned here.
  expect_equal(as.data.frame(out)$value, 42)

  # The unexplained case still warns. Area 100 (India) maps one-to-one, so a duplicate there is
  # not accounted for by folding and summing it is a guess.
  dupe$area_code <- 100L
  msg2 <- character()
  withCallingHandlers(
    whep:::.select_best_source(dupe),
    warning = function(w) {
      msg2 <<- c(msg2, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_true(length(msg2) > 0L)
  expect_match(paste(msg2, collapse = " "), "fold no other", fixed = TRUE)
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
