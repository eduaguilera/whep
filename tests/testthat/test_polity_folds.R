# test_polity_folds.R -- tests for R/polity_folds.R
#
# `polity_area_code` is an aggregation bucket, not an identity. FABIO folds
# FAOSTAT areas 276 Sudan and 277 South Sudan into bucket 206, so a post-2011
# bucket-206 value covers both territories while the polity resolved from the
# bucket code covers one of them (whep#414). Measured on real FAOSTAT
# production for 2015, bucket 206 carries Sudan 53,124,088 t and South Sudan
# 14,876,146 t -- South Sudan is 21.9% of the bucket -- and the reporting
# columns label it `SUD-1956-2011`, a polity that had ended by then, with the
# `out_of_span` status dropped on the way out.
#
# These tests pin that the mismatch is now reported and warned about, and that
# a fold whose bucket polity IS an aggregate is not falsely flagged.

# A crosswalk fixture exercising all four outcomes in one pass. `polity_type`
# aggregate on bucket 900's own label makes that fold honest; bucket 950 has no
# row for its own code; bucket 960 folds two national polities; bucket 970 folds
# nothing and must not be reported at all.
.fold_fixture_crosswalk <- function() {
  tibble::tribble(
    ~area_code, ~polity_area_code, ~polity_code,    ~polity_name, ~polity_type,
           900L,              900L, "AGG-2000-2025", "Aggregate", "aggregate",
           901L,              900L, "X-2000-2025",   "X",         "national",
           902L,              900L, "Y-2000-2025",   "Y",         "national",
           951L,              950L, "P-2000-2025",   "P",         "national",
           952L,              950L, "Q-2000-2025",   "Q",         "national",
           960L,              960L, "R-2000-2025",   "R",         "national",
           961L,              960L, "S-2000-2025",   "S",         "national",
           970L,              970L, "T-2000-2025",   "T",         "national"
  ) |>
    dplyr::mutate(
      area_name = .data$polity_name,
      area_iso3c = NA_character_,
      polity_start_year = 2000L,
      polity_end_year = 2025L,
      mapping_status = "matched",
      has_geometry = TRUE
    )
}

.local_fold_crosswalk <- function(env = parent.frame()) {
  fixture <- data.table::as.data.table(.fold_fixture_crosswalk())
  testthat::local_mocked_bindings(
    .polity_crosswalk = function(include_unmapped = TRUE) {
      data.table::copy(fixture)
    },
    .env = env
  )
}

test_that("bucket 206 is reported as partially covered after the secession", {
  # Post-2011 years only: PR #480 changes how the three Sudan areas resolve
  # BEFORE 2011, and this assertion must survive that. From 2011 the bucket
  # folds both successors whatever #480 does to the earlier years.
  coverage <- polity_bucket_coverage(years = c(2012L, 2015L, 2020L))
  sudan <- coverage[coverage$polity_area_code == 206L, ]

  expect_equal(nrow(sudan), 3L)
  expect_true(all(sudan$coverage == "partial"))
  expect_true(all(grepl("SDN-2011-2025", sudan$member_polity_codes)))
  expect_true(all(grepl("SSD-2011-2025", sudan$member_polity_codes)))
  # The label names one territory (or, before #480 lands, the ended unified
  # state) while the value covers both successors.
  expect_false(any(sudan$bucket_polity_code %in% c(NA_character_)))
  expect_true(all(sudan$n_member_polities > 1L))
})

test_that("bucket 206 is the only partially covered bucket", {
  # An invariant rather than a row count: FABIO's rest-of-world bucket 999 folds
  # many areas but resolves to `ROW-1850-2025`, an aggregate polity that means
  # the union, so it is honest and must not be flagged. If any other bucket ever
  # starts folding several territories under a single-territory polity, this is
  # where it shows up instead of shipping.
  partial <- polity_bucket_coverage()
  partial <- partial[partial$coverage == "partial", ]

  expect_gt(nrow(partial), 0L)
  expect_equal(unique(partial$polity_area_code), 206L)
})

test_that("an aggregate-labelled fold is not flagged, an unlabelled one is", {
  .local_fold_crosswalk()
  coverage <- polity_bucket_coverage(years = 2015L)

  expect_equal(coverage$polity_area_code, c(900L, 950L, 960L))
  expect_equal(coverage$coverage, c("aggregate", "unlabelled", "partial"))
  expect_equal(coverage$bucket_polity_code[[1]], "AGG-2000-2025")
  expect_true(is.na(coverage$bucket_polity_code[[2]]))
  expect_equal(coverage$bucket_polity_code[[3]], "R-2000-2025")
  # Bucket 970 folds a single polity, so the question does not arise for it.
  expect_false(970L %in% coverage$polity_area_code)
})

test_that("polity_bucket_coverage validates its years argument", {
  expect_error(polity_bucket_coverage(years = NA_integer_), "non-missing year")
})

test_that("the fold warning fires only for a partially covered bucket", {
  .local_fold_crosswalk()
  partial <- data.table::data.table(
    polity_area_code = c(960L, 960L),
    year = c(2015L, 2015L)
  )
  honest <- data.table::data.table(
    polity_area_code = c(900L, 970L),
    year = c(2015L, 2015L)
  )

  expect_warning(
    whep:::.warn_partial_bucket_polities(partial),
    "sums more than one territory"
  )
  expect_no_warning(whep:::.warn_partial_bucket_polities(honest))

  withr::local_options(whep.warn_polity_folds = FALSE)
  expect_no_warning(whep:::.warn_partial_bucket_polities(partial))
})

test_that(".aggregate_to_polities warns when it folds Sudan into bucket 206", {
  # The real shape, with real 2015 sorghum tonnages: two reporting areas landing
  # on one bucket code, which every downstream key (`key_cols` in build_cbs.R
  # deliberately excludes the area name) then sums into a single value.
  sudan <- data.table::data.table(
    area_code = c(276L, 277L),
    year = c(2015L, 2015L),
    element = c("production", "production"),
    unit = c("t", "t"),
    item_prod_code = c("83", "83"),
    item_prod = c("Sorghum", "Sorghum"),
    value = c(2744000, 661356)
  )

  expect_warning(
    folded <- whep:::.aggregate_to_polities(
      data.table::copy(sudan),
      item_prod_code,
      item_prod
    ),
    "Bucket 206"
  )

  # The bucket SUMS. Until whep#563 this came back as two rows under one code,
  # 2,744,000 and 661,356, because the aggregator also grouped by the member's
  # `polity_name` -- so the fold the warning describes was not actually being
  # performed, and every consumer keyed on `(area_code, year, item, element)`
  # saw a duplicated key.
  expect_equal(nrow(folded), 1L)
  expect_equal(folded$area_code, 206L)
  expect_equal(folded$value, 3405356)

  withr::local_options(whep.warn_polity_folds = FALSE)
  expect_no_warning(
    whep:::.aggregate_to_polities(
      data.table::copy(sudan),
      item_prod_code,
      item_prod
    )
  )
})

# The FABIO Rest-of-World fold, pinned at the level that actually decides where
# a number lands.
#
# `test_polity_faostat_map.R` pins that the fold exists in the crosswalk. These
# tests pin what it DOES: that the numeric key really sums a reporting area's
# observed rows into bucket 999, that both tables state the fold identically so
# a promotion cannot survive in one of them, and that a build now says so out
# loud instead of reporting a clean match. See #419.

testthat::test_that("folded_reporting_areas() names every fold and its kind", {
  # Scoped to the explicit fold, because it is no longer the default: WHEP
  # now models the reporting members in their own right (#459). What this
  # pins is the fold ITSELF -- its membership and its kinds -- which still
  # has to work, because reproducing a published-before number depends on it.
  withr::local_options(whep.unfold_rest_of_world = "none")
  folded <- whep::folded_reporting_areas()

  testthat::expect_s3_class(folded, "tbl_df")
  testthat::expect_setequal(
    names(folded),
    c(
      "area_code",
      "area_name",
      "area_iso3c",
      "polity_area_code",
      "polity_code",
      "polity_name",
      "fold_kind"
    )
  )
  testthat::expect_setequal(
    unique(folded$fold_kind),
    c("fabio_rest_of_world", "cbs_reporter_folded", "successor_state")
  )

  # Every fold, by definition: the bucket is not the area's own code.
  testthat::expect_true(all(folded$area_code != folded$polity_area_code))

  # The successor folds are the three deliberate territorial identities. They
  # are listed BECAUSE they are folds, not because they are wrong: FAOSTAT area
  # 62 "Ethiopia PDR" is pre-1993 Ethiopia, and 276/277 are the two halves of
  # 206 "Sudan (former)".
  successor <- folded[folded$fold_kind == "successor_state", ]
  testthat::expect_setequal(unique(successor$area_code), c(62L, 276L, 277L))
  testthat::expect_setequal(unique(successor$polity_area_code), c(238L, 206L))

  # Everything else lands on one bucket and one polity, and splits in two.
  row_fold <- folded[folded$fold_kind != "successor_state", ]
  testthat::expect_equal(unique(row_fold$polity_area_code), 999L)
  testthat::expect_equal(unique(row_fold$polity_code), "ROW-1850-2025")
  testthat::expect_equal(length(unique(row_fold$area_code)), 61L)
})

testthat::test_that("the four CBS reporters folded into 999 are named (#556)", {
  # Scoped to the explicit fold, because it is no longer the default: WHEP
  # now models the reporting members in their own right (#459). What this
  # pins is the fold ITSELF -- its membership and its kinds -- which still
  # has to work, because reproducing a published-before number depends on it.
  withr::local_options(whep.unfold_rest_of_world = "none")
  # `"fabio_rest_of_world"` claimed something about FABIO that is false for four
  # of the 61 members. FABIO's published region list -- `io_codes.csv` of the
  # v1.1 release (Zenodo record 2577067), 192 areas x 125 commodities -- gives
  # each of these four its own block, distinct from area 999 `RoW`, and the
  # FABIO source repository marks all four `current == TRUE`, the flag its
  # `replace_RoW()` keeps out of bucket 999. So this fold is WHEP's own, and
  # `regions_full` says as much itself by flagging them `cbs`.
  folded <- whep::folded_reporting_areas()
  contradictory <- folded[folded$fold_kind == "cbs_reporter_folded", ]

  testthat::expect_setequal(
    unique(contradictory$area_code),
    c(153L, 154L, 209L, 212L)
  )
  testthat::expect_equal(unique(contradictory$polity_area_code), 999L)
  testthat::expect_equal(unique(contradictory$polity_code), "ROW-1850-2025")

  # The other 57 are folds FABIO also makes, and none of them is a reporter.
  agreed <- folded[folded$fold_kind == "fabio_rest_of_world", ]
  testthat::expect_equal(length(unique(agreed$area_code)), 57L)
  testthat::expect_length(
    intersect(agreed$area_code, contradictory$area_code),
    0L
  )
})

testthat::test_that("folded_reporting_areas() needs the cbs flag", {
  cw <- as.data.frame(whep::polity_area_crosswalk)
  cw$cbs <- NULL

  testthat::expect_error(whep::folded_reporting_areas(cw), "cbs")
})

testthat::test_that("the areas that report real data of their own are folded", {
  # Scoped to the explicit fold, because it is no longer the default: WHEP
  # now models the reporting members in their own right (#459). What this
  # pins is the fold ITSELF -- its membership and its kinds -- which still
  # has to work, because reproducing a published-before number depends on it.
  withr::local_options(whep.unfold_rest_of_world = "none")
  # Measured against the raw FAOSTAT pins at the base commit: these are the
  # reporting areas FABIO folds into Rest of World that carry observed rows,
  # with their `faostat-production` row counts. The fold is left standing on
  # purpose (#419); what must not happen is it being invisible.
  reporting <- tibble::tribble(
    ~area_code, ~area_name,             ~production_rows,
    212L,       "Syrian Arab Republic", 24426L,
    209L,       "Eswatini",             12196L,
    182L,       "Reunion",              11970L,
    87L,        "Guadeloupe",           10639L,
    154L,       "North Macedonia",      10365L,
    153L,       "New Caledonia",        9857L,
    299L,       "Palestine",            9606L,
    47L,        "Cook Islands",         8717L,
    135L,       "Martinique",           8435L,
    69L,        "French Guiana",        7870L,
    61L,        "Equatorial Guinea",    7274L,
    160L,       "Niue",                 6290L,
    64L,        "Faroe Islands",        2458L
  )

  folded <- whep::folded_reporting_areas()
  testthat::expect_true(all(reporting$area_code %in% folded$area_code))

  # And they resolve to Rest of World rather than to their own polity, which is
  # why no coverage count can see the misattribution: the fold IS a match.
  resolved <- tibble::tibble(
    area_code = reporting$area_code,
    year = 2010L
  ) |>
    whep::add_polity_code()
  testthat::expect_equal(unique(resolved$polity_area_code), 999L)
  testthat::expect_equal(unique(resolved$polity_code), "ROW-1850-2025")
  testthat::expect_equal(unique(resolved$mapping_status), "matched")
})

testthat::test_that("regions_full and the crosswalk state the fold alike", {
  # Scoped to the explicit fold, because it is no longer the default: WHEP
  # now models the reporting members in their own right (#459). What this
  # pins is the fold ITSELF -- its membership and its kinds -- which still
  # has to work, because reproducing a published-before number depends on it.
  withr::local_options(whep.unfold_rest_of_world = "none")
  # The promotion this pins against survived one round of withdrawal by being
  # written down twice and only one table being rebuilt. `regions_full` feeds
  # `.iso3c_area_code_lookup()` and the crosswalk feeds every polity join, so
  # they must agree on the numeric key for every area they share.
  cw <- as.data.frame(whep::polity_area_crosswalk)
  rf <- as.data.frame(whep::regions_full)

  from_cw <- unique(data.frame(
    area_code = cw$area_code,
    cw_key = cw$polity_area_code
  ))
  from_cw <- from_cw[!is.na(from_cw$area_code), ]
  from_rf <- unique(data.frame(
    area_code = as.integer(rf$code),
    rf_key = as.integer(rf$polity_area_code)
  ))
  from_rf <- from_rf[!is.na(from_rf$area_code), ]

  both <- merge(from_cw, from_rf, by = "area_code")
  differs <- both[
    is.na(both$cw_key) != is.na(both$rf_key) |
      (!is.na(both$cw_key) & !is.na(both$rf_key) & both$cw_key != both$rf_key),
  ]

  # ONE documented exception. FAOSTAT area 351 "China" is the aggregate of
  # 41/96/128/214, reported alongside them for every year, so `regions_full`
  # leaves its key NA to drop it while the crosswalk carries 351 with no polity
  # at all -- which drops it just the same, one step later.
  testthat::expect_equal(differs$area_code, 351L)
  testthat::expect_true(is.na(differs$rf_key))
  testthat::expect_true(all(is.na(cw$polity_code[cw$area_code == 351L])))
})

testthat::test_that(".aggregate_to_polities() sums a fold and reports it", {
  # Scoped to the explicit fold: Syria is a standalone area by default now
  # (#459), so there is no fold here to sum unless one is asked for. The
  # summing behaviour itself still has to be right, because bucket 206 folds
  # regardless of this option and the same code path serves both.
  withr::local_options(whep.unfold_rest_of_world = "none")
  # The whole defect in one fixture: Syria's row keeps its value but comes back
  # under area 999, added to Rest of World's own row. Nothing is dropped, so the
  # only way a build can know is the warning.
  raw <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~element,     ~unit,    ~value,
    2010L, 212L,       2511,           "production", "tonnes", 300,
    2010L, 999L,       2511,           "production", "tonnes", 40,
    2010L, 40L,        2511,           "production", "tonnes", 7
  )

  testthat::expect_warning(
    whep:::.aggregate_to_polities(
      data.table::as.data.table(raw),
      item_cbs_code,
      source_label = "a-test-fixture"
    ),
    "folded into another"
  )
  out <- suppressWarnings(
    whep:::.aggregate_to_polities(
      data.table::as.data.table(raw),
      item_cbs_code,
      source_label = "a-test-fixture"
    )
  )

  totals <- stats::setNames(out$value, as.character(out$area_code))
  testthat::expect_equal(unname(totals[["999"]]), 340)
  testthat::expect_equal(unname(totals[["40"]]), 7)
  testthat::expect_false("212" %in% names(totals))
})

testthat::test_that("the fold warning names the area, bucket and row count", {
  folded <- data.table::data.table(
    area_code = c(212L, 212L, 40L),
    area_name = c("Syrian Arab Republic", "Syrian Arab Republic", "Cuba"),
    polity_area_code = c(999L, 999L, 40L)
  )

  testthat::expect_warning(
    whep:::.warn_folded_areas(folded, source_label = "a-test-fixture"),
    "Syrian Arab Republic \\(212\\) -> 999 \\(n = 2\\)"
  )
  testthat::expect_warning(
    whep:::.warn_folded_areas(folded, source_label = "a-test-fixture"),
    "a-test-fixture"
  )
})

testthat::test_that("nothing is reported when nothing is folded", {
  unfolded <- data.table::data.table(
    area_code = c(40L, 41L),
    area_name = c("Cuba", "China (PRC)"),
    polity_area_code = c(40L, 41L)
  )
  testthat::expect_silent(whep:::.warn_folded_areas(unfolded))
  testthat::expect_equal(nrow(whep:::.warn_folded_areas(unfolded)), 0L)

  # A frame with no polity columns at all is not an occasion to abort.
  testthat::expect_silent(
    whep:::.warn_folded_areas(data.table::data.table(x = 1))
  )
})

testthat::test_that("the un-fold is the default and the fold is now opt-in", {
  # THIS TEST ASSERTED THE OPPOSITE until the fold stopped being WHEP's country
  # set. FABIO's 192-country layout is a methodology this package compares
  # against, not a constraint on which territories it models: WHEP decides that
  # for itself, and 21 of the folded areas file their own FAOSTAT returns.
  #
  # So the committed crosswalk is no longer the published shape -- it is the raw
  # input, and `.polity_crosswalk()` promotes on read. Syria is the clearest
  # case: it reports its own production and was being published as
  # "Rest of World".
  cw <- as.data.frame(whep:::.polity_crosswalk())
  testthat::expect_equal(whep:::.iso3c_to_area_code("SYR"), 212L)
  testthat::expect_equal(whep:::.unfold_rest_of_world_mode(), "all")

  # Only bucket 999 itself still carries 999: every member has been promoted.
  still_folded <- unique(cw$area_code[which(cw$polity_area_code == 999L)])
  testthat::expect_equal(still_folded, 999L)

  # The bucket survives, and that matters -- it is a genuine residual for the
  # territories that report nothing, not an empty shell. Of the 61 members only
  # about a third ever report, so promotion is self-limiting: an area with no
  # rows contributes none either way.
  testthat::expect_true(999L %in% cw$area_code)

  # And the fold is still reachable, because reproducing a published-before
  # number has to stay possible.
  refolded <- as.data.frame(
    withr::with_options(
      list(whep.unfold_rest_of_world = "none"),
      suppressWarnings(whep:::.polity_crosswalk())
    )
  )
  testthat::expect_equal(
    refolded$polity_area_code,
    as.data.frame(whep::polity_area_crosswalk)$polity_area_code
  )
})

testthat::test_that("promotion reaches the whole pipeline, silently by default", {
  testthat::skip_if_not_installed("withr")
  # This asserted a WARNING when promotion happened, because the fold was what
  # WHEP published. Promotion is now the published shape (#459), so warning on it
  # would fire on every read of every build. The warning moved to the opposite
  # case -- re-folding -- and is asserted below.
  withr::local_options(whep.unfold_rest_of_world = "all")

  testthat::expect_equal(whep:::.unfold_rest_of_world_mode(), "all")
  testthat::expect_no_warning(whep:::.polity_crosswalk())
  cw <- as.data.frame(suppressWarnings(whep:::.polity_crosswalk()))
  members <- suppressWarnings(whep::folded_reporting_areas(
    as.data.frame(whep::polity_area_crosswalk)
  ))
  members <- unique(
    members$area_code[members$fold_kind != "successor_state"]
  )
  promoted <- cw[!is.na(cw$area_code) & cw$area_code %in% members, ]
  testthat::expect_true(all(
    promoted$polity_area_code == promoted$area_code
  ))

  # Both tables must move together, or the two lookups disagree about where a
  # member's rows belong -- the exact failure mode #419 records.
  testthat::expect_equal(whep:::.iso3c_to_area_code("SYR"), 212L)
  testthat::expect_equal(whep:::.iso3c_to_area_code("FRO"), 64L)

  # And the successor folds are NOT lifted: they are identities, not a FABIO
  # convention.
  still_folded <- suppressWarnings(whep::folded_reporting_areas())
  testthat::expect_setequal(
    unique(still_folded$fold_kind),
    "successor_state"
  )
})

testthat::test_that("the unfold switch can lift only the CBS reporters", {
  # The narrower experiment #556 asks for: lift exactly the four folds FABIO
  # does not make, and leave the 57 it does make standing.
  testthat::skip_if_not_installed("withr")
  withr::local_options(whep.unfold_rest_of_world = "cbs_reporters")

  # It warns because it is NARROWER than the default: 57 areas that WHEP now
  # models in their own right get re-folded, so the run does not match the
  # published series and has to say so.
  testthat::expect_equal(whep:::.unfold_rest_of_world_mode(), "cbs_reporters")
  testthat::expect_warning(
    whep:::.polity_crosswalk(),
    "Rest-of-World fold is being applied"
  )
  cw <- as.data.frame(suppressWarnings(whep:::.polity_crosswalk()))
  reporters <- c(153L, 154L, 209L, 212L)
  lifted <- cw[!is.na(cw$area_code) & cw$area_code %in% reporters, ]
  testthat::expect_true(all(lifted$polity_area_code == lifted$area_code))

  # Both tables move together, or the two lookups disagree (#419).
  testthat::expect_equal(whep:::.iso3c_to_area_code("SYR"), 212L)
  # A non-reporter FABIO also folds stays in the bucket, unlike under `"all"`.
  testthat::expect_equal(whep:::.iso3c_to_area_code("FRO"), 999L)

  still_folded <- suppressWarnings(whep::folded_reporting_areas())
  testthat::expect_setequal(
    unique(still_folded$fold_kind),
    c("fabio_rest_of_world", "successor_state")
  )
  testthat::expect_equal(
    length(unique(
      still_folded$area_code[still_folded$fold_kind == "fabio_rest_of_world"]
    )),
    57L
  )
})

testthat::test_that("an unrecognised unfold mode aborts instead of folding", {
  testthat::skip_if_not_installed("withr")
  withr::local_options(whep.unfold_rest_of_world = "reporters")

  testthat::expect_error(
    whep:::.unfold_rest_of_world_mode(),
    "cbs_reporters"
  )
})

testthat::test_that("folded_reporting_areas() rejects a frame it cannot read", {
  testthat::expect_error(
    whep::folded_reporting_areas(tibble::tibble(area_code = 1L)),
    "missing"
  )
})

# The bucket label, which is what whep#563 is about ---------------------------
#
# One `area_code` must come out under one `area`, whatever the members resolve
# to, because `area` is a join key and a duplicated `(area_code, year, item,
# element)` is what fed the `dcast()` `length()` fallback in whep#425.

.bucket_fixture_rows <- function() {
  data.table::data.table(
    area_code = c(900L, 901L, 902L, 951L, 952L, 960L, 961L, 970L),
    year = 2015L,
    element = "production",
    unit = "t",
    item_prod_code = "83",
    value = c(1, 2, 4, 8, 16, 32, 64, 128)
  )
}

testthat::test_that("every bucket gets exactly one label, whatever it folds", {
  .local_fold_crosswalk()
  out <- suppressWarnings(
    whep:::.aggregate_to_polities(.bucket_fixture_rows(), item_prod_code)
  )

  # Four buckets, four rows, four labels -- not eight rows under four codes.
  testthat::expect_equal(nrow(out), 4L)
  labels <- stats::setNames(out$area, as.character(out$area_code))
  testthat::expect_equal(
    labels[c("900", "950", "960", "970")],
    c(`900` = "Aggregate", `950` = "P", `960` = "R", `970` = "T")
  )

  # Bucket 900 is the shape whep#480 shipped: a member (901 "X", 902 "Y") with
  # its own polity while `polity_area_code` stays on the aggregate. It sums.
  totals <- stats::setNames(out$value, as.character(out$area_code))
  testthat::expect_equal(unname(totals[["900"]]), 7)
  testthat::expect_equal(unname(totals[["960"]]), 96)
  testthat::expect_equal(sum(out$value), sum(.bucket_fixture_rows()$value))
})

testthat::test_that("a bucket with no row of its own falls back to a member", {
  # Bucket 950 has no crosswalk row for its own code, so it cannot be labelled
  # from the bucket. The member label is then used -- deterministically, the
  # lowest `area_code`, so it does not depend on which member reported or on
  # row order -- rather than leaving the rows unlabelled.
  .local_fold_crosswalk()
  rows <- .bucket_fixture_rows()[area_code %in% c(951L, 952L)]

  out <- suppressWarnings(
    whep:::.aggregate_to_polities(data.table::copy(rows), item_prod_code)
  )
  testthat::expect_equal(out$area, "P")

  reversed <- suppressWarnings(
    whep:::.aggregate_to_polities(
      data.table::copy(rows)[order(-area_code)],
      item_prod_code
    )
  )
  testthat::expect_equal(reversed$area, "P")
  testthat::expect_false(any(is.na(out$area)))
})

testthat::test_that("labelling annotates the aggregate, it cannot filter it", {
  # `.apply_bucket_area_labels()` is an update-join precisely so that a missing
  # label costs a label and never a row -- the 702,166-row drop in whep#382 came
  # from a labelling change meeting an inner join.
  agg <- data.table::data.table(
    year = c(2015L, 2015L),
    polity_area_code = c(900L, 111L),
    value = c(7, 5)
  )
  labels <- data.table::data.table(
    polity_area_code = 900L,
    year = 2015L,
    area = "Aggregate"
  )

  out <- whep:::.apply_bucket_area_labels(agg, labels)
  testthat::expect_equal(nrow(out), 2L)
  testthat::expect_equal(names(out)[1:3], c("year", "area_code", "area"))
  testthat::expect_equal(out$area, c("Aggregate", NA_character_))
})
