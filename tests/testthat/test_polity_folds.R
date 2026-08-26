# test_polity_folds.R -- tests for R/polity_folds.R
#
# `polity_area_code` is an aggregation bucket, not an identity. FABIO folds
# FAOSTAT areas 276 Sudan and 277 South Sudan into bucket 206, so from 2012 a
# bucket-206 value covers both territories (whep#414). Measured on real FAOSTAT
# production for 2015, bucket 206 carries Sudan 54,040,755 t and South Sudan
# 14,876,146 t -- South Sudan is 21.6% of the bucket -- and the reporting
# columns label it `SUD-1956-2011`, a polity that had ended by then.
#
# The label is right about the TERRITORY and wrong about the PERIOD:
# `SUD-1956-2011`'s published successors are exactly `SDN-2011-2025` and
# `SSD-2011-2025`, the two the bucket folds, so its extent is the sum. These
# tests pin that distinction, that the fold is reported for the 14 years it
# happens rather than all 65, and that a fold whose bucket polity IS an
# aggregate is not flagged at all.

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

test_that("bucket 206 carries its members' ended predecessor", {
  # Post-2011 years only: PR #480 changes how the three Sudan areas resolve
  # BEFORE 2011, and this assertion must survive that. From 2012 the bucket
  # folds both successors whatever #480 does to the earlier years.
  coverage <- polity_bucket_coverage(years = c(2012L, 2015L, 2020L))
  sudan <- coverage[coverage$polity_area_code == 206L, ]

  expect_equal(nrow(sudan), 3L)
  # NOT `"partial"`: the label is `SUD-1956-2011`, whose published successors
  # are exactly the two polities the bucket folds, so its territory IS the sum.
  # What is wrong is the period -- that polity had ended (whep#414).
  expect_true(all(sudan$coverage == "predecessor"))
  expect_equal(unique(sudan$bucket_polity_code), "SUD-1956-2011")
  expect_true(all(sudan$bucket_mapping_status == "out_of_span"))
  expect_equal(
    unique(sudan$member_polity_codes),
    "SDN-2011-2025, SSD-2011-2025"
  )
  expect_true(all(sudan$n_member_polities == 2L))
})

test_that("no bucket is labelled with a polity covering less than it sums", {
  # An invariant rather than a row count: FABIO's rest-of-world bucket 999 folds
  # many areas but resolves to `ROW-1850-2025`, an aggregate polity that means
  # the union, so it is honest and must not be flagged. If any bucket ever
  # starts folding several territories under a polity covering only some of
  # them, this is where it shows up instead of shipping.
  coverage <- polity_bucket_coverage()

  expect_equal(nrow(coverage[coverage$coverage == "partial", ]), 0L)
  expect_equal(unique(coverage$polity_area_code), 206L)
  expect_equal(unique(coverage$coverage), "predecessor")
})

test_that("a stand-in outside an area's reporting years is not a member", {
  # The fold is 2012-2025, not 1961-2025. FAOSTAT reports area 206 through 2011
  # and areas 276/277 from 2012, never in the same year, so before 2012 bucket
  # 206 sums exactly one territory. The year-aware resolver answers for every
  # (area_code, year) pair regardless, standing in with the nearest period, and
  # counting those stand-ins reported a three-way fold in all 65 years.
  coverage <- polity_bucket_coverage()

  expect_equal(nrow(coverage), 14L)
  expect_equal(min(coverage$year), 2012L)
  expect_false(any(coverage$year <= 2011L))
  expect_equal(unique(coverage$member_area_codes), "276, 277")
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

test_that("the predecessor class needs the successor set to match exactly", {
  # Narrow by construction, so it cannot launder a real extent mismatch: the
  # label must have ENDED (an in-span polity has no successors yet) and its
  # published successors must be the whole member set, not merely overlap it.
  members <- "SDN-2011-2025, SSD-2011-2025"

  expect_true(
    whep:::.bucket_is_predecessor("SUD-1956-2011", "out_of_span", members)
  )
  expect_false(
    whep:::.bucket_is_predecessor("SUD-1956-2011", "matched", members)
  )
  expect_false(
    whep:::.bucket_is_predecessor(
      "SUD-1956-2011",
      "out_of_span",
      "SDN-2011-2025"
    )
  )
  # A polity with no successors published can never reach the class.
  expect_false(
    whep:::.bucket_is_predecessor("SDN-2011-2025", "out_of_span", members)
  )
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

testthat::test_that("206 is the only bucket folding live territories", {
  # whep#557: the `fun.aggregate` guard in `.select_best_source` was justified by
  # 206 AND 999 each folding several live territories into one bucket key. 999 no
  # longer does -- so this pins the property the comment there now records,
  # straight off the crosswalk, with no pins and no network.
  #
  # Measured on `.polity_crosswalk()`, NOT on the raw `polity_area_crosswalk`.
  # The raw table deliberately carries BOTH answers for a Rest-of-World member --
  # a `"fabio_row_fold"` row on `ROW-1850-2025` and, where upstream names one,
  # `"fabio_row_promoted"` rows on the real polity -- and 44 buckets look folded
  # there for that reason alone. `.unfold_rest_of_world()` picks one per area, and
  # its result is what `.aggregate_to_polities()` (and hence
  # `.select_best_source()`) actually keys on. So the property is pinned where it
  # bites, on the default `whep.unfold_rest_of_world = "all"`, made explicit here.
  withr::local_options(whep.unfold_rest_of_world = "all")
  resolved <- tibble::as_tibble(whep:::.polity_crosswalk(
    include_unmapped = FALSE
  ))

  cw <- resolved |>
    dplyr::filter(!is.na(.data$polity_area_code)) |>
    dplyr::distinct(
      .data$polity_area_code,
      .data$polity_code,
      .data$polity_start_year,
      .data$polity_end_year
    )

  folds <- cw |>
    dplyr::cross_join(tibble::tibble(year = 1850L:2023L)) |>
    dplyr::filter(
      .data$polity_start_year <= .data$year,
      .data$polity_end_year > .data$year
    ) |>
    dplyr::summarise(
      n_polities = dplyr::n_distinct(.data$polity_code),
      .by = c("polity_area_code", "year")
    ) |>
    dplyr::filter(.data$n_polities > 1L)

  testthat::expect_equal(unique(folds$polity_area_code), 206L)
  testthat::expect_equal(max(folds$n_polities), 2L)
  testthat::expect_equal(min(folds$year), 2011L)

  # Everything still ON the bucket answers as the bucket: the members upstream
  # names nowhere keep their fold row, and they all share the one aggregate
  # polity, so the bucket key cannot carry two live territories.
  bucket_rows <- resolved |>
    dplyr::filter(.data$polity_area_code == 999L)
  testthat::expect_equal(unique(bucket_rows$polity_code), "ROW-1850-2025")
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
  # number has to stay possible. Asserted on EVERY COLUMN, not just
  # `polity_area_code`: since whep#717 promotion moves the territorial identity
  # too, so a `"none"` run that restored only the numeric half would still
  # publish a different `reporting_polity_code` and this would not have noticed.
  #
  # The comparison is against the shipped table MINUS the promoted rows, which
  # is what "the fold state" now means: those rows exist only to be chosen
  # instead of a fold row, and `"none"` chooses no fold row against them.
  refolded <- as.data.frame(
    withr::with_options(
      list(whep.unfold_rest_of_world = "none"),
      suppressWarnings(whep:::.polity_crosswalk())
    )
  )
  shipped <- as.data.frame(whep::polity_area_crosswalk)
  fold_state <- shipped[shipped$mapping_source != "fabio_row_promoted", ]
  rownames(fold_state) <- NULL
  testthat::expect_equal(refolded, fold_state)
  # No member keeps an identity of its own when everything is re-folded.
  members <- refolded[
    refolded$fabio_code %in% 999L & !refolded$area_code %in% 999L,
  ]
  testthat::expect_gt(nrow(members), 0L)
  testthat::expect_setequal(members$polity_code, "ROW-1850-2025")
  testthat::expect_setequal(members$polity_area_code, 999L)
})

# A PROMOTED MEMBER'S TERRITORIAL IDENTITY (whep#717) --------------------------
#
# Promotion has two halves. The numeric one landed in #628 and is asserted
# above; these assert the other, which did not: for 62 areas
# `polity_area_code == area_code` while `polity_code` stayed `ROW-1850-2025`, an
# aggregate on the continent "World" with no geometry. A row that reports as
# itself and is identified as somewhere else.

testthat::test_that("a promoted member does not publish as Rest of World", {
  # THE INVARIANT, not a list: no area may be its own aggregation bucket and
  # simultaneously carry the bucket's aggregate polity. Written this way so it
  # catches the defect on any area, including one a future snapshot adds.
  cw <- as.data.frame(whep:::.polity_crosswalk())
  members <- cw[
    cw$fabio_code %in% 999L & !cw$area_code %in% 999L & !is.na(cw$area_code),
  ]
  testthat::expect_gt(nrow(members), 0L)

  own_bucket <- members$area_code == members$polity_area_code
  testthat::expect_true(all(own_bucket))

  # 47 of the 61 members now carry a real territory, up from 31 on 2026-08-13 when
  # whep-polities added ten polities (#210), an iso3 for the trust territory (#210) and the
  # sixteen FAOSTAT map rows that let any of them be reached (#212). The rest are held to a
  # DIFFERENT standard below, because upstream names no polity for them and inventing one
  # here is what #717 argues against -- so this asserts the count the mapping supports, and
  # `row_promotion_status()` says which is which.
  with_identity <- unique(members$area_code[
    members$polity_code != "ROW-1850-2025"
  ])
  testthat::expect_equal(length(with_identity), 47L)
  named <- members[members$area_code %in% with_identity, ]
  testthat::expect_true(all(named$polity_type != "aggregate"))
  testthat::expect_false(any(named$continent == "World"))
  testthat::expect_true(all(named$mapping_source == "fabio_row_promoted"))
  # Syria and North Macedonia are the areas the issue names; Syria is the
  # largest single contributor the fold ever carried.
  testthat::expect_true(all(c(212L, 154L) %in% with_identity))
})

testthat::test_that("the identity is resolved year by year, not once", {
  # A promoted member can span several polities, and #717 is explicit that a
  # 1950 row and a 2020 row need not agree. Syria has two upstream periods, so
  # a "current polity" implementation would answer `SYR-1967-2025` for both and
  # pass every count-based check above.
  syria <- whep::add_polity_code(
    tibble::tibble(area_code = 212L, year = c(1950L, 1965L, 2020L))
  )
  testthat::expect_equal(
    syria$polity_code,
    c("SYR-1946-1967", "SYR-1946-1967", "SYR-1967-2025")
  )
  testthat::expect_true(all(syria$has_geometry))

  # And the pre-anchor row is honest about how it got there: 1950 is resolved at
  # the back-cast anchor, and `SYR-1946-1967` really is live in 1950, so it
  # stays `matched` rather than becoming a stand-in.
  testthat::expect_setequal(syria$mapping_status, "matched")

  macedonia <- whep::add_polity_code(
    tibble::tibble(area_code = 154L, year = c(1965L, 2020L))
  )
  testthat::expect_setequal(macedonia$polity_code, "MKD-1991-2025")
  # The 1965 row is NOT hidden: `MKD-1991-2025` did not exist then, and saying
  # so is the point of promoting -- the fold answered `ROW-1850-2025`,
  # `matched`, for the same row.
  testthat::expect_equal(
    macedonia$mapping_status,
    c("out_of_span", "matched")
  )
})

testthat::test_that("a member upstream does not name keeps the bucket", {
  # THE NEAR MISS. Handing the identity to an area upstream has NOT named is
  # the same defect in the opposite direction -- it would be WHEP minting a
  # territory, which is what #717 says not to do -- and three of these are not
  # territories at all. `row_promotion_status()` has to keep them separable
  # from a genuine Rest-of-World residual, and they must never acquire a polity
  # of their own.
  status <- whep::row_promotion_status()
  never <- status[status$area_code %in% c(252L, 254L), ]

  testthat::expect_equal(nrow(never), 2L)
  testthat::expect_setequal(never$status, "no_polity")
  testthat::expect_setequal(never$polity_codes, "ROW-1850-2025")
  # 999 is the residual itself, not a member of it, so it is not reported here.
  testthat::expect_false(999L %in% status$area_code)

  # THE NOTIFICATION FIRED. This asserted "no_polity" for the ten FAOSTAT land-use
  # territories of whep-polities#209, with the comment "when upstream adds those rows this
  # fails, which is the notification". Upstream added them on 2026-08-13 -- the polities in
  # #210, the map rows in #212 -- so all ten now carry their own territory and the assertion
  # is inverted rather than deleted: the ten are named explicitly so a REGRESSION upstream
  # would fail here too.
  answered <- c(270L, 36L, 224L, 163L, 164L, 172L, 258L, 82L, 281L, 279L)
  testthat::expect_setequal(
    status$status[status$area_code %in% answered],
    "own_polity"
  )

  cw <- as.data.frame(whep:::.polity_crosswalk())
  unnamed <- status$area_code[status$status != "own_polity"]
  testthat::expect_setequal(
    cw$polity_code[cw$area_code %in% unnamed],
    "ROW-1850-2025"
  )

  # THE NEAR MISS IS CLOSED, AND BY THE ROUTE THIS TEST INSISTED ON. It used to assert that
  # six members DO have a live territorial polity upstream and are still on the bucket,
  # because no FAOSTAT map row names it -- warning that deriving the mapping from the ISO3
  # instead "would empty this class rather than shrinking it the way an upstream fix does".
  #
  # The class is empty as of 2026-08-13, and the upstream fix is what emptied it: whep-polities
  # #212 added the missing map rows. The distinction the old comment drew is still checked, and
  # it is the `map_match_route` assertion at the end of this test -- every promoted identity
  # traces to an upstream map row, never to inference. So this now asserts the class is empty
  # AND keeps the proof of HOW it emptied, which is the part worth guarding.
  unmapped <- status[status$status == "polity_unmapped", ]
  testthat::expect_equal(nrow(unmapped), 0L)
  # Every identity that WAS handed out came from an upstream map row, never
  # from inference: `map_match_route` is upstream's record of how it decided.
  promoted_rows <- cw[cw$mapping_source %in% "fabio_row_promoted", ]
  testthat::expect_true(all(!is.na(promoted_rows$map_match_route)))
})

testthat::test_that("row_promotion_status splits the members three ways", {
  status <- whep::row_promotion_status()

  # THE SPLIT MOVED ON 2026-08-13, and the middle class emptied because this test named it
  # precisely enough to act on. `polity_unmapped` was "the actionable upstream list: a live
  # territorial polity exists for the ISO3 and only the map row is missing", and it held
  # exactly six areas -- 22 Aruba, 71 French Southern Territories, 94 Holy See, 218 Tokelau,
  # 243 Wallis and Futuna, 271 South Georgia. whep-polities#212 added the sixteen missing
  # FAOSTAT map rows, which included all six.
  #
  #     own_polity       31 -> 47   the ten of #209/#210 plus these six
  #     polity_unmapped   6 ->  0   the class this test existed to report
  #     no_polity        24 -> 14   the fourteen that genuinely have no polity
  #
  # 47 + 0 + 14 = 61, unchanged. The `no_polity` fourteen are Antarctica, Bouvet, Heard and
  # McDonald, Svalbard, the US-administered Pacific atolls, the Neutral Zone and the two
  # accounting residuals UXY/OXY -- none of which is a territory that should get a polity.
  testthat::expect_equal(nrow(status), 61L)
  testthat::expect_equal(sum(status$status == "own_polity"), 47L)
  testthat::expect_equal(sum(status$status == "polity_unmapped"), 0L)
  testthat::expect_equal(sum(status$status == "no_polity"), 14L)
  # Kept as an assertion rather than deleted: if any of those six loses its map row upstream
  # it reappears here, and the class is actionable again.
  testthat::expect_length(
    status$area_code[status$status == "polity_unmapped"],
    0L
  )
  testthat::expect_setequal(
    status$polity_codes[status$status != "own_polity"],
    "ROW-1850-2025"
  )
  # A period count that is not a row count: Syria has two, most have one.
  testthat::expect_equal(status$n_periods[status$area_code == 212L], 2L)
  testthat::expect_equal(status$n_periods[status$area_code == 209L], 1L)

  # Every mode is reported, so the diagnostic cannot go silent on a re-fold.
  refolded <- withr::with_options(
    list(whep.unfold_rest_of_world = "none"),
    suppressWarnings(whep::row_promotion_status())
  )
  testthat::expect_setequal(refolded$status, "folded")
  cbs_only <- withr::with_options(
    list(whep.unfold_rest_of_world = "cbs_reporters"),
    suppressWarnings(whep::row_promotion_status())
  )
  testthat::expect_equal(sum(cbs_only$status == "own_polity"), 4L)
  testthat::expect_setequal(
    cbs_only$area_code[cbs_only$status == "own_polity"],
    c(153L, 154L, 209L, 212L)
  )
})

testthat::test_that("one area-year still resolves to one identity", {
  # THE RULE THAT MUST SURVIVE A PROMOTION. whep#563 forced the revert of
  # whep#480 for splitting a bucket while conserving mass, and whep#589 diluted
  # Syria's livestock 12x through the same family of bug: a key that resolves to
  # two identities stops summing. Promotion adds 36 crosswalk rows, so the
  # cheapest way for it to go wrong is an `(area, year)` with two answers.
  #
  # Asserted over the whole reporting era rather than on a sample, and in every
  # mode, because the fold state decides which rows are live.
  areas <- sort(unique(stats::na.omit(whep::polity_area_crosswalk$area_code)))
  grid <- tidyr::expand_grid(area_code = areas, year = 1961:2023)
  for (mode in c("all", "none", "cbs_reporters")) {
    resolved <- withr::with_options(
      list(whep.unfold_rest_of_world = mode),
      suppressWarnings(whep::add_polity_code(grid))
    )
    per_key <- resolved |>
      dplyr::distinct(
        .data$area_code,
        .data$year,
        .data$polity_code,
        .data$polity_name
      ) |>
      dplyr::count(.data$area_code, .data$year)
    testthat::expect_equal(nrow(resolved), nrow(grid))
    testthat::expect_equal(max(per_key$n), 1L)
  }

  # And one polity code carries one name, so the two published identity columns
  # cannot disagree with each other.
  named <- whep::polity_area_crosswalk |>
    tibble::as_tibble() |>
    dplyr::filter(!is.na(.data$polity_code)) |>
    dplyr::distinct(.data$polity_code, .data$polity_name) |>
    dplyr::count(.data$polity_code)
  testthat::expect_equal(max(named$n), 1L)
})

testthat::test_that("row_promotion_status validates its crosswalk", {
  testthat::expect_error(
    whep::row_promotion_status(tibble::tibble(area_code = 1L)),
    "missing"
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
    area = "Aggregate",
    reporting_polity_area_code = 900L,
    reporting_polity_code = "AGG-1900-2000",
    reporting_polity_name = "Aggregate",
    reporting_polity_has_geometry = TRUE
  )

  out <- whep:::.apply_bucket_area_labels(agg, labels)
  testthat::expect_equal(nrow(out), 2L)
  # The prefix is where it always was: the identity is added, not inserted.
  testthat::expect_equal(names(out)[1:3], c("year", "area_code", "area"))
  testthat::expect_equal(out$area, c("Aggregate", NA_character_))
  # The identity travels with the label, and an unlabelled bucket keeps NA
  # rather than borrowing the labelled one's polity.
  testthat::expect_equal(
    out$reporting_polity_code,
    c("AGG-1900-2000", NA_character_)
  )
  # `polity_area_code` survives the rename as the bucket's own resolution: the
  # fixed point that lets `.add_reporting_polity_columns()` tell a carried
  # identity from a stale one, NA for the bucket that resolves to nothing.
  testthat::expect_equal(out$polity_area_code, c(900L, NA_integer_))
})

# The identity the fold emits, which is what whep#670 is about ----------------
#
# `.aggregate_to_polities()` has always resolved the bucket's polity to label it
# and then dropped the code, leaving ~100 outputs to re-derive it at the tail.
# It now emits it. These tests pin the property that makes that safe: the
# emitted identity is the SAME FUNCTION of (area_code, year) as the tail
# helper's, not a second opinion resolved somewhere else.

testthat::test_that(".aggregate_to_polities() emits the reporting identity", {
  rows <- data.table::data.table(
    area_code = c(276L, 277L, 40L, 255L),
    year = c(2015L, 2015L, 2015L, 1990L),
    element = "production",
    unit = "t",
    item_prod_code = "83",
    value = c(1, 2, 4, 8)
  )

  out <- suppressWarnings(
    whep:::.aggregate_to_polities(data.table::copy(rows), item_prod_code)
  )
  testthat::expect_true(all(whep:::.reporting_polity_cols() %in% names(out)))
  # Sudan (276) and South Sudan (277) fold into bucket 206, which carries the
  # bucket's own polity rather than either member's.
  bucket <- out[out$area_code == 206L, ]
  testthat::expect_equal(nrow(bucket), 1L)
  testthat::expect_equal(bucket$value, 3)
  testthat::expect_equal(bucket$reporting_polity_code, "SUD-1956-2011")
  testthat::expect_equal(
    out$reporting_polity_code[out$area_code == 40L],
    "CHL-1902-2025"
  )
})

testthat::test_that("the emitted identity equals the tail resolution", {
  rows <- data.table::data.table(
    area_code = c(276L, 277L, 40L, 255L, 256L, 15L),
    year = c(2015L, 2015L, 2015L, 1990L, 1990L, 1970L),
    element = "production",
    unit = "t",
    item_prod_code = "83",
    value = c(1, 2, 4, 8, 16, 32)
  )
  out <- suppressWarnings(
    whep:::.aggregate_to_polities(data.table::copy(rows), item_prod_code)
  )

  stripped <- data.table::copy(out)
  stripped[, (whep:::.reporting_polity_cols()) := NULL]

  # Keeping what the fold emitted and resolving it from scratch produce the same
  # published table, down to column order and type.
  kept <- whep:::.add_reporting_polity_columns(out)
  resolved <- whep:::.add_reporting_polity_columns(stripped)
  testthat::expect_equal(as.data.frame(kept), as.data.frame(resolved))
  testthat::expect_true(
    all(whep:::.reporting_polity_cols() %in% names(kept))
  )
})

testthat::test_that("every bucket resolves the same way on both paths", {
  # The contract whep#670 needs before the fold's answer can be trusted
  # downstream: over every bucket code the crosswalk knows and every year the
  # package covers, resolving the bucket at the fold and resolving it at the
  # tail are the same function. The two calls differ in one argument -- the
  # fold excludes unmapped crosswalk rows and the tail keeps them -- and a
  # bucket code is mapped by construction, so they must agree everywhere.
  buckets <- whep:::.polity_crosswalk(include_unmapped = FALSE)
  grid <- data.table::CJ(
    area_code = sort(unique(stats::na.omit(buckets$polity_area_code))),
    year = seq(1850L, 2030L, by = 1L)
  )
  at_fold <- whep:::.add_polity_columns_dt(
    data.table::copy(grid),
    include_unmapped = FALSE
  )
  at_tail <- whep:::.add_polity_columns_dt(
    data.table::copy(grid),
    include_unmapped = TRUE
  )

  testthat::expect_gt(nrow(grid), 40000L)
  testthat::expect_equal(at_fold$polity_code, at_tail$polity_code)
  testthat::expect_equal(at_fold$polity_name, at_tail$polity_name)
  testthat::expect_equal(at_fold$has_geometry, at_tail$has_geometry)
  # And a bucket code is a fixed point wherever it resolves at all, which is
  # what lets the tail helper tell a carried identity from one left behind by a
  # re-keying. It is NA, never another bucket, in the years no polity covers.
  testthat::expect_true(all(
    is.na(at_tail$polity_area_code) |
      at_tail$polity_area_code == at_tail$area_code
  ))
  testthat::expect_gt(sum(is.na(at_tail$polity_area_code)), 0L)
})

testthat::test_that("a fallback label does not borrow the member's polity", {
  # Bucket 950 has no crosswalk row of its own, so the label falls back to the
  # member's. The polity code does NOT: a member's polity is not the bucket's
  # identity, and the tail helper resolving 950 would return NA. Borrowing it
  # here would be the one way the two paths could disagree.
  .local_fold_crosswalk()
  rows <- .bucket_fixture_rows()[area_code %in% c(951L, 952L)]

  out <- suppressWarnings(
    whep:::.aggregate_to_polities(data.table::copy(rows), item_prod_code)
  )
  testthat::expect_equal(out$area, "P")
  testthat::expect_true(is.na(out$reporting_polity_code))
  testthat::expect_true(is.na(out$reporting_polity_name))
  testthat::expect_true(is.na(out$polity_area_code))

  # Which is the same answer as resolving the bucket at the tail. Borrowing any
  # part of the member's identity here -- its code, its name, or its bucket --
  # would make the two paths differ on exactly these rows.
  stripped <- data.table::copy(out)
  stripped[, (whep:::.reporting_polity_cols()) := NULL]
  testthat::expect_equal(
    as.data.frame(whep:::.add_reporting_polity_columns(out)),
    as.data.frame(whep:::.add_reporting_polity_columns(stripped))
  )
})


# -- area-vintage mismatch (#884) ----------------------------------------------

test_that(".area_reporting_windows says when each area code reports", {
  windows <- whep:::.area_reporting_windows()

  expect_true(all(
    c("area_code", "window_start", "window_end") %in% names(windows)
  ))
  expect_equal(anyDuplicated(windows$area_code), 0L)
  # Belgium-Luxembourg (15) is the FAOSTAT vocabulary until 1999; Belgium (255)
  # and Luxembourg (256) only start at 2000. That is the mismatch #884 is about.
  belux <- windows[windows$area_code == 15L, ]
  belgium <- windows[windows$area_code == 255L, ]
  expect_equal(belux$window_end, 1999L)
  expect_equal(belgium$window_start, 2000L)
})

test_that(".off_window_area_years flags a source's wrong-vintage area-years", {
  # FishStat's shape: Belgium keyed 255 in 1976-1999, when the territory
  # reports as 15. Area 15 in the same years is on-window and must not flag.
  dt <- tibble::tribble(
    ~area_code, ~year, ~value,
    255L, 1976L, 1,
    255L, 1990L, 1,
    15L, 1990L, 1,
    255L, 2000L, 1
  )

  off <- whep:::.off_window_area_years(dt)

  expect_equal(off$area_code, 255L)
  expect_equal(off$rows, 2L)
  expect_equal(off$year_min, 1976L)
  expect_equal(off$year_max, 1990L)
})

test_that(".off_window_area_years does not flag a deliberate fold", {
  # FAOSTAT reports Sudan (276) and South Sudan (277) from 2012 and WHEP sums
  # them into bucket 206, whose own window ends 2011, so both the members and
  # the bucket look off-window while nothing is wrong.
  members <- tibble::tribble(
    ~area_code, ~year, ~value,
    276L, 2015L, 1,
    277L, 2015L, 1
  )
  # The same fold seen from the other side, which is the shape a table that has
  # already been through `.aggregate_to_polities()` has: bucket 238 in 1990,
  # reported by area 62 (Ethiopia PDR) and folded onto 238. The crosswalked
  # trade record the CBS recovery reads is exactly this, and a check on the
  # bucket's own window declines 29 real trade rows at 1990.
  bucket <- tibble::tribble(
    ~area_code, ~year, ~value,
    206L, 2015L, 1,
    238L, 1990L, 1
  )

  expect_equal(nrow(whep:::.off_window_area_years(members)), 0L)
  expect_equal(nrow(whep:::.off_window_area_years(bucket)), 0L)
})

test_that(".reported_bucket_years follows a fold across its handover", {
  # Bucket 238 is reported in 1990 by area 62 and in 2000 by area 238 itself.
  reported <- whep:::.reported_bucket_years(c(1990L, 2000L))

  expect_true(all(
    c(1990L, 2000L) %in% reported$year[reported$area_code == 238L]
  ))
  # Nothing reports bucket 255 before 2000, which is the whole of #884.
  expect_false(1990L %in% reported$year[reported$area_code == 255L])
  expect_true(2000L %in% reported$year[reported$area_code == 255L])
})

test_that(".off_window_area_years tolerates empty and column-less input", {
  empty <- tibble::tibble(area_code = integer(), year = integer())

  expect_equal(nrow(whep:::.off_window_area_years(empty)), 0L)
  expect_equal(nrow(whep:::.off_window_area_years(tibble::tibble(x = 1))), 0L)
})

test_that(".warn_off_window_area_years names the area and can be silenced", {
  dt <- tibble::tribble(
    ~area_code, ~year, ~value,
    255L, 1990L, 1
  )

  expect_warning(
    whep:::.warn_off_window_area_years(dt, "fishstat-trade"),
    "255"
  )
  withr::local_options(whep.warn_area_vintage = FALSE)
  expect_no_warning(
    whep:::.warn_off_window_area_years(dt, "fishstat-trade")
  )
})

test_that(".warn_off_window_area_years is silent on an on-window source", {
  dt <- tibble::tribble(
    ~area_code, ~year, ~value,
    15L, 1990L, 1,
    255L, 2010L, 1
  )

  expect_no_warning(whep:::.warn_off_window_area_years(dt, "faostat-fbs-old"))
})

test_that(".abort_if_off_window_areas aborts on a created area-year", {
  dt <- tibble::tribble(
    ~area_code, ~year, ~value,
    255L, 1990L, 1
  )

  expect_error(
    whep:::.abort_if_off_window_areas(dt),
    class = "whep_error_off_window_area_year"
  )
  expect_no_error(
    whep:::.abort_if_off_window_areas(
      tibble::tribble(~area_code, ~year, ~value, 15L, 1990L, 1)
    )
  )
})
