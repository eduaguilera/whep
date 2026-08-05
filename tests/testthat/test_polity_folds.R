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
  # many areas but resolves to `ROW-1850-2023`, an aggregate polity that means
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

  # No value moved: the two members keep their own rows and their own values,
  # and both now sit under bucket code 206.
  expect_equal(folded$area_code, c(206L, 206L))
  expect_equal(sum(folded$value), 3405356)

  withr::local_options(whep.warn_polity_folds = FALSE)
  expect_no_warning(
    whep:::.aggregate_to_polities(
      data.table::copy(sudan),
      item_prod_code,
      item_prod
    )
  )
})
