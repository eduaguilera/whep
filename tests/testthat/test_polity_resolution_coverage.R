# WHAT A POLITY RE-SYNC MAY DO TO A BUILD, PINNED WITHOUT BUILDING (whep#444).
#
# `.aggregate_to_polities()` drops every row whose `area_code`/`year` resolves
# to no polity, and it groups by `polity_name` beside `polity_area_code`. So a
# change to the shipped crosswalk moves published quantities through exactly
# two doors:
#
#   1. COVERAGE -- an area-year that used to resolve stops resolving (or
#      starts), so source rows leave or enter the build.
#   2. LABEL SPLIT -- two areas folded into one bucket stop sharing a
#      `polity_name`, so the bucket is emitted as several rows instead of
#      summed into one. That is whep#480, reverted in whep#561:
#      `polity_area_code` was unchanged for all 266 areas and mass conserved,
#      and the build broke anyway.
#
# Neither door is visible to a value-neutrality check, and finding out which
# one a vintage opened used to cost two ~9-minute full-range `get_wide_cbs()`
# builds. Both are decidable from `polity_area_crosswalk` alone, offline, in
# about a second -- so they are decided here.
#
# Both are tripwires: they are meant to fail on a deliberate re-sync, and the
# maintainer is meant to re-state the new expectation rather than delete them.

# The default `build_commodity_balances()` window, which is what the published
# wide CBS covers. `.add_polity_columns_dt()` floors the lookup year at its
# 1961 back-cast anchor, so pre-1961 rows resolve to the 1961 territory.
.resolution_grid <- function(years = 1850:2023) {
  cw <- whep::polity_area_crosswalk
  areas <- sort(unique(stats::na.omit(cw$area_code)))
  grid <- data.table::CJ(area_code = areas, year = years)
  whep:::.add_polity_columns_dt(
    grid,
    code_col = "area_code",
    year_col = "year",
    include_unmapped = FALSE
  )
}

testthat::test_that("only three reporting areas fail to resolve", {
  resolved <- .resolution_grid()

  gaps <- resolved[is.na(polity_code), c("area_code", "year"), with = FALSE]

  # Stated as rules rather than as a count, so the expectation carries the
  # reason it holds:
  #   15  Belgium-Luxembourg   -- reports jointly only to 1999, after which
  #                               Belgium (255) and Luxembourg (256) report
  #                               separately.
  #   151 Netherlands Antilles -- dissolved in 2010.
  #   351 China                -- the FAOSTAT statistical aggregate, left
  #                               unmapped on purpose so that it cannot
  #                               double-count areas 41, 96, 128 and 214.
  expected <- rbind(
    data.frame(area_code = 15L, year = 2000:2023),
    data.frame(area_code = 151L, year = 2011:2023),
    data.frame(area_code = 351L, year = 1850:2023)
  )

  testthat::expect_setequal(
    paste(gaps$area_code, gaps$year),
    paste(expected$area_code, expected$year)
  )

  # Non-vacuity: the grid really was resolved, not empty.
  testthat::expect_equal(
    sum(!is.na(resolved$polity_code)),
    nrow(resolved) - nrow(expected)
  )
})

testthat::test_that("two buckets carry one polity name per year, no more", {
  # The shipped-data counterpart of the three-row fixture in
  # `test_read_raw_inputs.R`. That one proves the aggregator sums a bucket;
  # this one proves the crosswalk hands it a bucket that can be summed.
  #
  # The two live exceptions are exactly the two buckets that fold a member
  # outside Rest-of-World, and both are filed:
  #
  #   206  FAOSTAT areas 206 "Sudan (former)", 276 "Sudan" and 277
  #        "South Sudan" share it under three different polity names, so the
  #        aggregator emits three rows for one key. That is why
  #        `.select_best_source()`'s `fun.aggregate = sum` is load-bearing
  #        (whep#557), and why the bucket's own label is contested
  #        (whep#546, whep#414).
  #   238  FAOSTAT areas 62 "Ethiopia PDR" and 238 "Ethiopia" share it, and
  #        from 1993 the dissolved area 62 stands in with `ETH-1952-1993`
  #        while 238 is a period hit on `ETH-1993-2025`. New with whep#741,
  #        which stopped the crosswalk handing dead area 62 the live polity
  #        the map awards to 238; that row was manufacturing the agreement,
  #        not preventing the split. Area 62 publishes nothing after 1992, so
  #        the aggregator never sees two labels for one key here.
  resolved <- .resolution_grid()
  resolved <- resolved[!is.na(polity_area_code)]

  labels <- resolved[,
    list(n_labels = data.table::uniqueN(polity_name)),
    by = c("polity_area_code", "year")
  ]
  split_buckets <- sort(unique(labels$polity_area_code[labels$n_labels > 1L]))

  testthat::expect_equal(split_buckets, c(206L, 238L))
})

# WHAT A MISSING `polity_code` MEANS, AND WHAT IT DOES NOT (whep#875).
#
# `polity_area_crosswalk` is keyed on reporting areas, not on polities: the
# builder starts from `regions_full.csv` and asks which polity each area names,
# so a polity gets a row only if some area names it. Reading an absent polity as
# a coverage gap is what #875 did, and the two tests below pin the reason it is
# not one. They are tripwires: a re-sync that makes either fail is telling the
# maintainer that an absence stopped being structural.

.live_polity_attrs <- function() {
  attrs <- tibble::as_tibble(sf::st_drop_geometry(whep::polities))
  attrs[whep:::.polity_is_live(attrs$wiki_status), , drop = FALSE]
}

.absent_from_crosswalk <- function() {
  live <- .live_polity_attrs()
  absent <- live[
    !live$polity_code %in% whep::polity_area_crosswalk$polity_code,
  ]
  dplyr::mutate(
    absent,
    polity_prefix = sub("-[0-9]{4}-[0-9]{4}$", "", .data$polity_code)
  )
}

testthat::test_that("an absent polity is one no reporting area names", {
  testthat::skip_if_not_installed("sf")

  absent <- .absent_from_crosswalk()

  # Non-vacuity, and the scale of the thing: absence is ordinary. 176 of the
  # 735 live polities have no row, only 7 of them aggregates, so the 7 are a
  # slice of a normal phenomenon rather than a class of their own. It was 8
  # until whep#860 gave `F206-2011-2025` a row keyed on the aggregation bucket.
  testthat::expect_gt(nrow(absent), 100L)
  testthat::expect_equal(sum(absent$polity_type == "aggregate"), 7L)

  # The invariant that makes absence structural: no absent AGGREGATE carries a
  # prefix that any reporting area carries, so the row space has no slot to put
  # it in. Were one to appear, the crosswalk really would be omitting a polity
  # some area names, and that would be the bug #875 assumed.
  prefixes <- unique(whep::polity_area_crosswalk$legacy_polity_prefix)
  aggregates <- absent[absent$polity_type == "aggregate", ]
  testthat::expect_equal(
    intersect(aggregates$polity_prefix, prefixes),
    character(0)
  )
})

testthat::test_that("absent aggregates are label-reachable but for two", {
  testthat::skip_if_not_installed("sf")

  absent <- .absent_from_crosswalk()
  aggregates <- absent[absent$polity_type == "aggregate", ]
  reachable <- aggregates$polity_code %in%
    whep::polity_label_aliases$polity_code

  # Five resolve by the LABEL route instead of by an area code, which is the
  # route a pre-FAOSTAT combined reporting unit is supposed to take: it never
  # had an area code to be absent from.
  testthat::expect_setequal(
    aggregates$polity_code[reachable],
    c(
      "AOI-1936-1941",
      "GCT-1919-1956",
      "MASG-1946-1963",
      "PAPNG-1920-1949",
      "SYL-1944-1953"
    )
  )

  # Two are reachable from neither route, and both for the same reason:
  # `EGYSUD-1934-1956` and `CODRU-1922-1960` are upstream composed-union
  # identities for footnote series that fold a colony into its metropole.
  # Upstream has registered no label for either, so nothing in this package can
  # reach them and nothing here can fix that.
  #
  # `F206-2011-2025` used to be the third, and was a different thing: a real
  # gap in the BUCKET vocabulary rather than a missing label. whep#860 closed it
  # by keying a crosswalk row on `polity_area_code` 206 -- so it is now absent
  # from THIS list because it is present in the crosswalk, which is what the
  # test below asserts rather than leaves implied.
  testthat::expect_setequal(
    aggregates$polity_code[!reachable],
    c("CODRU-1922-1960", "EGYSUD-1934-1956")
  )
  testthat::expect_true(
    "F206-2011-2025" %in% whep::polity_area_crosswalk$polity_code
  )
})
