# A FAOSTAT area maps to a sequence of polities that is meant to partition time, so
# `(area_code, year)` has exactly one answer. Nothing checked that, and two separate
# routes break it.
#
# ROUTE 1 -- DEAD POLITIES WERE CANDIDATES. Upstream retires a polity when a finer
# split supersedes it (`F248-1920-1991` was retired once `F248-1920-1947` and
# `F248-1947-1991` replaced it) and records that in `wiki_status`. The crosswalk build
# carried the column and never filtered on it, so the retired period stayed a
# candidate and competed with its own replacements. `add_polity_code()` returned
# whichever row order surfaced. Fixed in `data-raw/table_mappings.R` by joining
# `live_polity_attrs` rather than `polity_attrs`.
#
# That filter is now load-bearing rather than latent. The refreshed snapshot (#530)
# holds 41 dead polities against the previous vintage's 27, and 14 of the codes this
# package used to treat as live are dead in it -- including `BLX-1921-1999`,
# `CAN-1948-2025` and `ROW-1850-2023`. Without the filter each of those would compete
# with its own replacement. The first test below therefore has teeth on shipped data;
# the second still exercises the DETECTOR against a fixture so it cannot go vacuous if
# a future vintage happens to be clean.
#
# ROUTE 2 -- GENUINELY OVERLAPPING LIVE PERIODS, which no downstream filter can fix.
# Montenegro had `MNE-1913-1915` and `MNE-1913-1918` both `draft` upstream and both
# covering 1913-1914, and Peru had `PER-1825-1909` overlapping `PER-1825-1884` and
# `PER-1884-1909`. Deciding those was an upstream question, filed upstream as
# whep-polities issue 62, and upstream has now decided both. `MNE-1913-1915` is
# retired and `PER-1825-1909` superseded, so route 1's filter removes them.
#
# So the third test asserts ZERO. It used to pin the two known conflicts because
# asserting zero would have failed; the pin was written to shrink when upstream fixed
# Montenegro, and it has shrunk all the way. Zero is the invariant the crosswalk is
# supposed to satisfy, so it is asserted directly now instead of enumerated.

testthat::test_that("no dead polity is a resolution candidate", {
  cw <- as.data.frame(whep::polity_area_crosswalk)
  p <- as.data.frame(whep::polities)

  dead <- p$polity_code[
    !is.na(p$wiki_status) & p$wiki_status %in% c("retired", "superseded")
  ]
  candidates <- unique(cw$polity_code[!is.na(cw$polity_code)])

  testthat::expect_equal(intersect(candidates, dead), character(0))
})

testthat::test_that("the conflict detector finds an overlap the filter cannot", {
  # Against a fixture, so this is not hostage to which polities the shipped vintage
  # happens to contain. Two periods of one area covering 1930: a real conflict.
  overlapping <- data.frame(
    area_code = c(15L, 15L, 99L),
    polity_code = c("XXX-1900-1950", "XXX-1925-1950", "YYY-1900-1950"),
    polity_start_year = c(1900L, 1925L, 1900L),
    polity_end_year = c(1950L, 1950L, 1950L),
    stringsAsFactors = FALSE
  )
  out <- whep:::.area_year_polity_conflicts(overlapping)

  testthat::expect_gt(nrow(out), 0L)
  testthat::expect_setequal(unique(out$area_code), 15L)
  testthat::expect_setequal(unique(out$n), 2L)
  # 1925..1949 inclusive -- 25 years, and NOT 26, because `polity_end_year` is
  # exclusive. An off-by-one here would report a phantom conflict at every boundary.
  testthat::expect_equal(nrow(out), 25L)
  testthat::expect_equal(min(out$year), 1925L)
  testthat::expect_equal(max(out$year), 1949L)
})

testthat::test_that("adjacent periods are not a conflict", {
  # The complement, and the case an exclusive-end off-by-one would break: periods that
  # meet exactly must not overlap.
  adjacent <- data.frame(
    area_code = c(1L, 1L),
    polity_code = c("AAA-1900-1950", "AAA-1950-2000"),
    polity_start_year = c(1900L, 1950L),
    polity_end_year = c(1950L, 2000L),
    stringsAsFactors = FALSE
  )
  testthat::expect_equal(nrow(whep:::.area_year_polity_conflicts(adjacent)), 0L)
})

testthat::test_that("the shipped crosswalk resolves every area-year uniquely", {
  # ZERO, not a pin -- see the header. The measured history of this number on the
  # same detector: prefix inference alone gave 199 conflicting area-years across
  # five groups (areas 15, 28, 101, 170, 273); adopting the upstream FAOSTAT map
  # (#517) resolved the three FAOSTAT-era groups and left 86 area-years across
  # two pre-1961 groups (areas 170 Peru and 273 Montenegro); refreshing the
  # polities snapshot (#530) resolved both, because upstream retired
  # `MNE-1913-1915` and superseded `PER-1825-1909`.
  #
  # Any regression here means a NEW overlap, so the offending code pairs are
  # asserted before the count: a bare `nrow == 0` failure would not say which.
  #
  # ASSERTED IN EVERY MODE, because since whep#717 the shipped table holds two
  # ALTERNATIVE answers for a Rest-of-World member -- `ROW-1850-2025` over
  # 1850-2025 and, where upstream names them, that member's own periods -- and
  # `.unfold_rest_of_world()` keeps one per area. What must partition time is
  # the table the resolver reads, once per mode; the raw table is a union of
  # three partitions and overlaps by construction (37 pairs, 4,111 area-years).
  # Reading the raw table here would therefore be asserting the wrong thing in
  # the loudest possible way.
  for (mode in c("all", "none", "cbs_reporters")) {
    out <- withr::with_options(
      list(whep.unfold_rest_of_world = mode),
      suppressWarnings(whep:::.area_year_polity_conflicts())
    )
    testthat::expect_equal(unique(out$polity_codes), character(0))
    testthat::expect_equal(nrow(out), 0L)
  }
})

testthat::test_that("the raw crosswalk holds one answer per area and mode", {
  # The other half of the test above: an area may carry two answers ONLY when
  # they belong to different `.unfold_rest_of_world()` modes, and the shipped
  # table must not smuggle a genuine overlap in behind that licence. So every
  # overlapping pair in the raw table has to be a fold row against the promoted
  # rows of the SAME area -- never two fold rows, never two promoted ones, and
  # never an area outside bucket 999.
  cw <- as.data.frame(whep::polity_area_crosswalk)
  out <- whep:::.area_year_polity_conflicts(cw)

  testthat::expect_gt(nrow(out), 0L)
  sources <- lapply(strsplit(out$polity_codes, ", ", fixed = TRUE), \(codes) {
    sort(unique(cw$mapping_source[cw$polity_code %in% codes]))
  })
  testthat::expect_true(all(vapply(
    sources,
    \(s) identical(s, c("fabio_row_fold", "fabio_row_promoted")),
    logical(1)
  )))
  conflicting_areas <- unique(out$area_code)
  testthat::expect_true(all(
    cw$fabio_code[cw$area_code %in% conflicting_areas] == 999L
  ))
  # And exactly the areas the promotion gives an identity to.
  promoted <- sort(unique(
    cw$area_code[cw$mapping_source == "fabio_row_promoted"]
  ))
  testthat::expect_setequal(conflicting_areas, promoted)
})

# THE CONTRACT ------------------------------------------------------------------
#
# `(area_code, year)` determines `polity_code`. That is what makes the joins in
# `R/` keyed on the numeric area code polity-correct without saying so, and there
# are a great many of them: 275 single-line `by =` / `on =` specifications name
# `area_code` (176 of them alongside `year`) against 6 naming `polity_code`. It
# is also the thing #458 doubted -- the identity is not lost by keying on the
# numeric code, it is recoverable from it. The tests below assert that as a
# guarantee instead of leaving it a property of the shipped snapshot.
#
# The check above is NOT that guarantee, for two reasons.
#
# It reads the crosswalk's spans as declared, and the resolver does not: it joins
# on `.polity_join_end_year()`, which widens an open period by a year and to the
# upstream map's inclusive `map_year_end`. 263 shipped rows are widened.
#
# And measuring the guarantee on `add_polity_code()`'s OUTPUT is vacuous: one
# input row is one output row, because `unique(matches, by = rowid)` keeps
# exactly one candidate after a `polity_start_year DESC` sort. Ambiguity is
# resolved by row order and never shows as a duplicated row, which is precisely
# why it needs a detector at the CANDIDATE level.

testthat::test_that("the joined spans see an overlap the declared spans hide", {
  # Against a fixture, so the strictly-stronger relation is asserted directly
  # rather than inferred from whichever periods the shipped vintage holds.
  # Adjacent declared periods, and the earlier one's upstream map claiming the
  # boundary year as reported: [1900, 2000) and [2000, 2050) do not touch, but
  # `map_year_end = 2000` widens the first to cover 2000, which the second also
  # covers.
  fixture <- data.frame(
    area_code = c(1L, 1L),
    polity_code = c("AAA-1900-2000", "BBB-2000-2050"),
    polity_start_year = c(1900L, 2000L),
    polity_end_year = c(2000L, 2050L),
    map_year_end = c(2000L, 2049L),
    stringsAsFactors = FALSE
  )

  testthat::expect_equal(nrow(whep:::.area_year_polity_conflicts(fixture)), 0L)

  joined <- whep:::.polity_join_conflicts(fixture, years = 1990:2010)
  testthat::expect_equal(nrow(joined), 1L)
  testthat::expect_equal(joined$year, 2000L)
  testthat::expect_equal(joined$polity_codes, "AAA-1900-2000, BBB-2000-2050")
})

testthat::test_that("every area-year has exactly one joined candidate", {
  # ZERO, and it used to be one: `ANG-1905-1975` (colonial Angola) records no
  # successor upstream, `.open_polity_codes()` called it open,
  # `.polity_join_end_year()` widened it to cover 1975 -- the year
  # `AGO-1975-2025` starts -- and `pmax(territorial, reported)` let the widened
  # span beat the FAOSTAT map's narrower bound (area 7 reports as ANG through
  # 1974, as AGO from 1975). Area 7 in 1975 then had two candidates, decided by
  # the `polity_start_year DESC` tie-break rather than by the data.
  #
  # #683 closed that by reading upstream's succession relation in BOTH
  # directions: `AGO-1975-2025` names `ANG-1905-1975` as its predecessor, so
  # colonial Angola is not open. The exception is gone, not tolerated, and a
  # new one fails here with the offending pair named.
  out <- whep:::.polity_join_conflicts()

  testthat::expect_equal(out$polity_codes, character(0))
  testthat::expect_equal(nrow(out), 0L)
})

testthat::test_that("a hand-over closes a period and a carve-out does not", {
  # The rule behind the fix, against a fixture so it says what the rule IS
  # rather than what the shipped snapshot happens to hold.
  #
  # `polities$predecessor` carries two different relations. HAND-OVER: the
  # successor begins exactly where the predecessor ends, and the predecessor is
  # over -- that period must not be widened. CARVE-OUT: a piece was split off
  # and the predecessor goes on existing (`TRS-1947-1954` names
  # `ITA-1919-2025`, which still runs to 2025) -- that period IS still open and
  # widening it is what keeps its terminal year resolvable.
  fixture <- data.frame(
    polity_code = c(
      "OLD-1900-1950",
      "NEW-1950-2000",
      "LIVE-1900-2025",
      "PIECE-1960-1970"
    ),
    start_year = c(1900L, 1950L, 1900L, 1960L),
    end_year = c(1950L, 2000L, 2025L, 1970L),
    predecessor = c(NA, "OLD-1900-1950", NA, "LIVE-1900-2025"),
    stringsAsFactors = FALSE
  )

  testthat::expect_equal(
    whep:::.handed_over_polity_codes(fixture),
    "OLD-1900-1950"
  )
})

testthat::test_that("the shipped snapshot's hand-overs include Angola", {
  # And the carve-out predecessors stay open, which is the half a rule without
  # the begin-at-end test would break: `ITA-1919-2025`, `MUS-1800-2025`,
  # `NOR-1800-2025`, `AUT-1919-2025` and `MHL-1874-2025` are all named as
  # somebody's predecessor while still running to their own end. Closing them
  # measurably costs 5 FAOSTAT areas the year 2025, which degrades from
  # `matched` to the `out_of_span` nearest-period stand-in.
  open <- whep:::.open_polity_codes()
  handed <- whep:::.handed_over_polity_codes()

  testthat::expect_true("ANG-1905-1975" %in% handed)
  testthat::expect_false("ANG-1905-1975" %in% open)
  testthat::expect_true(all(
    c(
      "ITA-1919-2025",
      "MUS-1800-2025",
      "NOR-1800-2025",
      "AUT-1919-2025",
      "MHL-1874-2025"
    ) %in%
      open
  ))
})

testthat::test_that("the bucket recovers the polity outside two folds", {
  # The other half of the contract, and the half that fails: `polity_area_code`
  # is a bucket several `area_code` values can share, so keying on it is keying
  # on the polity only where the bucket has one member -- or where its members
  # agree.
  #
  # Measured over 1961-2025, TWO buckets do not, and they are the only two that
  # fold a member at all outside Rest-of-World:
  #
  #   206  Sudan (former) 206 + Sudan 276 + South Sudan 277, three polities in
  #        EVERY reported year -- 65 of them, not just the 15 its periods
  #        overlap in, because the pre-secession years reach 276 and 277 through
  #        the nearest-period stand-in. That is #414.
  #   238  Ethiopia PDR 62 + Ethiopia 238, two polities from 1993 on -- 33
  #        years. Area 62 dissolved in 1993 and its own periods all end there,
  #        so from 1993 it resolves to `ETH-1952-1993` as an out-of-span
  #        stand-in while 238 resolves to `ETH-1993-2025` as a period hit.
  #
  # 238 IS NEW WITH #741, and it is a finding rather than a regression: the
  # crosswalk used to hand dead area 62 the live `ETH-1993-2025`, which made
  # the two members agree by giving one of them a polity the map awards to the
  # other. Removing that row does not create the disagreement, it stops hiding
  # it. Nothing published moves, because area 62 contributes no rows after 1992
  # and `.bucket_area_labels()` labels a bucket from the BUCKET code anyway --
  # verified on a real `get_primary_production()`, where bucket 238 carries
  # `ETH-1952-1993` through 1992 and `ETH-1993-2025` after it, never both.
  #
  # Neither is decided here; both are enumerated so the count can only shrink
  # deliberately.
  out <- whep:::.bucket_year_polity_conflicts()

  testthat::expect_setequal(out$polity_area_code, c(206L, 238L))
  testthat::expect_setequal(
    out$polity_codes,
    c(
      "SDN-2011-2025, SSD-2011-2025, SUD-1956-2011",
      "ETH-1952-1993, ETH-1993-2025"
    )
  )
  testthat::expect_setequal(out$year, 1961:2025)
  testthat::expect_equal(nrow(out), 98L)
  testthat::expect_equal(sum(out$polity_area_code == 206L), 65L)
  testthat::expect_setequal(out$year[out$polity_area_code == 238L], 1993:2025)
})

testthat::test_that("sharing a bucket is not itself a conflict", {
  # The complement, and the false positive that would make the bucket check
  # useless: `options(whep.unfold_rest_of_world = "none")` restores the FABIO
  # fold, putting 21 reporting areas back into bucket 999 -- all of them under
  # ONE polity. Agreement is not ambiguity, so 999 must stay clean and the two
  # real folds must stay the only exceptions under either setting.
  withr::local_options(whep.unfold_rest_of_world = "none")

  out <- suppressWarnings(whep:::.bucket_year_polity_conflicts())

  testthat::expect_false(999L %in% out$polity_area_code)
  testthat::expect_setequal(out$polity_area_code, c(206L, 238L))
})
