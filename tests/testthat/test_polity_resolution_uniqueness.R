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
  out <- whep:::.area_year_polity_conflicts()

  testthat::expect_equal(unique(out$polity_codes), character(0))
  testthat::expect_equal(nrow(out), 0L)
})
