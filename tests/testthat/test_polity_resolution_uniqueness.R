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
# Currently that filter removes nothing: this vintage of the database holds one
# `superseded` polity and it is not in the crosswalk. It becomes load-bearing when the
# snapshot is refreshed (#485), where 22 of 27 dead polities are candidates. So the
# first test below would pass today even without the fix -- which is why the second
# test exercises the DETECTOR against a fixture instead of against shipped data.
#
# ROUTE 2 -- GENUINELY OVERLAPPING LIVE PERIODS, which no downstream filter can fix.
# Montenegro has `MNE-1913-1915` and `MNE-1913-1918`, both `draft` upstream, both
# covering 1913 and 1914. One is presumably a legacy duplicate that was never retired,
# but deciding which is an upstream question. Filed as lbm364dl/whep-polities#62.
#
# So the third test PINS the known conflict rather than asserting zero. Asserting zero
# would fail today and teach nobody which route regressed; pinning means a new conflict
# fails the test and the pin shrinks when upstream fixes Montenegro.

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

testthat::test_that("the shipped crosswalk's conflicts are the known ones", {
  # PINNED, not asserted to zero -- see the header. Both remaining conflicts are the
  # same upstream shape: a legacy period that was never retired when a finer split
  # replaced it, so two live periods of one family cover the same years. Montenegro is
  # filed upstream as whep-polities issue 62. Peru's `PER-1825-1909` overlaps both
  # `PER-1825-1884` and `PER-1884-1909`, which between them partition the same interval.
  #
  # Both sit BEFORE 1961, so neither is reachable through the upstream FAOSTAT map --
  # they enter as prefix-derived pre-FAOSTAT periods, which is why adopting the map
  # (#517) shrank the conflict set without emptying it. Measured on the same polities
  # vintage: prefix inference alone produced 199 conflicting area-years across five
  # groups (areas 15, 28, 101, 170, 273); the map resolves the three FAOSTAT-era ones
  # and 86 area-years across two groups remain.
  out <- whep:::.area_year_polity_conflicts()

  testthat::expect_setequal(
    unique(paste0(out$area_code, " ", out$polity_codes)),
    c(
      "170 PER-1825-1884, PER-1825-1909",
      "170 PER-1825-1909, PER-1884-1909",
      "273 MNE-1913-1915, MNE-1913-1918"
    )
  )
  testthat::expect_equal(nrow(out), 86L)
})
