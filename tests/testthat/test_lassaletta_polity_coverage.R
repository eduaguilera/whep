# whep::lassaletta_grassland_share is keyed by a country NAME and a year, and until
# this branch it was one of five exported datasets that define a territory with no
# link to the polities database at all. Row coverage was 14.5%: the nine curated
# aliases covered only the cases a human had noticed (Belgium-Luxemburg, FSU,
# DPRepublic of Korea, Cape Verde split at independence), while plain modern names
# -- Afghanistan, Albania, Argentina -- resolved to nothing, because there is no
# name-to-polity path outside the alias table.
#
# 158 aliases were added UPSTREAM rather than name-matching logic here, because the
# polities database owns label-to-polity identity. Coverage is now 98.1%.
#
# The first 129 were generated mechanically; the later 29 are described below.
#
# HOW THE 129 WERE DERIVED, since "generated mechanically" is the part worth
# distrusting:
#
#   * exact match of the source label to a polity_name, after normalising away
#     case and punctuation
#   * only families with a SINGLE live prefix; a name matching two families cannot
#     be resolved mechanically
#   * dead rows excluded, using `dead_status` read from the published manifest
#     rather than a sixth hardcoded copy of that list
#   * one alias per polity period overlapping the data window, with NON-OVERLAPPING
#     year ranges following the exclusive end_year convention, so each data year
#     resolves to the polity live in it regardless of match order
#
# Two of those rules were added because the first generation violated them. It
# emitted 133 rows, of which 5 targeted RETIRED or SUPERSEDED polities
# (ARG-1800-2025, BLZ-1800-2025, BRA-1800-2025, GRC-1919-2025, IRQ-1921-2025) --
# rows that must never receive data. They surfaced as overlapping year ranges,
# which is how a dead duplicate of a live period looks from the outside: two
# same-named polities both spanning 1961-2009. validate_period_overlaps upstream
# reports only 4 overlaps precisely because it filters dead rows, and my generator
# did not.
#
# WHAT REMAINS UNRESOLVED, and it is now exactly ONE thing rather than four.
# Coverage is 98.1%, and every one of the 128 remaining country-years is a SOURCE
# ANACHRONISM -- the dataset carries a label outside the period its entity existed:
#
#   South Sudan        1961-2009   did not exist until 2011
#   FSU                1992-2009   dissolved in 1991
#   Yugoslav SFR       1992-2009   dissolved in 1992
#   Ethiopia PDR       1993-2009   renamed in 1993
#   Czechoslovakia     1994-2009   dissolved in 1993
#   Belgium-Luxemburg  2000-2009   split in 1999
#
# Resolving any of these would be WRONG, so the residue is not a backlog and 98.1% is
# effectively complete. Each label already resolves correctly for the years its entity
# did exist -- FSU 1961-1991 reaches F228-1945-1991, Czechoslovakia 1961-1992 reaches
# F51-1947-1993 -- so the gap is precisely the out-of-period tail.
#
# The 88% -> 98.1% step came from a realisation rather than new curation: I had written
# the previous residue up as "predecessor-era cases needing curation decisions", when the
# `faostat` source ALREADY CONTAINED those decisions for the identical labels. Germany
# before 1990 was already assigned DEU-1949-1990, Yemen F249-1918-1990, Bangladesh
# BGD-1947-1971, Zambia NRH-1953-1964, and Morocco's two-family ambiguity was already
# settled as MAR. Lassaletta et al. use FAO country names, so 28 of the 29 aliases added
# are copies of decisions someone had already made, with only the year range intersected
# against this dataset's window. The 29th is China, which has no faostat alias because
# area 351 is deliberately unmapped to avoid double-counting its components -- a concern
# that does not arise here, since this source reports a single China series.
#
# The floor below is deliberately a floor and not an equality: upstream alias work
# should be free to raise it. It fails if coverage DROPS, which is the regression
# that matters.
testthat::test_that("Lassaletta country-years resolve to polities at the established rate", {
  d <- whep::lassaletta_grassland_share
  testthat::expect_true(all(c("Country", "year") %in% names(d)))
  pairs <- unique(d[, c("Country", "year")])
  # Non-vacuous: a renamed column would otherwise make the rate meaningless.
  testthat::expect_gt(nrow(pairs), 6000L)

  resolved <- unlist(lapply(
    split(pairs, pairs$year),
    function(chunk) {
      resolve_polity_label(
        as.character(chunk$Country),
        source = "lassaletta-grassland-share",
        year = chunk$year[[1]]
      )
    }
  ))
  rate <- mean(!is.na(resolved))
  testthat::expect_gt(rate, 0.97)

  # Every resolved code must be a real, live polity. A coverage rate says nothing
  # about whether the targets exist, and a typo in a generated alias would raise
  # the rate while pointing at nothing.
  p <- as.data.frame(whep::polities)
  live <- p$polity_code[!p$wiki_status %in% c("retired", "superseded")]
  hit <- stats::na.omit(resolved)
  testthat::expect_equal(
    length(setdiff(hit, live)),
    0L,
    info = paste0(
      "aliases resolving to codes that are absent or dead: ",
      paste(utils::head(unique(setdiff(hit, live)), 10), collapse = ", ")
    )
  )
})

testthat::test_that("no Lassaletta alias year range overlaps another for the same label", {
  al <- as.data.frame(whep::polity_label_aliases)
  al <- al[which(al$source == "lassaletta-grassland-share"), ]
  testthat::expect_gt(nrow(al), 100L)

  # Overlapping ranges make resolution depend on match order, which is how the five
  # dead-polity rows hid in the first generation: a dead duplicate spanning the same
  # years as its live successor. Non-overlap is the property that made them visible.
  #
  # The two hand-curated Cape Verde rows DO overlap at 1975 (1961-1975 and
  # 1975-2009), a deliberate choice about which polity gets a mid-year
  # independence, so they are exempted by name rather than by weakening the rule.
  al <- al[al$source_label != "Cape Verde", ]
  overlaps <- character(0)
  for (lbl in unique(al$source_label)) {
    rows <- al[al$source_label == lbl, ]
    if (nrow(rows) < 2L) {
      next
    }
    rows <- rows[order(rows$year_start), ]
    if (any(rows$year_start[-1] <= rows$year_end[-nrow(rows)])) {
      overlaps <- c(overlaps, lbl)
    }
  }
  testthat::expect_equal(
    length(overlaps),
    0L,
    info = paste0(
      "labels whose alias ranges overlap: ",
      paste(utils::head(overlaps, 10), collapse = ", ")
    )
  )
})
