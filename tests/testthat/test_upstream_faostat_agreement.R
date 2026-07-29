# whep-polities resolves, per FAOSTAT reporting area and year range, which polity
# that area's data belongs to, and publishes the result as
# data/final/faostat_area_polity_map.csv. This package builds the SAME mapping
# independently, by joining area codes to polity families on an ISO3-shaped
# prefix.
#
# Two authorities for one question. These tests compare them.
#
# The good news, and the invariant worth keeping: there are ZERO true conflicts.
# No area has both repos claiming a polity for overlapping years and disagreeing
# about which. Where they differ, this package is less COMPLETE, never
# contradictory — and that gap is baselined below rather than asserted away.
#
# Skips when the upstream checkout is absent, like test_polities_upstream_
# contract.R: an unavailable second opinion is not evidence of agreement.
#
# The upstream map is fingerprinted in polities_manifest.json under
# `faostat_area_map`, so a copy that has drifted is detectable rather than
# silently compared.

# Reads the PUBLISHED map, data/final/faostat_area_polity_map.csv, not the
# matching pipeline's state directory. The state file is working state — its
# columns serve the matcher and nothing promises their shape — whereas the
# published map has a documented column set and is gated by
# scripts/write_faostat_area_map.py --check upstream.
aliases_path <- function() {
  Sys.getenv(
    "WHEP_POLITIES_FAOSTAT_MAP",
    unset = path.expand(
      "~/whep-polities/data/final/faostat_area_polity_map.csv"
    )
  )
}

read_aliases <- function() {
  path <- aliases_path()
  testthat::skip_if_not(
    file.exists(path),
    paste0(
      "upstream FAOSTAT area map not found at ",
      path,
      " — set WHEP_POLITIES_FAOSTAT_MAP or check out the sibling repository"
    )
  )
  a <- utils::read.csv(path, stringsAsFactors = FALSE)
  # The published map contains only resolved mappings, so there is no
  # match_status to filter on: every row is an assertion that this area's data
  # belongs to this polity over this year range.
  a <- a[!is.na(suppressWarnings(as.integer(a$area_code))), ]
  a$area_code <- as.integer(a$area_code)
  a$target_polity_code <- a$polity_code
  a
}

# Areas where upstream names a specific polity but this package folds the area
# into an aggregate (ROW-1850-2023 and the continental "Other" buckets).
#
# 31 -> 27 -> 15. The 12 removed in this round (Reunion 182, Guadeloupe 87,
# Palestine 299, Cook Is. 47, Martinique 135, French Guiana 69, Equatorial Guinea
# 61, Niue 160, Faroes 64, Bermuda 17, Guam 88, Palau 180) now resolve to their
# own polities, matching upstream exactly, as a consequence of the
# `manual_area_prefixes` era resolution rather than of any change here.
#
# This assertion had NEVER RUN. It reads the published map, which lives on
# whep-polities#39 and is therefore absent from the default sibling path, so the
# whole file skipped -- locally and on CI alike. The shrink-side check existed
# precisely so a closed gap could not stay baselined, and it sat inert while
# 12 gaps closed. Pointing WHEP_POLITIES_FAOSTAT_MAP at the branch worktree is
# what surfaced it. One more instance of a skip being invisible in a summary
# line, and the most expensive one so far, because the test was correct and
# simply not executing.
#
# CONSEQUENCE WORTH A MAINTAINER'S ATTENTION: unfolding is a fidelity gain but
# not a free one. 9 of the 12 carry real data -- 73,259 rows of FAOSTAT
# production -- which previously aggregated into Rest of World and now aggregates
# under each territory's own `polity_area_code`. WHEP's ROW total therefore no
# longer equals FABIO's ROW total. The divergence is RECOVERABLE, not lost:
# `fabio_code` is still 999 for all 12, so FABIO-comparable aggregation is a
# group-by away. Row counts: 182 11,970 | 87 10,639 | 299 9,606 | 47 8,717 |
# 135 8,435 | 69 7,870 | 61 7,274 | 160 6,290 | 64 2,458; areas 17, 88 and 180
# carry none.
#
# The remaining 15 are correctly folded and are NOT a defect to clear: they carry
# no CBS data, so routing them individually would diverge from FABIO for no gain.
# Kept baselined so a NEW fold still fails.
folded_into_aggregate <- c(
  5L,
  6L,
  42L,
  65L,
  85L,
  125L,
  140L,
  142L,
  161L,
  187L,
  190L,
  192L,
  205L,
  239L,
  240L
)

# CLOSED. This held the seven areas whose colonial-era polity was unreachable,
# because the crosswalk mapped an area to one prefix while upstream gives the
# colonial and modern polities different ones (Angola ANG then AGO, Sudan SUD
# then SDN, Zimbabwe SRH then ZWE, ...). `manual_area_prefixes` in
# data-raw/table_mappings.R now lists BOTH prefixes for each, so the year-aware
# resolution reaches the right era and the set is empty.
#
# Deliberately kept as an empty vector rather than deleted: the third test
# asserts in both directions, so if a future change reopens any of these the
# failure names the area instead of silently re-baselining it.
different_era <- integer(0)

classify <- function() {
  a <- read_aliases()
  cw <- as.data.frame(whep::polity_area_crosswalk)
  cw <- cw[!is.na(cw$area_code) & !is.na(cw$polity_code), ]
  cw$area_code <- as.integer(cw$area_code)

  out <- lapply(seq_len(nrow(a)), function(k) {
    r <- a[k, ]
    cands <- cw[cw$area_code == r$area_code, ]
    if (nrow(cands) == 0) {
      return(data.frame(area_code = r$area_code, kind = "absent"))
    }
    if (r$target_polity_code %in% cands$polity_code) {
      return(data.frame(area_code = r$area_code, kind = "agree"))
    }
    if (all(!is.na(cands$polity_type) & cands$polity_type == "aggregate")) {
      return(data.frame(area_code = r$area_code, kind = "folded"))
    }
    overlapping <- cands[
      !is.na(cands$polity_start_year) &
        !is.na(cands$polity_end_year) &
        cands$polity_end_year > r$year_start &
        cands$polity_start_year < r$year_end,
    ]
    data.frame(
      area_code = r$area_code,
      kind = if (nrow(overlapping) == 0) "different_era" else "conflict"
    )
  })
  do.call(rbind, out)
}

test_that("no area conflicts with upstream on overlapping years", {
  cls <- classify()
  conflicts <- sort(unique(cls$area_code[cls$kind == "conflict"]))

  # The strong invariant. A conflict means both repos claim a polity for the same
  # area over the same years and name DIFFERENT ones — a genuine contradiction
  # between the source of truth and its consumer, not merely a gap.
  expect_equal(
    length(conflicts),
    0L,
    info = paste0(
      "areas where this package and whep-polities name different polities for ",
      "overlapping years: ",
      paste(conflicts, collapse = ", ")
    )
  )
})

test_that("every upstream-matched area is present in the crosswalk", {
  cls <- classify()
  absent <- sort(unique(cls$area_code[cls$kind == "absent"]))
  expect_equal(
    length(absent),
    0L,
    info = paste0(
      "areas upstream matched to a polity that this crosswalk does not carry ",
      "at all: ",
      paste(absent, collapse = ", ")
    )
  )
})

test_that("the known completeness gaps do not grow", {
  cls <- classify()

  for (kind in c("folded", "different_era")) {
    baseline <- if (kind == "folded") folded_into_aggregate else different_era
    observed <- sort(unique(cls$area_code[cls$kind == kind]))

    # New entries fail: a gap appearing is a regression, whether from an upstream
    # split or a change here.
    expect_equal(
      length(setdiff(observed, baseline)),
      0L,
      info = paste0(
        "NEW ",
        kind,
        " areas not in the baseline: ",
        paste(setdiff(observed, baseline), collapse = ", ")
      )
    )
    # Entries that have been resolved must be removed from the baseline, so it
    # shrinks as the gap closes and cannot quietly license a regression later.
    expect_equal(
      length(setdiff(baseline, observed)),
      0L,
      info = paste0(
        "baseline lists ",
        kind,
        " areas that now resolve correctly — remove ",
        "them: ",
        paste(setdiff(baseline, observed), collapse = ", ")
      )
    )
  }
})

# The three tests above all walk upstream -> shipped: they take each published
# mapping and ask what this package did with it. That direction cannot see the
# failure that matters most for the integration, which is this package asserting
# a polity for an area that upstream never endorsed. A mapping invented here is
# exactly the "areas defined inconsistently, outside the polities database"
# problem the integration exists to remove, and it would pass every check above.
#
# Measured before pinning, and the property is exact rather than approximate:
#
#   213 areas whose crosswalk entry names a NON-AGGREGATE polity
#     0 of them absent from the published map
#    37 areas absent from the published map
#    37 of those resolve to aggregates only (ROW, the continental "Other"
#       buckets, 901-906, and FABIO's 999) — never to a specific polity
#
# The 335 crosswalk (area, polity) pairs that are not in the published map are
# NOT a defect and are not asserted against: the crosswalk carries a family's
# whole era chain per area, while the published map names only the eras that
# carry data. Afghanistan is the plain case — crosswalk AFG-1800-1893,
# AFG-1893-1919, AFG-1919-2025; published AFG-1919-2025 alone. Counting pairs
# would flag 102 areas as disagreements when nothing disagrees, which is why
# this asserts at AREA granularity in one direction and reuses the fold baseline
# in the other.
testthat::test_that("this package invents no area-to-polity mapping upstream does not publish", {
  published <- read_aliases()
  cw <- as.data.frame(whep::polity_area_crosswalk)
  cw <- cw[!is.na(cw$area_code) & !is.na(cw$polity_code), ]
  cw$area_code <- as.integer(cw$area_code)

  is_aggregate <- !is.na(cw$polity_type) & cw$polity_type == "aggregate"
  specific_areas <- unique(cw$area_code[!is_aggregate])
  published_areas <- unique(published$area_code)

  unendorsed <- sort(setdiff(specific_areas, published_areas))
  testthat::expect_equal(
    length(unendorsed),
    0L,
    info = paste0(
      "these areas resolve to a specific polity here while upstream publishes ",
      "no mapping for them at all, so the assignment originates in this ",
      "package rather than in the polities database: ",
      paste(utils::head(unendorsed, 20), collapse = ", ")
    )
  )

  # The converse half: an area upstream declines to map must fall to an
  # aggregate, never to a specific polity picked here.
  unpublished <- setdiff(unique(cw$area_code), published_areas)
  leaked <- sort(unique(unpublished[unpublished %in% specific_areas]))
  testthat::expect_equal(
    length(leaked),
    0L,
    info = paste0(
      "areas with no upstream mapping that nonetheless reach a specific ",
      "polity: ",
      paste(utils::head(leaked, 20), collapse = ", ")
    )
  )
})

# And the mirror of the fold baseline. A published (area, polity) pair the
# crosswalk does not carry means upstream named a polity this package cannot
# route data to. All 17 such pairs today are the deliberate ROW folds already
# baselined in `folded_into_aggregate` above — asserting that containment ties
# the two files together in the direction the earlier tests do not cover, and
# reuses one baseline instead of introducing a second copy that could drift
# from it.
testthat::test_that("published pairs the crosswalk lacks are exactly the baselined folds", {
  published <- read_aliases()
  cw <- as.data.frame(whep::polity_area_crosswalk)
  cw <- cw[!is.na(cw$area_code) & !is.na(cw$polity_code), ]
  cw$area_code <- as.integer(cw$area_code)

  have <- paste(cw$area_code, cw$polity_code)
  missing <- unique(published[
    !paste(published$area_code, published$polity_code) %in% have,
    c("area_code", "polity_code")
  ])
  stray <- sort(unique(setdiff(missing$area_code, folded_into_aggregate)))
  testthat::expect_equal(
    length(stray),
    0L,
    info = paste0(
      "upstream publishes a polity for these areas that the crosswalk cannot ",
      "route to, and they are not among the deliberate folds: ",
      paste(utils::head(stray, 20), collapse = ", ")
    )
  )
})
