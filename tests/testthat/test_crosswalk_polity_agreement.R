# The defect that started this whole integration was 24 FAOSTAT area codes resolving to
# RETIRED or SUPERSEDED polities — data attributed to rows that had been withdrawn.
# test_polities_upstream_contract.R asserts that against the published manifest, and
# therefore SKIPS wherever the sibling checkout is absent, which includes CI. So the
# original defect has been guarded only on machines that happen to have whep-polities
# checked out.
#
# It does not need the manifest. `polity_area_crosswalk` carries `polity_code` and
# `polities` carries `wiki_status`, both embedded in this package, so the check is a join
# over data that always ships. These tests therefore run unconditionally.
#
# They also compare the crosswalk's OWN `wiki_status` column against `polities`', because
# the two are independent copies of the same fact written by the same build — and two
# copies of one fact with nothing comparing them is how the manifest came to call
# GCO-1884-2025 dead while the GeoPackage consumers read still called it live.

test_that("no crosswalk row resolves to a retired or superseded polity", {
  cw <- as.data.frame(whep::polity_area_crosswalk)
  pol <- as.data.frame(whep::polities)

  # Non-vacuous on both sides: there must be rows to check, and dead rows to check
  # against, or "no dead routing" is true for uninteresting reasons.
  expect_gte(nrow(cw), 500L)
  dead <- pol$polity_code[pol$wiki_status %in% c("retired", "superseded")]
  expect_gte(length(dead), 1L)

  routed <- sort(unique(stats::na.omit(cw$polity_code[
    cw$polity_code %in% dead
  ])))
  expect_equal(
    length(routed),
    0L,
    info = paste0(
      "area codes resolve to withdrawn polities: ",
      paste(utils::head(routed, 10), collapse = ", "),
      " — `retired` means the row was withdrawn and `superseded` that it was split or ",
      "merged into finer rows; either way it must never receive data."
    )
  )
})

test_that("every crosswalk polity_code exists in polities", {
  cw <- as.data.frame(whep::polity_area_crosswalk)
  pol <- as.data.frame(whep::polities)

  unknown <- sort(setdiff(
    unique(stats::na.omit(cw$polity_code)),
    pol$polity_code
  ))
  expect_equal(
    length(unknown),
    0L,
    info = paste0(
      "crosswalk resolves to codes absent from polities: ",
      paste(utils::head(unknown, 5), collapse = ", "),
      " — the two datasets were built from different upstream revisions."
    )
  )
})

test_that("the crosswalk's wiki_status agrees with polities'", {
  cw <- as.data.frame(whep::polity_area_crosswalk)
  pol <- as.data.frame(whep::polities)
  testthat::skip_if_not("wiki_status" %in% names(cw))

  both <- merge(
    unique(cw[!is.na(cw$polity_code), c("polity_code", "wiki_status")]),
    pol[, c("polity_code", "wiki_status")],
    by = "polity_code",
    suffixes = c("_cw", "_pol")
  )
  expect_gte(nrow(both), 100L)

  differing <- both[both$wiki_status_cw != both$wiki_status_pol, ]
  expect_equal(
    nrow(differing),
    0L,
    info = paste0(
      "the crosswalk and polities disagree about wiki_status for: ",
      paste(utils::head(differing$polity_code, 5), collapse = ", "),
      " — one of the two .rda files is stale."
    )
  )
})
