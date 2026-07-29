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

test_that("the crosswalk agrees with polities on EVERY identity field", {
  # The upstream manifest declares eight identity fields — polity_code, polity_name,
  # start_year, end_year, polity_type, iso3_code, cow_code, wiki_status — chosen precisely
  # because drift in any of them invalidates a downstream copy. The check below covered ONE
  # of the eight.
  #
  # Both embedded objects carry all eight (the crosswalk spells two of them
  # `polity_start_year` / `polity_end_year`), and they are built by separate steps from the
  # same upstream. So comparing them detects a partial rebuild — data/polities.rda regenerated
  # while data/polity_area_crosswalk.rda was not, or the reverse — which is the drift class
  # this whole branch exists to prevent.
  #
  # This matters most because the test that DOES compare all eight fields against upstream
  # lives in test_polities_upstream_contract.R, and that file is one of the eight [upstream]
  # skips: whep-polities is private, so CI cannot clone it. This comparison needs nothing
  # outside the package, so it runs everywhere.
  cw <- as.data.frame(whep::polity_area_crosswalk)
  pol <- as.data.frame(whep::polities)

  # crosswalk column -> polities column. Named rather than derived, because the two year
  # fields are deliberately prefixed in the crosswalk to distinguish them from an area's own
  # years, and guessing at that mapping would be the kind of cleverness that breaks quietly.
  fields <- c(
    polity_name = "polity_name",
    polity_start_year = "start_year",
    polity_end_year = "end_year",
    polity_type = "polity_type",
    iso3_code = "iso3_code",
    cow_code = "cow_code",
    wiki_status = "wiki_status"
  )
  present <- fields[
    names(fields) %in% names(cw) & unname(fields) %in% names(pol)
  ]
  # Non-vacuous: if the crosswalk stops carrying these columns this must fail, not pass by
  # comparing nothing.
  expect_gte(length(present), 6L)

  left <- unique(cw[!is.na(cw$polity_code), c("polity_code", names(present))])
  both <- merge(
    left,
    pol[, c("polity_code", unname(present))],
    by = "polity_code"
  )
  expect_gt(nrow(both), 400L)

  problems <- character()
  for (i in seq_along(present)) {
    cw_col <- names(present)[i]
    pol_col <- unname(present)[i]
    a <- both[[if (cw_col == pol_col) paste0(cw_col, ".x") else cw_col]]
    b <- both[[if (cw_col == pol_col) paste0(pol_col, ".y") else pol_col]]
    differing <- !((is.na(a) & is.na(b)) |
      (!is.na(a) & !is.na(b) & as.character(a) == as.character(b)))
    if (any(differing)) {
      problems <- c(
        problems,
        paste0(
          pol_col,
          ": ",
          sum(differing),
          " row(s), e.g. ",
          paste(utils::head(both$polity_code[differing], 3), collapse = ", ")
        )
      )
    }
  }
  expect_equal(
    length(problems),
    0L,
    info = paste0(
      "the crosswalk and polities disagree on identity fields, which means one was ",
      "regenerated from upstream and the other was not: ",
      paste(problems, collapse = " | ")
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
