# Two independent representations of "which continent is this area in":
#
#   the vendored `region_UN` column in inst/extdata/harmonization/regions_full.csv
#   the `continent` field of the polity the area reports as, carried through
#     polity_area_crosswalk and now surfaced as `reporting_polity_continent`
#
# Nothing in the package reads `region_UN` — it appears only in roxygen — so the
# disagreements below were invisible. Comparing the two found one genuine defect in
# the vendored table and quantified a known modelling gap.
#
# The rule this test encodes: a disagreement is acceptable ONLY when the area folds
# into a rest-of-world aggregate, because a ROW polity's continent is legitimately
# "World" while `region_UN` still names the area's real continent. Every other
# disagreement is a defect. Expressed as a derived condition rather than a list of
# area codes, so newly-folded or newly-unfolded areas are handled without an edit.
#
# `region_UN == "RoW"` rows are excluded from comparison for the mirror-image reason:
# there the vendored side is the aggregate and the polity side may be specific.

testthat::test_that("vendored region_UN agrees with the polity's own continent", {
  # The one non-fold disagreement, and why it is upstream-correct:
  #
  #   area 228 "USSR" -> F228-1945-1991. region_UN says Asia; the polity says
  #   Europe. Area 185 "Russian Federation" — the SAME territorial family, and the
  #   direct successor — is filed under Europe in this very table, so the vendored
  #   column contradicts itself. Upstream is consistent: all twelve F228/RUS periods
  #   from the Russian Empire to modern Russia are Europe.
  #
  # Baselined rather than fixed in place because `region_UN` is a vendored
  # harmonization table no code reads; the integration's answer is
  # `reporting_polity_continent`, which carries Europe correctly. Bidirectional — if
  # the vendored cell is ever corrected this fails and the baseline comes out.
  baseline_disagreements <- 228L

  d <- as.data.frame(whep::regions_full)
  testthat::expect_true("reporting_polity_continent" %in% names(d))

  fold <- function(x) {
    # region_UN uses UN M49 macro-regions, which merge the Americas; the polities
    # database splits North and South. Fold the finer side to compare.
    ifelse(x %in% c("North America", "South America"), "Americas", x)
  }

  cmp <- d[
    !is.na(d$reporting_polity_code) &
      !is.na(d$reporting_polity_continent) &
      nzchar(d$reporting_polity_continent) &
      !is.na(d$region_UN) &
      d$region_UN != "RoW",
  ]
  testthat::expect_gt(nrow(cmp), 200L)

  disagree <- cmp[fold(cmp$reporting_polity_continent) != cmp$region_UN, ]

  # Every disagreement must be explained by the ROW fold...
  unexplained <- disagree[
    disagree$reporting_polity_continent != "World" &
      !disagree$code %in% baseline_disagreements,
  ]
  testthat::expect_equal(
    nrow(unexplained),
    0L,
    info = paste0(
      "areas whose vendored region_UN contradicts their polity's continent for ",
      "no reason the ROW fold explains: ",
      paste(
        utils::head(paste0(unexplained$code, " (", unexplained$name, ")"), 10),
        collapse = ", "
      )
    )
  )

  # ...and the baselined one must still disagree, or the baseline is stale.
  still <- intersect(baseline_disagreements, disagree$code)
  testthat::expect_setequal(still, baseline_disagreements)
})

testthat::test_that("ROW-folded areas are the only bulk source of continent divergence", {
  # Quantifies the fold rather than asserting a magic number: areas whose reporting
  # polity is a rest-of-world aggregate lose their real continent, which is the
  # substance of the open decision to move them onto their own polities. When that
  # move lands, `folded` drops and this test's lower bound is what needs relaxing —
  # it exists so the change is noticed rather than absorbed silently.
  d <- as.data.frame(whep::regions_full)
  folded <- d[
    !is.na(d$reporting_polity_continent) &
      d$reporting_polity_continent == "World" &
      !is.na(d$region_UN) &
      d$region_UN != "RoW",
  ]
  testthat::expect_true(all(grepl("^ROW-", folded$reporting_polity_code)))
  # Their real continents are known and recoverable from region_UN, which is what
  # makes the move mechanical rather than a research task.
  testthat::expect_true(all(
    folded$region_UN %in%
      c("Africa", "Americas", "Asia", "Europe", "Oceania")
  ))
  testthat::expect_gt(nrow(folded), 40L)
})
