# Pre-1962 production is back-cast onto LUH2 land, and the bridge from an area to its LUH2
# land is the area's ISO3. LUH2 is keyed by MODERN ISO3, while this project periodizes
# territory, so the bridge cannot reach a territory that has no modern code. The build
# warns about it:
#
#   Historical extension: {n} areas have no LUH2 land match; their pre-1962 production is
#   not back-cast.
#
# Measured on 1961-62, which is the first year the extension is asked for: 195 production
# areas, 187 reachable, 8 not — and those 8 carry 11.91% OF PRODUCTION VALUE. This is not
# a rounding gap, and the size is why it is pinned rather than noted.
#
# A FULL-RANGE BUILD SAYS NINE, AND THAT DOES NOT CONTRADICT THIS. Two differences, both
# in what is being counted rather than in what is broken:
#
#   window  the warning fires on every row with year <= 1961, so a full build's "pre-1962"
#           is 1850-1961, where historical sources report territories that no longer exist
#           by 1961. Rebuilt over 1959-1965 the warning says 8, matching this list exactly.
#   space   the warning counts distinct `area` NAMES; this test counts unreachable
#           `polity_area_code`s. An area with no crosswalk row has NA for the code and
#           drops out of a setdiff, while its name survives — so the two need not agree
#           even on one window.
#
# The second difference is the one worth watching: it means this test is blind to exactly
# the areas that are hardest to reach, and a count taken in name space is the stricter
# measure of the two.
#
# Two classes, and only one is the documented one:
#
#   dissolved federations   15 Belgium-Luxembourg, 51 Czechoslovakia, 228 USSR,
#                           248 Yugoslav SFR. LUH2 has no ISO3 for a state that no longer
#                           exists, so the bridge has nothing to match. This is the bulk of
#                           the 11.91%.
#   French departments      69 French Guiana, 87 Guadeloupe, 135 Martinique, 182 Reunion.
#                           LUH2 folds departements into France, so their land is present
#                           but not separable.
#
# A SHORTCUT THAT DOES NOT WORK, recorded so nobody re-derives it: the polities database has
# `successor`, so reconstructing a federation's land as the union of its successors' LUH2
# land looks available. It is not. `successor` points at the next PERIOD OF THE SAME FAMILY
# — F228-1945-1991 gives F228-1991-1992, not the fifteen republics — so the field yields 0
# to 2 codes for these four areas and almost none of their ISO3s are in LUH2. Reaching the
# republics would mean going through the polygons rather than through any code field, which
# is a different piece of work (whep#408).
testthat::test_that("the LUH2 back-cast gap is the eight areas with no modern ISO3 land", {
  testthat::skip_on_ci()
  luh <- tryCatch(whep:::whep_read_file("luh2-areas"), error = function(e) NULL)
  testthat::skip_if(is.null(luh), "luh2-areas pin unavailable")
  prod <- tryCatch(
    suppressWarnings(suppressMessages(
      build_primary_production(start_year = 1961, end_year = 1962)
    )),
    error = function(e) NULL
  )
  testthat::skip_if(is.null(prod), "production pins unavailable")

  luh_iso <- unique(as.character(as.data.frame(luh)$ISO3))
  cw <- as.data.frame(whep::polity_area_crosswalk)
  bridge <- unique(cw[
    which(!is.na(cw$area_iso3c) & !is.na(cw$polity_area_code)),
    c("area_iso3c", "polity_area_code")
  ])
  reachable <- unique(
    bridge$polity_area_code[bridge$area_iso3c %in% luh_iso]
  )

  prod <- as.data.frame(prod)
  areas <- unique(prod$polity_area_code)
  # Non-vacuous: an empty area set would make the setdiff empty and the test pass on
  # nothing.
  testthat::expect_gt(length(areas), 100L)

  unreached <- sort(setdiff(areas, reachable))
  # Pinned by identity, because the point is WHICH areas and why. A ninth would be a new
  # class of territory the bridge cannot see, not more of the same.
  testthat::expect_setequal(
    unreached,
    c(15L, 51L, 69L, 87L, 135L, 182L, 228L, 248L)
  )

  # And the share, because 8 of 195 areas sounds negligible and 11.91% of value is not.
  # A floor and a ceiling: a large move in either direction means the bridge or the data
  # changed and the reasoning above needs rechecking.
  share <- sum(
    prod$value[prod$polity_area_code %in% unreached],
    na.rm = TRUE
  ) /
    sum(prod$value, na.rm = TRUE)
  testthat::expect_gt(share, 0.08)
  testthat::expect_lt(share, 0.16)
})
