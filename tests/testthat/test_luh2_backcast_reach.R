# Pre-1962 production is back-cast onto LUH2 land, and the bridge from an area to its LUH2
# land is the area's ISO3. LUH2 is keyed by MODERN ISO3, while this project periodizes
# territory, so the bridge cannot reach a territory that has no modern code. The build
# warns about it:
#
#   Historical extension: {n} areas have no LUH2 land match; their pre-1962 production is
#   not back-cast.
#
# Measured on 1961-62, which is the first year the extension is asked for: 195 production
# areas, 178 reachable, 4 not — and those 4 carry 11.88% OF PRODUCTION VALUE. This is not
# a rounding gap, and the size is why it is pinned rather than noted.
#
# A FULL-RANGE BUILD USED TO SAY NINE, and chasing that down is what fixed it. The
# warning counts distinct `area` NAMES while this test counts unreachable
# `polity_area_code`s, and the ninth name was a second SPELLING of one of these eight:
# `.aggregate_to_polities()` labelled a reporting bucket with one of its folded members'
# polity names, so the same area could appear as "Czechoslovakia" and as
# "Czechoslovakia (1947-1993)" depending on which rows reached the warning. Naming the
# bucket from the bucket removed the duplicate, and a full-range build now reports eight,
# with the same identities pinned below.
#
# The difference in KIND remains and is worth watching: this test works in code space,
# where an area with no crosswalk row has NA and drops out of a setdiff, while the
# warning works in name space and keeps it. A name-space count is therefore the stricter
# of the two, and this test is blind to exactly the areas hardest to reach.
#
# Measured at full range after whep#425 was fixed: `get_wide_cbs()` completes with ~2.00M rows
# -- 1,999,609 twice in one session and 1,997,944 in another, so it still moves across sessions
# under whep#420, by 0.08% where the corrupted frame moved 0.148%. Zero NA across all four
# polity columns. This warning names FIVE areas in name space against the four this test finds
# in code space -- the difference in kind described above.
#
# The row count fell from ~2.766M because the fix restored quantities: counts are never zero, so
# the `value != 0` filter had nothing to remove until then. Do not read the drop as lost data.
#
# ONE class now, and the second one leaving is a consequence rather than a fix:
#
#   dissolved federations   15 Belgium-Luxembourg, 51 Czechoslovakia, 228 USSR,
#                           248 Yugoslav SFR. LUH2 has no ISO3 for a state that no longer
#                           exists, so the bridge has nothing to match. All of the 11.88%.
#
# The French departments -- 69 French Guiana, 87 Guadeloupe, 135 Martinique, 182 Reunion --
# used to form a second class, because this branch promoted them out of FABIO's rest-of-world
# bucket and they then appeared as areas in their own right, unreachable because LUH2 folds
# departements into France. That promotion is withdrawn (whep#419: it inflated global feed
# 13.7x), so they fold into 999 again and are no longer separate areas here. Their land is
# still not separable in LUH2; they are simply no longer counted at this level.
#
# A SHORTCUT THAT DOES NOT WORK, recorded so nobody re-derives it: the polities database has
# `successor`, so reconstructing a federation's land as the union of its successors' LUH2
# land looks available. It is not. `successor` points at the next PERIOD OF THE SAME FAMILY
# — F228-1945-1991 gives F228-1991-1992, not the fifteen republics — so the field yields 0
# to 2 codes for these four areas and almost none of their ISO3s are in LUH2. Reaching the
# republics would mean going through the polygons rather than through any code field, which
# is a different piece of work (whep#408).
testthat::test_that("the LUH2 back-cast gap is the four dissolved federations", {
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
    c(15L, 51L, 228L, 248L)
  )

  # And the share, because 4 of 182 areas sounds negligible and 11.88% of value is not.
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
