# `polity_area_code` is the numeric area key the builds actually key on —
# get_primary_production() emits it AS its `area_code`, and build_trade.R assigns
# `area_code := polity_area_code` outright. So an area folded at THIS level is folded in the
# output regardless of what `polity_code` says.
#
# That distinction cost a full smoke run to find. The eleven areas pulled out of the FABIO
# rest-of-world fold had their `polity_code` corrected to their own polities, and I reported
# that as done — but `polity_area_code` still took `fabio_code`, which is 999 for every
# folded area. The result: the Faroe Islands' 2,458 raw production rows and Palestine's 9,606
# were still summed into area 999 and attributed to ROW-1850-2023, while the crosswalk
# claimed FRO-1800-2025 and PSE-1948-2025. Two representations of one decision, and only one
# had been fixed.
#
# Nothing caught it, and "0 rows unresolved" actively concealed it: every row DID have a
# reporting polity, because 999 resolves to ROW perfectly well.

testthat::test_that("areas with data of their own keep their own numeric area code", {
  cw <- as.data.frame(whep::polity_area_crosswalk)
  reattributed <- c(17L, 47L, 61L, 64L, 69L, 87L, 135L, 160L, 180L, 182L, 299L)

  for (ac in reattributed) {
    rows <- cw[which(cw$area_code == ac), ]
    testthat::expect_gt(nrow(rows), 0L)
    testthat::expect_true(
      all(rows$polity_area_code == ac),
      info = paste0(
        "area ",
        ac,
        " carries polity_area_code ",
        paste(unique(rows$polity_area_code), collapse = "/"),
        " — folding it at the numeric level puts its data back into the aggregate ",
        "no matter what polity_code says"
      )
    )
  }

  # And the areas that genuinely have no data of their own must STILL fold, so this does not
  # quietly widen into "nothing ever folds".
  still_folded <- c(30L, 152L, 252L, 254L)
  for (ac in still_folded) {
    rows <- cw[which(cw$area_code == ac), ]
    if (nrow(rows) == 0L) {
      next
    }
    testthat::expect_true(
      all(rows$polity_area_code == 999L),
      info = paste0(
        "area ",
        ac,
        " has no data of its own and should still fold to 999"
      )
    )
  }
})
