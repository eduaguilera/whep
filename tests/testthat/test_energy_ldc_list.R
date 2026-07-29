# .energy_ldc_iso3() is a hardcoded UN Least-Developed-Countries list, used by
# .energy_country_grouping() to assign the GLEAM "development3" grouping. The grouping table is
# built FROM gleam_geographic_hierarchy and then filtered by `iso3 %in% ldc`, so a code in the
# list that the hierarchy does not contain never gets a row at all — its LDC classification is
# unreachable. The list asserts something the code cannot act on.
#
# That is the same defect shape as the five inert aliases upstream: an entry that looks like
# configuration, reads as intent, and does nothing.
#
# Checked both directions. All 46 codes reach a polity, so the list is consistent with the
# polities database. 45 of 46 are in the GLEAM hierarchy; TUV is not, so Tuvalu cannot be
# classified. Tuvalu has negligible livestock, so this costs nothing in practice — but it is
# baselined rather than ignored, because the next inert entry might not be Tuvalu.

testthat::test_that("every hardcoded LDC code can actually be classified", {
  ldc <- whep:::.energy_ldc_iso3()
  hierarchy <- as.data.frame(whep::gleam_geographic_hierarchy)

  testthat::expect_gt(length(ldc), 40L)
  testthat::expect_equal(anyDuplicated(ldc), 0L)

  # Tuvalu is absent from the GLEAM hierarchy, so its LDC entry is inert. Baselined with the
  # reason; bidirectional, so if GLEAM ever gains it this fails and the baseline comes out.
  baseline_inert <- "TUV"

  inert <- sort(setdiff(ldc, hierarchy$iso3))
  testthat::expect_setequal(inert, baseline_inert)
})

testthat::test_that("every hardcoded LDC code names a territory the polities database knows", {
  # The weaker but broader check: whatever GLEAM covers, the list itself must not contain a
  # code that reaches no polity, or it is asserting a classification for a territory this
  # project does not model.
  ldc <- whep:::.energy_ldc_iso3()
  cw <- as.data.frame(whep::polity_area_crosswalk)
  known <- unique(stats::na.omit(c(cw$area_iso3c, cw$iso3_code)))
  known <- known[nzchar(known)]

  unknown <- sort(setdiff(ldc, known))
  testthat::expect_equal(
    length(unknown),
    0L,
    info = paste0(
      "these LDC codes reach no polity at all: ",
      paste(unknown, collapse = ", ")
    )
  )
})
