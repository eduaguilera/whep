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

testthat::test_that("every ISO3 literal in the energy grouping can be acted on", {
  # Generalises the LDC check to the whole function rather than the one list inside it.
  # .energy_country_grouping() also hardcodes nine codes for the GLEAM "detailed15" scheme —
  # USA, CAN, AUS, JPN, KOR, NZL, RUS, TUR, ISR — and each carries the same risk: a code the
  # hierarchy lacks is a branch that can never be taken.
  #
  # Read from the FUNCTION BODY, not from R/energy_co2_extension.R, so this keeps working in an
  # installed package where there is no R/ directory, and so a literal added anywhere in the
  # function is covered without editing this test.
  code <- paste(deparse(body(whep:::.energy_country_grouping)), collapse = " ")
  literals <- unique(unlist(regmatches(
    code,
    gregexpr('"[A-Z]{3}"', code)
  )))
  literals <- gsub('"', "", literals)
  # Non-vacuous, with the floor set from what this function actually contains. I first wrote 40
  # here on the assumption that the LDC list was inline; it is not — it lives in
  # .energy_ldc_iso3(), which this function CALLS, so the body holds only the nine detailed15
  # codes. The LDC entries are covered by the two tests above.
  testthat::expect_gte(length(literals), 9L)

  hierarchy <- as.data.frame(whep::gleam_geographic_hierarchy)
  cw <- as.data.frame(whep::polity_area_crosswalk)
  known_iso <- unique(stats::na.omit(c(cw$area_iso3c, cw$iso3_code)))
  known_iso <- known_iso[nzchar(known_iso)]

  # Tuvalu is the one baselined inert entry — see above.
  inert <- sort(setdiff(literals, c(hierarchy$iso3, "TUV")))
  testthat::expect_equal(
    length(inert),
    0L,
    info = paste0(
      "these codes appear in the grouping logic but not in gleam_geographic_hierarchy, so ",
      "their branch can never be taken: ",
      paste(inert, collapse = ", ")
    )
  )

  unknown <- sort(setdiff(literals, known_iso))
  testthat::expect_equal(
    length(unknown),
    0L,
    info = paste0(
      "these codes appear in the grouping logic but reach no polity: ",
      paste(unknown, collapse = ", ")
    )
  )
})
