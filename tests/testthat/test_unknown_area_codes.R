# Three outcomes a bare `is.na(polity_code)` check cannot tell apart, and the column that can.
#
# A consumer resolving areas wants to know which of these happened:
#
#   the area resolved                      mapping_status "matched" or "manual"
#   the area exists and is deliberately    mapping_status "unmapped" — FAOSTAT 351 "China" is
#     left unmapped                          reported alongside its own components, so mapping it
#                                            would double-count every Chinese figure
#   the area code does not exist           mapping_status NA, because no crosswalk row was found
#
# All three give `polity_code = NA`. Only the third is a bug in the caller's data, and treating a
# typo as a documented non-mapping is how a bad area code survives to the output.
#
# Nothing asserted this and the roxygen described the vocabulary without saying that NA is the
# fourth, unlisted outcome. Pinned here because it is a real affordance that a refactor of the join
# could remove without any test noticing — a left join replaced by an inner one would silently drop
# the unknown rows instead of marking them.

testthat::test_that("resolution distinguishes matched, deliberately unmapped, and unknown", {
  resolved <- as.data.frame(add_polity_code(
    data.frame(
      area_code = c(185L, 351L, 99999L, -7L),
      year = 2000L
    )
  ))
  testthat::expect_true("mapping_status" %in% names(resolved))
  rownames(resolved) <- as.character(resolved$area_code)

  # Matched: a real area with a real polity.
  testthat::expect_equal(resolved["185", "polity_code"], "RUS-1991-2014")
  testthat::expect_true(
    resolved["185", "mapping_status"] %in% c("matched", "manual")
  )

  # Deliberately unmapped: exists, resolves to nothing, and says so.
  testthat::expect_true(is.na(resolved["351", "polity_code"]))
  testthat::expect_equal(resolved["351", "mapping_status"], "unmapped")

  # Unknown: no such area. Distinguished by mapping_status being NA rather than a word.
  for (bad in c("99999", "-7")) {
    testthat::expect_true(is.na(resolved[bad, "polity_code"]))
    testthat::expect_true(is.na(resolved[bad, "mapping_status"]))
  }

  # And the rows must survive: an unknown area code must come back marked, not be dropped.
  testthat::expect_equal(nrow(resolved), 4L)
})
