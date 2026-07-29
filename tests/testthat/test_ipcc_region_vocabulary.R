# Livestock manure and enteric coefficients are chosen by IPCC region, and an area reaches one
# through .add_ipcc_region(): iso3 -> gleam_geographic_hierarchy$gleam_region -> a hardcoded
# GLEAM-to-IPCC crosswalk -> `region`. Three things can go silently wrong along that chain, and
# all three end the same way — a coefficient of NA, applied to real animals.
#
# Checked, and the path is sound today. This locks it:
#
#   1. Every region the crosswalk can PRODUCE must exist in the coefficient table. A region
#      spelled one way in the crosswalk and another in the table joins to nothing.
#   2. Every region in the table should be producible, or it is dead weight nobody can reach.
#   3. `Global` must cover EVERY category, because that is the fallback for areas GLEAM does not
#      cover — and it does not cover 59 of the 265 areas that have an iso3. Almost all are tiny
#      dependencies with no livestock (Andorra, Bouvet Island, Holy See), but Bermuda is among
#      them and it reports data. If a category ever lacks a Global row, those areas get NA
#      instead of a coarser answer.
#
# Point 3 is the one worth having a test for: the fallback is invisible when it works and
# invisible when it does not.

testthat::test_that("the IPCC region vocabulary agrees between code and coefficients", {
  nex <- as.data.frame(whep::ipcc_2019_n_excretion)
  testthat::expect_true(all(c("region", "category") %in% names(nex)))

  # The regions .add_ipcc_region() can emit, read from the function's own crosswalk rather
  # than retyped, so a change there is picked up here.
  produced <- sort(unique(stats::na.omit(
    whep:::.add_ipcc_region(
      data.frame(iso3 = unique(whep::gleam_geographic_hierarchy$iso3))
    )$region
  )))
  testthat::expect_gt(length(produced), 5L)

  in_table <- sort(setdiff(unique(nex$region), "Global"))
  testthat::expect_setequal(produced, in_table)
})

testthat::test_that("the Global fallback covers every coefficient category", {
  nex <- as.data.frame(whep::ipcc_2019_n_excretion)
  categories <- unique(nex$category)
  global <- unique(nex$category[nex$region == "Global"])
  testthat::expect_gt(length(categories), 5L)

  uncovered <- sort(setdiff(categories, global))
  testthat::expect_equal(
    length(uncovered),
    0L,
    info = paste0(
      "these categories have no Global row, so every area GLEAM does not cover — 59 of the ",
      "265 with an iso3, including Bermuda, which reports data — gets NA rather than a ",
      "coarser coefficient: ",
      paste(uncovered, collapse = ", ")
    )
  )
})
