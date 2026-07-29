# `has_geometry` is a derived flag, and consumers use it to decide whether a spatial operation is
# possible at all — including this package's own polygon-gap assertions and upstream's
# `polygon_gap_polity_codes`. A flag that has drifted from the geometry it describes is worse than no
# flag: code guarded by it would skip rows that do have geometry, or attempt rows that do not.
#
# Two copies exist, in `polities` and in `polity_area_crosswalk`, and the truth is a third thing —
# whether the geometry column is actually non-empty. Checked all three: 0 disagreements across 740
# rows and 544 crosswalk pairs. That is a clean result today and exactly the kind that stops being
# true silently, since nothing recomputes the flag once it is written.

testthat::test_that("has_geometry matches the geometry it describes", {
  testthat::skip_if_not_installed("sf")
  p <- whep::polities
  geom <- sf::st_geometry(p)
  actual <- !sf::st_is_empty(geom) & !is.na(sf::st_dimension(geom))

  testthat::expect_true("has_geometry" %in% names(p))
  testthat::expect_gt(sum(actual), 500L)

  mismatched <- which(p$has_geometry != actual)
  testthat::expect_equal(
    length(mismatched),
    0L,
    info = paste0(
      "has_geometry disagrees with the actual geometry for: ",
      paste(utils::head(p$polity_code[mismatched], 8), collapse = ", ")
    )
  )
})

testthat::test_that("the crosswalk's copy of has_geometry agrees with polities'", {
  testthat::skip_if_not_installed("sf")
  p <- whep::polities
  geom <- sf::st_geometry(p)
  truth <- data.frame(
    polity_code = p$polity_code,
    actual = !sf::st_is_empty(geom) & !is.na(sf::st_dimension(geom)),
    stringsAsFactors = FALSE
  )
  cw <- unique(as.data.frame(whep::polity_area_crosswalk)[,
    c("polity_code", "has_geometry")
  ])
  cw <- cw[!is.na(cw$polity_code), ]
  both <- merge(cw, truth, by = "polity_code")
  testthat::expect_gt(nrow(both), 400L)

  differing <- both[both$has_geometry != both$actual, ]
  testthat::expect_equal(
    nrow(differing),
    0L,
    info = paste0(
      "the crosswalk's has_geometry disagrees with the geometry for: ",
      paste(utils::head(differing$polity_code, 8), collapse = ", ")
    )
  )
})
