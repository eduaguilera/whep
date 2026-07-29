# The `polities` roxygen listed a `polygon_status` vocabulary that no longer existed and described
# `end_year` as inclusive when it is exclusive. Both are the kind of error that produces silence
# rather than a complaint: a filter on `polygon_status == "missing"` returns zero rows, and one on
# `year <= end_year` double-counts every boundary year.
#
# Documentation cannot be tested directly, but the FACTS it asserts can be. If the vocabulary grows
# a value or the interval convention changes, these fail and the roxygen gets revisited — which is
# what did not happen when the vocabulary was migrated on this branch.

testthat::test_that("the documented polygon_status vocabulary is the actual one", {
  documented <- c(
    "assigned",
    "proxy",
    "estimate",
    "polygon_vintage_drift",
    "unassigned"
  )
  actual <- sort(unique(stats::na.omit(
    as.data.frame(whep::polities)$polygon_status
  )))
  testthat::expect_setequal(actual, documented)
})

testthat::test_that("end_year is exclusive, as the documentation now says", {
  # The behaviour the roxygen described backwards. Adjacent periods settle it.
  r <- as.data.frame(add_polity_code(
    data.frame(area_code = 185L, year = c(2013L, 2014L))
  ))
  testthat::expect_equal(r$polity_code, c("RUS-1991-2014", "RUS-2014-2025"))
})

testthat::test_that("polygon_area_km2 is sparse, as documented", {
  # Asserted so the "compute it from geom instead" advice cannot silently become wrong advice: if
  # the field ever becomes dense, the documentation should stop warning readers off it.
  p <- as.data.frame(whep::polities)
  recorded <- sum(!is.na(p$polygon_area_km2))
  testthat::expect_lt(recorded / nrow(p), 0.5)
  # And it must not be empty either, or the cross-check it exists for has nothing to work with.
  testthat::expect_gt(recorded, 50L)
})
