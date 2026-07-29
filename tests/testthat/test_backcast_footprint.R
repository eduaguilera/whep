# How much of the data the backcast anchor accounts for, and why that is correct rather than
# alarming.
#
# `add_polity_code(backcast_anchor = 1961L)` matches any earlier year to the polity active in 1961.
# On the real production build that touches a lot:
#
#   6,295,555 rows | 3,634,601 pre-1961 (57.7%) | 1,978,400 carrying a polity whose span
#                                                  EXCLUDES the row's year (31% of all rows)
#   post-1961 rows out of span: 1,822
#
# I measured that and began writing it up as misattribution at scale — 1900 Chinese production on
# CHN-1950-2025, 1880 US production on USA-1959-2025. It is not. The parameter's own documentation
# says why: "WHEP's pre-anchor series are back-cast onto the anchor-year territory rather than
# reported under their data-year borders." The data describes 1961 borders, so the polity active in
# 1961 is the correct target, and matching strictly by data year would attribute 1961-territory
# figures to an entity with different borders.
#
# So the number is the footprint of a deliberate modelling choice, not a defect. It is worth
# pinning anyway, for two reasons: it is large enough that anyone measuring span containment will
# find it and reach my first conclusion, and if the anchor or the back-cast method changes, the
# footprint should move visibly rather than silently.
#
# Uses example data, so it runs without pins; the real-build figures above are recorded for scale.

testthat::test_that("pre-anchor years match the anchor-year polity, by design", {
  # Australia is the clean demonstration: AUS-1800-1901 exists and is never chosen, because 1961
  # falls outside it.
  clamped <- as.data.frame(add_polity_code(
    data.frame(area_code = 10L, year = c(1871L, 1899L, 1950L, 1961L))
  ))
  testthat::expect_true(all(clamped$polity_code == "AUS-1901-2025"))

  # And the documented escape hatch must work, or the choice is not a choice.
  strict <- as.data.frame(add_polity_code(
    data.frame(area_code = 10L, year = c(1871L, 1990L)),
    backcast_anchor = -Inf
  ))
  testthat::expect_equal(
    strict$polity_code[strict$year == 1871L],
    "AUS-1800-1901"
  )
  testthat::expect_equal(
    strict$polity_code[strict$year == 1990L],
    "AUS-1901-2025"
  )
})

testthat::test_that("strict matching reaches polities the anchor makes unreachable", {
  # The reachability consequence, stated as a property rather than a count: turning the anchor off
  # must expose at least some of the 274 crosswalk pairs that are otherwise unusable. If it does
  # not, `backcast_anchor = -Inf` is not doing what its documentation claims.
  cw <- as.data.frame(whep::polity_area_crosswalk)
  areas <- c(10L, 175L, 51L)
  grid <- expand.grid(area_code = areas, year = c(1900L, 1930L, 1950L))

  anchored <- unique(as.data.frame(add_polity_code(grid))$polity_code)
  strict <- unique(
    as.data.frame(
      add_polity_code(grid, backcast_anchor = -Inf)
    )$polity_code
  )

  testthat::expect_true(length(setdiff(strict, anchored)) > 0L)
  # Specifically, the pre-federation Australian row.
  testthat::expect_true("AUS-1800-1901" %in% strict)
  testthat::expect_false("AUS-1800-1901" %in% anchored)
})
