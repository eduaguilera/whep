# Pre-1961 years resolve through the backcast clamp, and the consequence is easy to misread.
#
# `backcast_anchor = 1961L` clamps any earlier year before matching, because FAOSTAT's series
# begin there. So a pre-1961 year does NOT resolve to the polity that held the territory in that
# year — it resolves to whichever polity was live in 1961.
#
# Two areas of the same shape make the trap concrete, and I walked into it before writing this:
#
#   area 10  Australia    1899 -> AUS-1901-2025, whose span STARTS in 1901. AUS-1800-1901 exists
#                         and is in the crosswalk, and is nevertheless unreachable for any year,
#                         because 1961 is not inside it.
#   area 175 Guinea-Bissau 1899 -> GNB-1886-1974, which DOES contain 1899 and so looks
#                         era-correct. It is not era-correct; it is the 1961 answer, and 1961
#                         happens to fall inside the same colonial span.
#
# Seeing those two side by side, I concluded the resolver was inconsistent and hypothesised a
# status-based tie-break — area 10's colonial row is `draft` while its modern row is `reviewed`.
# Wrong: there is no wiki_status reference in the resolver at all. The clamp is uniform, and the
# apparent inconsistency is a coincidence of which spans contain 1961.
#
# Asserting both cases is the point. A test covering only Guinea-Bissau would read as evidence
# that pre-1961 years resolve to the historical polity, which is exactly backwards.

testthat::test_that("pre-1961 years resolve to the 1961 polity, not the era's", {
  clamped <- as.data.frame(add_polity_code(
    rbind(
      data.frame(area_code = 10L, year = c(1899L, 1961L)),
      data.frame(area_code = 175L, year = c(1899L, 1961L))
    )
  ))

  # The 1899 answer must equal the 1961 answer for both areas. That equality IS the clamp.
  for (area in c(10L, 175L)) {
    rows <- clamped[clamped$area_code == area, ]
    testthat::expect_equal(
      rows$polity_code[rows$year == 1899L],
      rows$polity_code[rows$year == 1961L]
    )
  }

  # And the shape of the trap: one area's answer excludes the requested year, the other's
  # includes it, from the same rule.
  aus <- clamped$polity_code[clamped$area_code == 10L & clamped$year == 1899L]
  gnb <- clamped$polity_code[clamped$area_code == 175L & clamped$year == 1899L]
  testthat::expect_equal(aus, "AUS-1901-2025")
  testthat::expect_equal(gnb, "GNB-1886-1974")

  pol <- as.data.frame(whep::polities)
  span <- function(code) {
    c(
      pol$start_year[pol$polity_code == code],
      pol$end_year[pol$polity_code == code]
    )
  }
  a <- span(aus)
  g <- span(gnb)
  testthat::expect_false(1899L >= a[1] && 1899L < a[2])
  testthat::expect_true(1899L >= g[1] && 1899L < g[2])
})

testthat::test_that("the resolver does not rank polities by editorial status", {
  # The hypothesis this test exists to kill. Area 10 has a `draft` colonial row and a `reviewed`
  # modern one, which invites the conclusion that review status decides. It does not, and it must
  # not: whether a wiki page has been reviewed says nothing about which years the polity covers.
  resolver <- paste(
    deparse(body(whep::add_polity_code)),
    paste(deparse(body(whep:::.add_polity_columns_dt)), collapse = " "),
    collapse = " "
  )
  testthat::expect_false(grepl("wiki_status|reviewed", resolver))
})
