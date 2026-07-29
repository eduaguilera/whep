# A reporting aggregate must cover the years it aggregates.
#
# `end_year` is EXCLUSIVE — area 185 in 2014 resolves to RUS-2014-2025, not
# RUS-1991-2014 — so a polity ending in 2023 has 2022 as its last valid year. The six
# regional "Other" buckets and Rest of World all end before FAOSTAT stops reporting them,
# so their most recent years resolve to nothing at all:
#
#   RLAM-1850-2013   Latin America Other, area 904        11 years unassigned (2014-2024)
#   RAFR/RASI/REUR/RNAM/ROCE-1850-2021, areas 901-906      3 years each      (2022-2024)
#   ROW-1850-2023    plus ~46 areas folded into it         1 year each       (2024)
#
# These are statistical reporting buckets, not historical states, and their end years look
# like artefacts of when the rows were written rather than any real dissolution. That is
# what separates them from the aggregates that legitimately stop: ANT-1961-2010 (the
# Netherlands Antilles dissolved in 2010) and BLX-*-1999 (the Belgium-Luxembourg economic
# union ended) SHOULD stop, and FAOSTAT continuing to report those areas afterwards is the
# source's business, not a gap in ours.
#
# Baselined bidirectionally, so extending the buckets upstream trips this test and the
# baseline comes out — the list can only shrink deliberately.

testthat::test_that("current reporting aggregates cover the reported data years", {
  # Areas whose unresolved years are expected, with the reason.
  #
  #   351  China, the mainland+HK+Macao+Taiwan aggregate, deliberately unmapped so it
  #        cannot double-count its own components
  #    15  Belgium-Luxembourg, dissolved 1999
  #   151  Netherlands Antilles, dissolved 2010
  expected_unresolved <- c(351L, 15L, 151L)

  # Areas unresolved only because a live reporting bucket's end year predates the data.
  # These are the defect, baselined until the buckets are extended upstream.
  bucket_shortfall <- c(901L, 902L, 903L, 904L, 905L, 906L)

  cw <- as.data.frame(whep::polity_area_crosswalk)
  areas <- sort(unique(cw$area_code[!is.na(cw$area_code)]))
  grid <- expand.grid(area_code = areas, year = 1961:2024)
  resolved <- as.data.frame(add_polity_code(grid))
  # Guard the sweep: a silently empty resolution would make everything below pass.
  testthat::expect_gt(sum(!is.na(resolved$polity_code)), 15000L)

  unresolved <- resolved[is.na(resolved$polity_code), ]
  by_area <- table(unresolved$area_code)
  affected <- as.integer(names(by_area))

  # Areas losing only 2024 are the ROW-fold tail: ROW-1850-2023's last valid year is
  # 2022, so 2023 is already out of span and 2024 falls off entirely. Derived from the
  # data rather than listed, because which areas fold changes.
  row_tail <- affected[by_area == 1L]
  accounted <- c(expected_unresolved, bucket_shortfall, row_tail)

  unexplained <- setdiff(affected, accounted)
  testthat::expect_equal(
    length(unexplained),
    0L,
    info = paste0(
      "areas with unresolved data years for no known reason: ",
      paste(utils::head(sort(unexplained), 8), collapse = ", ")
    )
  )

  # Bidirectional: each baselined bucket must STILL be short, so extending it upstream
  # fails here rather than passing quietly.
  testthat::expect_true(
    all(bucket_shortfall %in% affected),
    info = paste0(
      "a reporting bucket no longer loses years — remove it from bucket_shortfall: ",
      paste(setdiff(bucket_shortfall, affected), collapse = ", ")
    )
  )
})

testthat::test_that("polity end years are exclusive", {
  # The convention this whole file depends on, asserted rather than assumed. Adjacent
  # periods settle it: 2014 belongs to the row that STARTS in 2014.
  r <- as.data.frame(add_polity_code(
    data.frame(area_code = 185L, year = c(2013L, 2014L))
  ))
  testthat::expect_equal(r$polity_code, c("RUS-1991-2014", "RUS-2014-2025"))
})
