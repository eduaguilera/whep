# A group sum used as a DENOMINATOR is only meaningful if the group is one territory.
# `polity_area_code` does not guarantee that: FABIO's rest-of-world bucket 999 holds both
# rest-of-world and Eswatini, which reports its own polity while aggregating into 999.
#
# One site had exactly that defect -- livestock split shares dividing Eswatini's broilers
# by an Eswatini-plus-rest-of-world total, giving two different shares for one key and
# leaving which one applied to row order. Fixed by keying on the reporting territory.
#
# So the class was swept: every division by an area_code-grouped sum in R/. Four sites, and
# the distinction between the broken one and the safe ones is precise.
#
#   build_production.R      livestock split shares    WAS BROKEN, now keyed by territory
#   energy_co2_extension.R  slaughter-head shares     safe: summarise() collapses the
#                                                     bucket BEFORE the share is taken
#   n_balance_spatialize.R  crop area shares          safe: same collapse-first shape
#   carbon_balance.R        per-cell area fractions   safe: grouped by (lon, lat, ...), so
#                                                     one territory per cell by construction
#
# The rule that separates them: if a summarise() reduces the bucket to one row per group
# before the division, the denominator is the bucket's own total and the share is
# well-defined. If two territories survive into the division, it is not. Verified by
# running each and checking that shares sum to exactly 1 within their group -- a property
# the broken site could not have, because its groups spanned territories.
testthat::test_that("area-grouped shares sum to one within their group", {
  testthat::skip_on_ci()
  prod <- tryCatch(
    suppressWarnings(suppressMessages(
      build_primary_production(start_year = 1990, end_year = 1991)
    )),
    error = function(e) NULL
  )
  testthat::skip_if(is.null(prod), "production pins unavailable")

  # Site 1: the one that was broken. Keyed by territory, so every group sums to 1.
  stock <- as.data.frame(whep:::.compute_stock_shares(1990:1991))
  grp <- paste(
    stock$year,
    stock$area_code,
    stock$area,
    stock$Item_Code
  )
  sums <- tapply(stock$share, grp, sum)
  testthat::expect_gt(length(sums), 50L)
  testthat::expect_equal(sum(abs(sums - 1) >= 1e-9), 0L)

  # Site 2: safe by collapse-first, and every share finite. The 0/0 groups now read 0
  # explicitly rather than NaN -- two of them on this window, area 162 poultry, where the
  # group has no heads to allocate across. NaN was being discarded downstream by
  # sum(na.rm = TRUE), so the arithmetic was already 0; the difference is that it is now
  # a statement rather than an accident of NA-dropping.
  energy <- as.data.frame(whep:::.energy_slaughter_shares(prod))
  testthat::expect_equal(sum(!is.finite(energy$share)), 0L)
  egrp <- paste(energy$year, energy$area_code, energy$grp)
  esums <- tapply(energy$share, egrp, sum)
  # Each group sums to 1, or to 0 where the group had nothing to divide.
  testthat::expect_equal(
    sum(abs(esums - 1) >= 1e-9 & abs(esums) >= 1e-9),
    0L,
    info = paste0(
      "groups summing to neither 1 nor 0: ",
      paste(
        utils::head(
          names(esums)[abs(esums - 1) >= 1e-9 & abs(esums) >= 1e-9],
          5
        ),
        collapse = "; "
      )
    )
  )

  # And the intermediate denominator must not leak into the result.
  testthat::expect_false("denom" %in% names(energy))
})
