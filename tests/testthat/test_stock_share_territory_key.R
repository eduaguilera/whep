# Livestock split shares divide a parent item's stock between sub-items -- chickens into
# broilers and layers, cattle into dairy and non-dairy -- and they were computed as
# `value / sum(value)` grouped by `(year, area_code, Item_Code)`.
#
# `area_code` there is polity_area_code, and I broke the assumption that it identifies one
# territory. Earlier in this branch I unfolded four areas that carry their own commodity
# balances -- New Caledonia, North Macedonia, Eswatini and Syria -- so each reports its own
# polity. Three of them took their own polity_area_code (153, 154, 212); ESWATINI KEPT
# FABIO's 999, and that one is enough. The 999 group then held two territories, and
# `sum(value)` spanned them:
#
#   Eswatini       1,053,000 broilers   share 0.9419   <- denominator included ROW
#   Rest of World      3,000 broilers   share 0.0027   <- denominator included Eswatini
#
# Two different shares for one (year, area_code, Item_Code, item_cbs_code), so which one a
# downstream inner_join used was a matter of row order. After keying by the reporting
# territory the same two rows read 0.9461 and 0.600 -- each its own broilers over its own
# chickens. Rest of World's share is corrected by a factor of 222.
#
# HOW LONG IT HID, which is the part worth recording: the only symptom was
# "Duplicate year values found within groups. 24 group/time combination(s)" from
# fill_linear, and I wrote it off as pre-existing data noise TWICE while smoke-testing
# other things. Nothing else complained. The build completed, the row count was right, and
# every polity column was populated.
#
# The invariant below is the one that could not have been satisfied before: shares within a
# territory-item group must sum to 1. Under the old key they summed to 1 across a group
# that spanned territories, which is a different and meaningless statement.
testthat::test_that("stock shares are keyed by reporting territory and sum to one", {
  testthat::skip_on_ci()
  sh <- tryCatch(
    whep:::.compute_stock_shares(1990:1991),
    error = function(e) NULL
  )
  testthat::skip_if(is.null(sh), "livestock stocks pin unavailable")
  sh <- as.data.frame(sh)

  # `area` is half the key. Its absence is what allowed one polity_area_code to stand for
  # several territories.
  testthat::expect_true(
    all(
      c("year", "area_code", "area", "Item_Code", "item_cbs_code", "share") %in%
        names(sh)
    )
  )
  testthat::expect_gt(nrow(sh), 100L)

  key <- paste(
    sh$year,
    sh$area_code,
    sh$area,
    sh$Item_Code,
    sh$item_cbs_code
  )
  testthat::expect_equal(
    sum(duplicated(key)),
    0L,
    info = paste0(
      "duplicate share keys, so a downstream join picks one by row order: ",
      paste(utils::head(key[duplicated(key)], 5), collapse = "; ")
    )
  )

  # The real invariant. A share group is one territory's one parent item, so its parts
  # must be exactly the whole.
  grp <- paste(sh$year, sh$area_code, sh$area, sh$Item_Code)
  sums <- tapply(sh$share, grp, sum)
  testthat::expect_gt(length(sums), 50L)
  testthat::expect_equal(
    sum(abs(sums - 1) >= 1e-9),
    0L,
    info = paste0(
      "share groups not summing to 1: ",
      paste(utils::head(names(sums)[abs(sums - 1) >= 1e-9], 5), collapse = "; ")
    )
  )
})

testthat::test_that("the FABIO bucket holds a reporting territory besides rest-of-world", {
  # The premise of the fix, pinned because I got it wrong first: I wrote that all four
  # unfolded areas kept polity_area_code 999, and only ESWATINI did. The other three took
  # their own codes (New Caledonia 153, North Macedonia 154, Syria 212). One shared bucket
  # is enough to produce the defect, so the fix stands -- but the reasoning in a comment is
  # worth no more than the measurement behind it.
  cw <- as.data.frame(whep::polity_area_crosswalk)
  in_bucket <- cw[which(cw$polity_area_code == 999L), ]
  testthat::expect_gt(nrow(in_bucket), 40L)

  own_polity <- sort(unique(in_bucket$area_code[which(
    !is.na(in_bucket$polity_code) &
      !startsWith(in_bucket$polity_code, "ROW-")
  )]))
  testthat::expect_setequal(own_polity, 209L)

  # And the three that left the bucket must stay out of it: if one returned, its shares
  # would rejoin rest-of-world's denominator, which is the defect all over again.
  for (code in c(153L, 154L, 212L)) {
    rows <- cw[which(cw$area_code == code), ]
    testthat::expect_true(
      all(rows$polity_area_code == code),
      info = paste0(
        "area ",
        code,
        " is back in a shared aggregation bucket, so its stock shares now share a ",
        "denominator with another territory"
      )
    )
  }
})
