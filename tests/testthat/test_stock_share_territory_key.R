# Livestock split shares divide a parent item's stock between sub-items --
# chickens into broilers and layers, cattle into dairy and non-dairy -- and they
# were computed as `value / sum(value)` grouped by `(year, area_code, Item_Code)`.
#
# `area_code` there is polity_area_code, the FABIO reporting bucket, and that does
# not identify a territory. FAOSTAT area 206 carries BOTH Sudan and South Sudan
# from 2012 on: `.aggregate_to_polities()` emits one row per reporting territory,
# so the bucket holds two, and `sum(value)` spanned them. Measured on the full
# faostat-emissions-livestock pin before the fix:
#
#   48 distinct (year, area_code, Item_Code, item_cbs_code) keys had TWO share
#   rows, all in bucket 206, all 2012-2023;
#
#   in 2015 Sudan's 37,755,000 broilers over Sudan-plus-South-Sudan chickens gave
#   a broiler share of 0.6071, where Sudan's own broilers over Sudan's own
#   chickens are 0.8000.
#
# Two different shares for one key, so which one `.split_slaughter_by_shares()`'s
# inner_join used was a matter of row order.
#
# HOW LONG IT COULD HIDE is the part worth recording: the only symptom was
# fill_linear's "Duplicate year values found within groups", which reads exactly
# like ordinary source noise. The build completed and the row count was right.
#
# The invariant below could not have held before: shares within a
# territory-and-parent-item group must sum to 1. Under the old key they summed to
# 1 across a group that spanned territories, which is a different and meaningless
# statement.

# One reporting bucket holding two territories, which is the whole point. The
# numbers are 2015's real Sudan and South Sudan chicken stocks, so the expected
# shares below are the ones the pin produces.
.two_territory_stocks <- function() {
  tibble::tribble(
    ~year, ~area_code, ~area, ~item_cbs_code, ~item_cbs, ~value,
    2015L, 206L, "Sudan", 1053L, "Chickens, broilers", 37755000,
    2015L, 206L, "Sudan", 1052L, "Chickens, layers", 9439000,
    2015L, 206L, "South Sudan", 1053L, "Chickens, broilers", 10320284,
    2015L, 206L, "South Sudan", 1052L, "Chickens, layers", 4679716
  )
}

testthat::test_that(".compute_stock_shares keys shares by reporting territory", {
  testthat::local_mocked_bindings(
    .read_livestock_stocks = function(years = NULL) .two_territory_stocks()
  )

  result <- whep:::.compute_stock_shares(2015L)

  # `area` is half the key and must survive to the caller: without it the
  # downstream join cannot tell the two territories apart.
  testthat::expect_true("area" %in% names(result))

  # Each territory's parts are its own whole. Keyed on the bucket alone, Sudan's
  # broiler share was 0.6071 and South Sudan's 0.1659 -- both computed against
  # 62,194,000 chickens that belong to two countries.
  sudan <- result |> dplyr::filter(area == "Sudan")
  south <- result |> dplyr::filter(area == "South Sudan")
  testthat::expect_equal(
    sudan$share[sudan$item_cbs_code == 1053L],
    37755000 / (37755000 + 9439000)
  )
  testthat::expect_equal(
    south$share[south$item_cbs_code == 1053L],
    10320284 / (10320284 + 4679716)
  )

  # The invariant, stated over the group that actually means something.
  sums <- result |>
    dplyr::summarise(total = sum(share), .by = c(year, area_code, area))
  testthat::expect_equal(sums$total, rep(1, 2))

  # And no two rows share a key, so no downstream join picks by row order.
  key <- paste(
    result$year,
    result$area_code,
    result$area,
    result$Item_Code,
    result$item_cbs_code
  )
  testthat::expect_equal(sum(duplicated(key)), 0L)
})

testthat::test_that(".carry_forward_shares keeps two territories in one bucket apart", {
  # Same bucket, two territories, and shares that stop one year short of the
  # slaughter data. Keyed on `area_code` alone this is a single series with two
  # rows per year, so fill_linear sees duplicate years and the carried-forward
  # value depends on which row it picks.
  shares <- tibble::tribble(
    ~year, ~area_code, ~area, ~Item_Code, ~item_cbs_code, ~share,
    2015L, 206L, "Sudan", 1057L, 1053L, 0.8,
    2015L, 206L, "Sudan", 1057L, 1052L, 0.2,
    2015L, 206L, "South Sudan", 1057L, 1053L, 0.688,
    2015L, 206L, "South Sudan", 1057L, 1052L, 0.312
  )

  result <- testthat::expect_no_warning(
    whep:::.carry_forward_shares(shares, 2015:2016)
  )

  testthat::expect_equal(nrow(result), 8L)
  carried <- result |>
    dplyr::filter(year == 2016L) |>
    dplyr::arrange(area, item_cbs_code)
  testthat::expect_equal(
    carried$area,
    c(rep("South Sudan", 2), rep("Sudan", 2))
  )
  testthat::expect_equal(carried$share, c(0.312, 0.688, 0.2, 0.8))

  # Both years, both territories, parts summing to their own whole.
  sums <- result |>
    dplyr::summarise(total = sum(share), .by = c(year, area_code, area))
  testthat::expect_equal(sums$total, rep(1, 4))
})
