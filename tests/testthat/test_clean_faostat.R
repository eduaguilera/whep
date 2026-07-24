# test_clean_faostat.R — unit tests for R/clean_faostat.R helpers

# -- Fixtures ------------------------------------------------------------------

.make_constant_series <- function() {
  tibble::tibble(
    area = "Smallland",
    item_prod = "Minor crop",
    year = 2000:2010,
    value = 100
  )
}

.make_tail_series <- function() {
  # Rising trend 2000-2005, then a flat carry-forward tail 2005-2010.
  tibble::tibble(
    area = "Bigland",
    item_prod = "Wheat",
    year = 2000:2010,
    value = c(10, 20, 30, 40, 50, 60, 60, 60, 60, 60, 60)
  )
}

# -- smooth_carry_forward: constant series -------------------------------------

test_that(".smooth_carry_forward leaves a constant series unchanged", {
  by <- c("area", "item_prod")

  flagged <- .make_constant_series() |>
    whep:::.flag_cf_and_spikes(by = by)

  # Sanity: a nonzero constant tail is entirely flagged as carry-forward.
  expect_true(all(flagged$qc_carry_forward))

  smoothed <- whep:::.smooth_carry_forward(flagged, by = by) |>
    tibble::as_tibble()

  expect_false(any(is.nan(smoothed$value)))
  expect_equal(smoothed$value, rep(100, 11))
})

# -- smooth_carry_forward: genuine carry-forward tail --------------------------

test_that(".smooth_carry_forward smooths a flat tail via anchor trend", {
  by <- c("area", "item_prod")

  flagged <- .make_tail_series() |>
    whep:::.flag_cf_and_spikes(by = by)

  smoothed <- whep:::.smooth_carry_forward(flagged, by = by) |>
    tibble::as_tibble()

  expect_false(any(is.nan(smoothed$value)))
  # Non-flagged rising rows are untouched.
  keep <- !flagged$qc_carry_forward
  expect_equal(smoothed$value[keep], flagged$value[keep])
  # The flat tail (years 2005-2010) is replaced by the anchor trend
  # value = 10 * (year - 2000) + 10, extrapolated past the last anchor.
  tail <- flagged$qc_carry_forward
  expect_equal(
    smoothed$value[tail],
    10 * (smoothed$year[tail] - 2000) + 10
  )
})
