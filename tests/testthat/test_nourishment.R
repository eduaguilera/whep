# The default protein floor (62.1) and ceiling (85.05) g/cap/day from
# whep::nourishment_thresholds, restated as the exact 46 * 1.35 / 63 * 1.35
# doubles the packaged dataset stores so the boundary rows compare bit-exactly
# (the literals 62.1 / 85.05 are different doubles and would miss the boundary).
.protein_floor <- 46 * 1.35
.protein_ceiling <- 63 * 1.35

# A small supply spanning every class: below the floor, inside the adequate
# band, above the ceiling, and exactly on each boundary.
.nourish_fixture <- function() {
  tibble::tribble(
    ~area_code, ~protein_g_cap_day,
    10L, 30,
    20L, 70,
    30L, 100,
    40L, .protein_floor,
    50L, .protein_ceiling
  )
}

testthat::test_that("below-floor supply normalizes below 1 and is Under", {
  out <- whep::normalize_nourishment(.nourish_fixture())
  a10 <- dplyr::filter(out, area_code == 10)
  testthat::expect_equal(a10$value_norm, 30 / .protein_floor)
  testthat::expect_lt(a10$value_norm, 1)
  testthat::expect_equal(a10$nourish, "Under")
})

testthat::test_that("in-band supply normalizes to 1-2 and is Adequate", {
  out <- whep::normalize_nourishment(.nourish_fixture())
  a20 <- dplyr::filter(out, area_code == 20)
  expected <- 1 + (70 - .protein_floor) / (.protein_ceiling - .protein_floor)
  testthat::expect_equal(a20$value_norm, expected)
  testthat::expect_gt(a20$value_norm, 1)
  testthat::expect_lt(a20$value_norm, 2)
  testthat::expect_equal(a20$nourish, "Adequate")
})

testthat::test_that("above-ceiling supply normalizes above 2 and is Over", {
  out <- whep::normalize_nourishment(.nourish_fixture())
  a30 <- dplyr::filter(out, area_code == 30)
  testthat::expect_equal(a30$value_norm, 1 + 100 / .protein_ceiling)
  testthat::expect_gt(a30$value_norm, 2)
  testthat::expect_equal(a30$nourish, "Over")
})

testthat::test_that("the exact floor scores 1 (Adequate) and ceiling 2 (Over)", {
  out <- whep::normalize_nourishment(.nourish_fixture())
  a40 <- dplyr::filter(out, area_code == 40)
  a50 <- dplyr::filter(out, area_code == 50)
  testthat::expect_equal(a40$value_norm, 1)
  testthat::expect_equal(a40$nourish, "Adequate")
  testthat::expect_equal(a50$value_norm, 2)
  testthat::expect_equal(a50$nourish, "Over")
})

testthat::test_that("normalize_nourishment defaults to the protein bounds", {
  # With no thresholds supplied, the packaged protein floor/ceiling are used.
  out <- whep::normalize_nourishment(
    tibble::tibble(protein_g_cap_day = c(50, .protein_ceiling))
  )
  testthat::expect_equal(out$value_norm, c(50 / .protein_floor, 2))
  testthat::expect_equal(out$nourish, c("Under", "Over"))
})

testthat::test_that("value_col and thresholds switch to the energy axis", {
  x <- tibble::tibble(energy_kcal_cap_day = c(2000, 2600, 3000))
  out <- whep::normalize_nourishment(
    x,
    value_col = energy_kcal_cap_day,
    thresholds = c(floor = 2300, ceiling = 2900)
  )
  testthat::expect_equal(
    out$value_norm,
    c(2000 / 2300, 1 + (2600 - 2300) / (2900 - 2300), 1 + 3000 / 2900)
  )
  testthat::expect_equal(out$nourish, c("Under", "Adequate", "Over"))
})

testthat::test_that("normalize_nourishment aborts on malformed thresholds", {
  testthat::expect_error(
    whep::normalize_nourishment(
      tibble::tibble(protein_g_cap_day = 60),
      thresholds = c(low = 62.1, high = 85.05)
    ),
    "floor|ceiling"
  )
})

testthat::test_that("missing nourishment remains unclassified", {
  out <- whep::normalize_nourishment(
    tibble::tibble(protein_g_cap_day = NA_real_)
  )
  testthat::expect_true(is.na(out$value_norm))
  testthat::expect_true(is.na(out$nourish))
})
