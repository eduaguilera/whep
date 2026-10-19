# Tests for R/regime_residue.R: residue and root of each regime part,
# re-evaluated from the package's residue and root model at the part's yield.

# Maize in Spain in 2010 at a national yield of 6 t/ha, split half irrigated
# with R = 1.5: Y_r = 6 / (0.5 + 0.5 * 1.5) = 4.8 and Y_i = 7.2 t/ha.
.rrs_maize_parts <- function() {
  tibble::tribble(
    ~item_prod_code, ~area_code, ~year, ~yield_mean, ~yield_part, ~water_regime,
    56L,             203L,       2010L, 6,           4.8,         "rainfed",
    56L,             203L,       2010L, 6,           7.2,         "irrigated"
  )
}

testthat::test_that("irrigated maize takes x0.9 on residue and x0.85 on root:shoot, by hand", {
  # The two irrigation terms the model applies, as the coefficient tables
  # state them.
  irr <- whep::whep_coef_table("irrigation_adj")
  testthat::expect_identical(
    irr$residue_ratio_factor[irr$water_regime == "Irrigated"],
    0.9
  )
  testthat::expect_identical(
    irr$rs_ratio_factor[irr$water_regime == "Irrigated"],
    0.85
  )
  sens <- whep::whep_coef_table("irr_residue_crop_adj")
  testthat::expect_equal(
    sens$irr_residue_sensitivity[sens$crop_group == "Maize"],
    1
  )
  out <- whep:::.rr_part_factors(.rrs_maize_parts())
  mean <- .rs_maize_by_hand(6)
  rainfed <- .rs_maize_by_hand(4.8)
  irrigated <- .rs_maize_by_hand(7.2, residue_factor = 0.9, rs_factor = 0.85)
  testthat::expect_equal(
    out$residue_factor,
    c(rainfed$residue, irrigated$residue) / mean$residue,
    tolerance = 1e-12
  )
  testthat::expect_equal(
    out$root_factor,
    c(rainfed$root, irrigated$root) / mean$root,
    tolerance = 1e-12
  )
  # Proportional scaling would give the irrigated part Y_i / Y = 1.2 times
  # the crop's residue; the affine residue and the irrigation terms give it
  # less.
  testthat::expect_lt(out$residue_factor[2], 7.2 / 6)
  testthat::expect_lt(out$root_factor[2], 7.2 / 6)
})

testthat::test_that("a part at the crop's mean yield in rainfed regime has factor 1 exactly", {
  parts <- dplyr::mutate(
    .rrs_maize_parts(),
    yield_part = .data$yield_mean,
    water_regime = "rainfed"
  )
  out <- whep:::.rr_part_factors(parts)
  testthat::expect_identical(out$residue_factor, c(1, 1))
  testthat::expect_identical(out$root_factor, c(1, 1))
})

testthat::test_that("with the irrigation terms neutral, the parts' residue and root sum to the crop's", {
  .rs_neutral_irrigation()
  parts <- .rrs_maize_parts()
  out <- whep:::.rr_part_factors(parts)
  # Half the area in each part, so the area-weighted factors average to 1:
  # the model is affine in yield and the parts keep the crop's production.
  testthat::expect_lt(abs(mean(out$residue_factor) - 1), 1e-12)
  testthat::expect_lt(abs(mean(out$root_factor) - 1), 1e-12)
  # Not vacuous: the irrigated hectare carries more than the rainfed one.
  testthat::expect_gt(out$residue_factor[2], out$residue_factor[1])
  testthat::expect_gt(out$root_factor[2], out$root_factor[1])
})

testthat::test_that("the irrigation-label guard fires on a capitalisation mismatch", {
  read_coef <- whep:::.read_coef
  testthat::local_mocked_bindings(
    .read_coef = function(name) {
      out <- read_coef(name)
      if (name == "irrigation_adj") {
        out$water_regime <- tolower(out$water_regime)
      }
      out
    },
    .package = "whep"
  )
  row <- tibble::tibble(item_prod_code = "56", production_t = 7.2, area_ha = 1)
  irrigated <- whep::calculate_crop_npp(
    dplyr::mutate(row, water_regime = "Irrigated")
  )
  unadjusted <- whep::calculate_crop_npp(row)
  expect_supplied_guard(
    # The identity a vocabulary mismatch leaves intact: the irrigated row
    # silently takes factor 1 and equals the unadjusted one.
    identity = identical(irrigated$residue_dm_t, unadjusted$residue_dm_t) &&
      identical(irrigated$root_dm_t, unadjusted$root_dm_t),
    guard = whep:::.rr_part_factors(.rrs_maize_parts()),
    class = "whep_absent_label"
  )
})

testthat::test_that("a part label outside rainfed/irrigated aborts", {
  parts <- dplyr::mutate(.rrs_maize_parts(), water_regime = "Irrigated")
  testthat::expect_error(
    whep:::.rr_part_factors(parts),
    class = "whep_regime_label"
  )
})

testthat::test_that("a part without a yield aborts instead of taking the zero-yield intercept", {
  # Why the guard exists: the model books a missing production as zero and
  # still hands the hectare its residue intercept, so a part with no yield
  # would come back with a finite, plausible-looking factor.
  row <- tibble::tibble(
    item_prod_code = "56",
    production_t = NA_real_,
    area_ha = 1
  )
  at_na <- whep::calculate_crop_residues(row)
  testthat::expect_true(all(is.finite(at_na$residue_dm_t)))
  testthat::expect_gt(at_na$residue_dm_t, 0)
  no_part <- dplyr::mutate(
    .rrs_maize_parts(),
    yield_part = c(4.8, NA_real_)
  )
  testthat::expect_error(
    whep:::.rr_part_factors(no_part),
    class = "whep_regime_part_yield"
  )
  no_mean <- dplyr::mutate(.rrs_maize_parts(), yield_mean = NA_real_)
  testthat::expect_error(
    whep:::.rr_part_factors(no_mean),
    class = "whep_regime_part_yield"
  )
})

testthat::test_that("a crop with no residue or root in the model keeps factor 1", {
  # Item 999999 is in no coefficient table: the model gives it no residue and
  # no root at any yield, so there is nothing to scale.
  parts <- dplyr::mutate(.rrs_maize_parts()[2, ], item_prod_code = 999999L)
  out <- whep:::.rr_part_factors(parts)
  testthat::expect_identical(out$residue_factor, 1)
  testthat::expect_identical(out$root_factor, 1)
})
