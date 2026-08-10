# A country-resolution exceedance fixture: crop 2511 partly exceeds, crop 2513
# is fully within the boundary (zero exceedance).
.nex_exceedance_fixture <- function() {
  tibble::tribble(
    ~year,
    ~area_code,
    ~item_cbs_code,
    ~exceedance_n_t,
    ~within_boundary_n_t,
    ~actual_n_t,
    ~production_n_t,
    2010L, 10L, 2511L, 5, 3, 8, 12,
    2010L, 10L, 2513L, 0, 4, 4, 7
  )
}

testthat::test_that("the exceedance category carries exceedance_n_t", {
  out <- whep::build_n_exceedance_extension(
    .nex_exceedance_fixture(),
    category = "exceedance"
  )
  testthat::expect_equal(out$impact_u, c(5, 0))
  testthat::expect_true(all(out$method_n_exceedance == "exceedance"))
})

testthat::test_that("the within_boundary category carries within_boundary_n_t", {
  out <- whep::build_n_exceedance_extension(
    .nex_exceedance_fixture(),
    category = "within_boundary"
  )
  testthat::expect_equal(out$impact_u, c(3, 4))
  testthat::expect_true(all(out$method_n_exceedance == "within_boundary"))
})

testthat::test_that("the production category carries production_n_t", {
  out <- whep::build_n_exceedance_extension(
    .nex_exceedance_fixture(),
    category = "production"
  )
  testthat::expect_equal(out$impact_u, c(12, 7))
  testthat::expect_true(all(out$method_n_exceedance == "production"))
})

testthat::test_that("the default category is exceedance", {
  out <- whep::build_n_exceedance_extension(.nex_exceedance_fixture())
  testthat::expect_true(all(out$method_n_exceedance == "exceedance"))
})

testthat::test_that("the extension contract columns exist", {
  out <- whep::build_n_exceedance_extension(.nex_exceedance_fixture())
  pointblank::expect_col_exists(
    out,
    c("year", "area_code", "item_cbs_code", "impact_u", "method_n_exceedance")
  )
})

testthat::test_that("an unknown category is rejected", {
  testthat::expect_error(
    whep::build_n_exceedance_extension(
      .nex_exceedance_fixture(),
      category = "surplus"
    )
  )
})

testthat::test_that("the per-crop key is preserved and zero-impact kept", {
  out <- whep::build_n_exceedance_extension(
    .nex_exceedance_fixture(),
    category = "exceedance"
  )
  # Both crops survive: the zero-exceedance crop 2513 is kept (it still trades).
  testthat::expect_equal(nrow(out), 2L)
  testthat::expect_setequal(out$item_cbs_code, c(2511L, 2513L))
})

testthat::test_that("rows with a missing key are dropped", {
  exceedance <- tibble::tribble(
    ~year,
    ~area_code,
    ~item_cbs_code,
    ~exceedance_n_t,
    ~within_boundary_n_t,
    ~actual_n_t,
    ~production_n_t,
    2010L, 10L, 2511L, 5, 3, 8, 12,
    2010L, NA_integer_, 2513L, 1, 1, 2, 3
  )
  out <- whep::build_n_exceedance_extension(exceedance)
  testthat::expect_equal(nrow(out), 1L)
  testthat::expect_equal(out$item_cbs_code, 2511L)
})

testthat::test_that("signed crop-attributed exceedance is retained", {
  bad <- dplyr::mutate(
    .nex_exceedance_fixture(),
    exceedance_n_t = dplyr::if_else(
      .data$item_cbs_code == 2511L,
      -1,
      .data$exceedance_n_t
    ),
    attribution_method = "signed_crop_surplus_share"
  )
  out <- whep::build_n_exceedance_extension(bad)
  testthat::expect_equal(out$impact_u, c(-1, 0))
})

testthat::test_that("non-finite footprint attributions are rejected", {
  bad <- dplyr::mutate(
    .nex_exceedance_fixture(),
    exceedance_n_t = dplyr::if_else(
      .data$item_cbs_code == 2511L,
      NA_real_,
      .data$exceedance_n_t
    )
  )
  testthat::expect_error(
    whep::build_n_exceedance_extension(bad),
    "finite"
  )
})

testthat::test_that("explicit cell residuals block mandatory crop footprints", {
  unresolved <- dplyr::mutate(
    .nex_exceedance_fixture(),
    attribution_status = "undefined_zero_denominator",
    attribution_record_type = dplyr::if_else(
      dplyr::row_number() == 1L,
      "cell_residual",
      "crop_allocation"
    ),
    unallocated_positive_overshoot_n_t = dplyr::if_else(
      dplyr::row_number() == 1L,
      2,
      0
    )
  )
  testthat::expect_error(
    whep::build_n_exceedance_extension(unresolved),
    class = "whep_n_attribution_undefined"
  )
})
