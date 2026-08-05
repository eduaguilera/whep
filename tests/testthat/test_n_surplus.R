# A minimal build_nitrogen_balance()-shaped fixture: two cells, several crops,
# a nitrogen-deficit row and a zero-surplus row, with burnt_residue_n_t varied
# so the test can confirm it does not enter the harvest-removal surplus.
.n_surplus_balance_fixture <- function() {
  tibble::tribble(
    ~lon,
    ~lat,
    ~area_code,
    ~item_cbs_code,
    ~year,
    ~area_ha,
    ~n_input_std_t,
    ~prod_n_t,
    ~used_residue_n_t,
    ~grazed_weeds_n_t,
    ~burnt_residue_n_t,
    ~n_balance_t,
    0.25, 0.25, 1L, 2511L, 2010L, 100, 50, 20, 5, 0, 3, 22,
    0.25, 0.25, 1L, 2513L, 2010L, 50, 10, 8, 1, 0, 1, 0,
    0.25, 0.25, 1L, 2555L, 2010L, 40, 4, 6, 0, 0, 0, -2,
    0.75, 0.25, 1L, 2511L, 2010L, 200, 120, 40, 10, 8, 5, 55
  )
}

testthat::test_that("harvest_removal surplus is inputs minus harvest exports", {
  balance <- .n_surplus_balance_fixture()
  out <- whep::calculate_n_surplus(balance, method = "harvest_removal")
  expected <- balance$n_input_std_t -
    (balance$prod_n_t + balance$used_residue_n_t + balance$grazed_weeds_n_t)
  testthat::expect_equal(out$surplus_n_t, expected)
  testthat::expect_true(all(out$method_surplus == "harvest_removal"))
  # A deficit row is kept negative, never clamped.
  testthat::expect_true(any(out$surplus_n_t < 0))
})

testthat::test_that("burnt_residue_n_t does not affect the surplus", {
  balance <- .n_surplus_balance_fixture()
  bumped <- dplyr::mutate(
    balance,
    burnt_residue_n_t = .data$burnt_residue_n_t + 1000
  )
  a <- whep::calculate_n_surplus(balance)
  b <- whep::calculate_n_surplus(bumped)
  testthat::expect_equal(a$surplus_n_t, b$surplus_n_t)
})

testthat::test_that("per-crop item_cbs_code and the grid key are retained", {
  balance <- .n_surplus_balance_fixture()
  out <- whep::calculate_n_surplus(balance)
  pointblank::expect_col_exists(
    out,
    c("lon", "lat", "area_code", "item_cbs_code", "year")
  )
  testthat::expect_equal(out$item_cbs_code, balance$item_cbs_code)
})

testthat::test_that("surplus_kgn_ha is emitted from area_ha", {
  balance <- .n_surplus_balance_fixture()
  out <- whep::calculate_n_surplus(balance)
  pointblank::expect_col_exists(out, "surplus_kgn_ha")
  testthat::expect_equal(
    out$surplus_kgn_ha,
    out$surplus_n_t * 1000 / balance$area_ha
  )
})

testthat::test_that("surplus_kgn_ha is skipped when area_ha is absent", {
  balance <- dplyr::select(.n_surplus_balance_fixture(), -"area_ha")
  out <- whep::calculate_n_surplus(balance)
  testthat::expect_false(rlang::has_name(out, "surplus_kgn_ha"))
})

testthat::test_that("full_balance surplus returns n_balance_t", {
  balance <- .n_surplus_balance_fixture()
  out <- whep::calculate_n_surplus(balance, method = "full_balance")
  testthat::expect_equal(out$surplus_n_t, balance$n_balance_t)
  testthat::expect_true(all(out$method_surplus == "full_balance"))
})

testthat::test_that("calculate_n_surplus rejects an unknown method", {
  testthat::expect_error(
    whep::calculate_n_surplus(
      .n_surplus_balance_fixture(),
      method = "not_a_method"
    ),
    "arg_match|must be one of|not_a_method"
  )
})

testthat::test_that("calculate_n_surplus(example = TRUE) is self-consistent", {
  out <- whep::calculate_n_surplus(example = TRUE)
  testthat::expect_s3_class(out, "tbl_df")
  pointblank::expect_col_exists(
    out,
    c("item_cbs_code", "surplus_n_t", "method_surplus", "surplus_kgn_ha")
  )
  testthat::expect_true(all(out$method_surplus == "harvest_removal"))
})
