# Small crafted wide CBS fixture with consistent accounting.
# Supply is production plus import plus stock withdrawal.
# Use is export plus food, feed, seed, processing, other uses,
# and stock addition.
# Domestic supply is food, feed, seed, processing, and other uses.
.make_cbs_fixture <- function() {
  tibble::tribble(
    ~year, ~area_code, ~item_cbs_code,
    ~production, ~import, ~export,
    ~food, ~feed, ~seed, ~processing, ~other_uses,
    ~stock_withdrawal, ~stock_addition, ~domestic_supply,
    2000L, 203L, 2511L,
    5000, 1000, 500,
    3000, 1500, 200, 500, 300,
    0, 0, 5500,
    2000L, 68L, 2514L,
    3000, 500, 200,
    2000, 800, 100, 200, 200,
    0, 0, 3300
  )
}

# Small crafted processing coefficients fixture.
.make_coefs_fixture <- function() {
  tibble::tribble(
    ~year, ~area_code,
    ~item_cbs_code_to_process, ~value_to_process,
    ~item_cbs_code_processed, ~initial_conversion_factor,
    ~initial_value_processed, ~conversion_factor_scaling,
    ~final_conversion_factor, ~final_value_processed,
    2000L, 203L,
    2511L, 5000,
    2542L, 0.2,
    1000, 0.5,
    0.1, 500
  )
}

k_tolerance <- 1e-6

testthat::test_that("wide CBS has consistent supply-use balance", {
  cbs <- .make_cbs_fixture() |>
    dplyr::mutate(
      value_in = production + import + stock_withdrawal,
      value_out = export +
        food +
        feed +
        seed +
        processing +
        other_uses +
        stock_addition,
      my_domestic_supply = food + feed + seed + processing + other_uses
    )

  pointblank::expect_col_vals_expr(
    cbs,
    rlang::expr(
      dplyr::near(value_in, value_out, tol = !!k_tolerance)
    )
  )

  pointblank::expect_col_vals_expr(
    cbs,
    rlang::expr(
      dplyr::near(domestic_supply, my_domestic_supply, tol = !!k_tolerance)
    )
  )
})

testthat::test_that(".pivot_cbs_wide splits stock variation by balance sign", {
  cbs_long <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~element, ~value,
    2000L, 1L, 10L, "production", 100,
    2000L, 1L, 10L, "stock_variation", 30,
    2000L, 2L, 10L, "production", 100,
    2000L, 2L, 10L, "stock_variation", -20
  )

  result <- .pivot_cbs_wide(cbs_long) |>
    dplyr::arrange(.data$area_code)

  testthat::expect_equal(result$stock_addition, c(30, 0))
  testthat::expect_equal(result$stock_withdrawal, c(0, 20))
})

testthat::test_that("processing coefficients are internally consistent", {
  coefs <- .make_coefs_fixture()

  pointblank::expect_col_vals_expr(
    coefs,
    rlang::expr(
      dplyr::near(
        value_to_process * initial_conversion_factor,
        initial_value_processed,
        tol = !!k_tolerance
      )
    )
  )

  pointblank::expect_col_vals_expr(
    coefs,
    rlang::expr(
      dplyr::near(
        initial_value_processed * conversion_factor_scaling,
        final_value_processed,
        tol = !!k_tolerance
      )
    )
  )

  pointblank::expect_col_vals_expr(
    coefs,
    rlang::expr(
      dplyr::near(
        initial_conversion_factor * conversion_factor_scaling,
        final_conversion_factor,
        tol = !!k_tolerance
      )
    )
  )
})

testthat::test_that("livestock CBS routes slaughter animals to processing", {
  local_mocked_bindings(
    .get_livestock_trade_totals = function(livestock_items) {
      tibble::tibble(
        year = integer(),
        area_code = integer(),
        item_cbs_code = integer(),
        import = numeric(),
        export = numeric()
      )
    }
  )

  primary <- tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~live_anim_code, ~unit, ~value,
    2000L, 1L, 1096L, NA, "heads", 100,
    2000L, 1L, 1096L, NA, "slaughtered_heads", 10,
    2000L, 1L, 2735L, 1096L, "tonnes", 2,
    2000L, 1L, 946L, NA, "heads", 200,
    2000L, 1L, 946L, NA, "slaughtered_heads", 20,
    2000L, 1L, 2731L, 946L, "tonnes", 5
  )

  result <- get_livestock_cbs(primary)
  horse <- dplyr::filter(result, item_cbs_code == 1096L)
  buffalo <- dplyr::filter(result, item_cbs_code == 946L)

  testthat::expect_equal(horse$production, 10)
  testthat::expect_equal(horse$domestic_supply, 10)
  testthat::expect_equal(horse$other_uses, 0)
  testthat::expect_equal(horse$processing, 10)

  testthat::expect_equal(buffalo$production, 20)
  testthat::expect_equal(buffalo$domestic_supply, 20)
  testthat::expect_equal(buffalo$processing, 20)
  testthat::expect_equal(buffalo$other_uses, 0)
})
