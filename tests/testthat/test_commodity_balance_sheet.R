# Small crafted wide CBS fixture with consistent accounting.
# supply = production + import + stock_withdrawal
# use    = export + food + feed + seed + processing + other_uses + stock_addition
# domestic_supply = food + feed + seed + processing + other_uses
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
