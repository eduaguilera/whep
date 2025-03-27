testthat::test_that(
  "get_bilateral_trade gives consistent country_share",
  {
    bilateral_trade_path <- here::here(
      "inst/extdata/input/processed/bilateral_trade.csv"
    )
    if (!file.exists(bilateral_trade_path)) {
      skip("Not running local test that depends on file")
    }

    bilateral_trade <- bilateral_trade_path |>
      get_bilateral_trade() |>
      dplyr::filter(from_code == 4, year == 1986) |>
      dplyr::arrange(item) |>
      dplyr::group_by(from_code, year, item) |>
      dplyr::mutate(my_total = sum(value))

    bilateral_trade |>
      pointblank::create_agent() |>
      pointblank::col_vals_expr(
        ~ dplyr::near(sum(value) * country_share, value, tol = 1e-6)
      ) |>
      pointblank::interrogate() |>
      print()
  }
)
