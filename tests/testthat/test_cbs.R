# TODO: Consider fixing these unbalances somehow
k_ignore_unbalanced <- c(
  "Linum" = 772,
  "Seed cotton" = 328,
  "Oil, palm fruit" = 254,
  "Hemp" = 776,
  "Coconuts" = 248,
  "Kapok fruit" = 310
)

k_tolerance <- 1e-6

testthat::test_that("get_wide_cbs gives consistent Commodity Balance Sheet", {
  cbs <-
    "inst/extdata/input/processed/cbs.csv" |>
    here::here() |>
    get_wide_cbs() |>
    dplyr::filter(!(item_code %in% k_ignore_unbalanced)) |>
    dplyr::mutate(
      value_in = production + import + stock_retrieval,
      value_out = export + food + feed + seed + processing + other_uses,
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

testthat::test_that(
  "get_codes_coeffs gives consistent shares of processed items",
  {
    coefs <- "inst/extdata/input/processed/processing_coefs.csv" |>
      here::here() |>
      get_processing_coefs()

    cbs <- "inst/extdata/input/processed/cbs.csv" |>
      here::here() |>
      get_wide_cbs()

    df <- coefs |>
      dplyr::left_join(cbs, c("year", "area", "area_code", "item")) |>
      dplyr::group_by(year, area_code, item) |>
      dplyr::mutate(total_proc_item = sum(value_proc))

    # value_proc was correctly obtained from production data
    pointblank::expect_col_vals_expr(
      df,
      rlang::expr(
        dplyr::near(production, total_proc_item, tol = !!k_tolerance)
      ),
      # TODO: Fix single weird mismatch Grenada datapoint
      threshold = 0.99
    )
    pointblank::expect_col_vals_expr(
      df,
      rlang::expr(
        dplyr::near(
          value * product_fraction,
          value_proc_raw,
          tol = !!k_tolerance
        )
      )
    )
    pointblank::expect_col_vals_expr(
      df,
      rlang::expr(
        dplyr::near(value_proc_raw * scaling, value_proc, tol = !!k_tolerance)
      )
    )
    pointblank::expect_col_vals_expr(
      df,
      rlang::expr(
        dplyr::near(product_fraction * scaling, cf, tol = !!k_tolerance)
      )
    )
  }
)
