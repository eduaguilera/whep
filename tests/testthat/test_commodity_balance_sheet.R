# TODO: Consider fixing these unbalances somehow
k_ignore_unbalanced <- c(
  "Linum" = 772,
  "Seed cotton" = 328,
  "Oil, palm fruit" = 254,
  "Hemp" = 776,
  "Coconuts" = 248,
  "Kapok fruit" = 310,
  "Palmkernel Cake" = 2595
)

k_tolerance <- 1e-6

testthat::test_that("get_wide_cbs gives consistent Commodity Balance Sheet", {
  testthat::skip_on_ci()
  testthat::skip_on_cran()

  cbs <- get_wide_cbs() |>
    dplyr::filter(!(item_cbs_code %in% k_ignore_unbalanced)) |>
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

testthat::test_that("get_codes_coeffs gives consistent shares of processed items", {
  testthat::skip_on_ci()
  testthat::skip_on_cran()

  coefs <- get_processing_coefs()
  cbs <- get_wide_cbs()

  df <- coefs |>
    dplyr::left_join(
      cbs,
      dplyr::join_by(
        year,
        area_code,
        item_cbs_code_processed == item_cbs_code
      )
    ) |>
    dplyr::group_by(year, area_code, item_cbs_code_processed) |>
    dplyr::mutate(total_proc_item = sum(final_value_processed))

  pointblank::expect_col_vals_expr(
    df,
    rlang::expr(
      dplyr::near(production, total_proc_item, tol = !!k_tolerance)
    ),
    # TODO: Fix few problematic data rows
    threshold = 0.99
  )

  pointblank::expect_col_vals_expr(
    df,
    rlang::expr(
      dplyr::near(
        value_to_process * initial_conversion_factor,
        initial_value_processed,
        tol = !!k_tolerance
      )
    )
  )
  pointblank::expect_col_vals_expr(
    df,
    rlang::expr(
      dplyr::near(
        initial_value_processed * conversion_factor_scaling,
        final_value_processed,
        tol = !!k_tolerance
      )
    )
  )
  pointblank::expect_col_vals_expr(
    df,
    rlang::expr(
      dplyr::near(
        initial_conversion_factor * conversion_factor_scaling,
        final_conversion_factor,
        tol = !!k_tolerance
      )
    )
  )
})
