library("testthat")

# TODO: Consider fixing these unbalances somehow
k_ignore_unbalanced <- c(
  "Linum" = 772,
  "Seed cotton" = 328,
  "Oil, palm fruit" = 254,
  "Hemp" = 776,
  "Coconuts" = 248,
  "Kapok fruit" = 310
)

expect_almost_equal <- function(actual, expected, round_decimals = 6) {
  almost_actual <- round(actual, round_decimals)
  almost_expected <- round(expected, round_decimals)
  expect_equal(almost_actual, almost_expected)
}

expect_columns_almost_equal <- function(
    df,
    column_actual,
    column_expected,
    round_decimals = 6,
    output_bad_rows_path = NA) {
  almost_column_actual <- dplyr::pull(df, {{ column_actual }})
  almost_column_expected <- dplyr::pull(df, {{ column_expected }})

  if (!is.na(output_bad_rows_path)) {
    # print(bad_rows)
    df |>
      print() |>
      dplyr::filter(
        abs({{ column_actual }} - {{ column_expected }}) > 1e-6
      ) |>
      print() |>
      readr::write_csv(output_bad_rows_path)
  }

  expect_almost_equal(almost_column_actual, almost_column_expected, round_decimals)
  df
}

test_that("get_wide_cbs gives consistent Commodity Balance Sheet", {
  cbs <-
    "inst/extdata/input/processed/cbs.csv" |>
    here::here() |>
    get_wide_cbs()

  cbs |>
    dplyr::filter(!(item_code %in% k_ignore_unbalanced)) |>
    dplyr::mutate(
      value_in = production + import + stock_retrieval,
      value_out = export + food + feed + seed + processing + other_uses,
      my_domestic_supply = value_in - export,
      .keep = "unused"
    ) |>
    expect_columns_almost_equal(value_in, value_out) |>
    expect_columns_almost_equal(my_domestic_supply, domestic_supply)
})

test_that("get_codes_coeffs gives consistent shares of processed items", {
  coefs <- "inst/extdata/input/processed/processing_coefs.csv" |>
    here::here() |>
    get_processing_coefs()

  cbs <- "inst/extdata/input/processed/cbs.csv" |>
    here::here() |>
    get_wide_cbs()

  df <- coefs |>
    dplyr::left_join(cbs, c("year", "area", "area_code", "item")) |>
    dplyr::group_by(year, area_code, processeditem) |>
    dplyr::summarize(
      total_proc = sum(value_proc),
      total_proc_raw = sum(value_proc_raw),
      dplyr::across()
    )

  # value_proc was correctly obtained from production data
  expect_equal(df$production, df$value_proc)

  # value_proc_raw is the product (value * product_fraction)
  # i.e. the new estimation of value to match official production
  # of the item that is processed
  bad_share_path <- here::here("inst/extdata/output/bad_share_path.csv")
  df |>
    dplyr::mutate(my_share = value * product_fraction) |>
    expect_columns_almost_equal(my_share, value_proc_raw, output_bad_rows_path = bad_share_path)

  bad_total_path <- here::here("inst/extdata/output/bad_total_path.csv")
  expect_columns_almost_equal(df, total_proc_raw, value, output_bad_rows_path = bad_total_path)
})
