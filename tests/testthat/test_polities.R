.expect_consistent_code_info <- function(polities) {
  polities |>
    tidyr::separate_wider_delim(
      polity_code,
      names = c("code_name", "code_start_year", "code_end_year"),
      delim = "-",
      cols_remove = FALSE
    ) |>
    pointblank::expect_col_vals_expr(~ code_start_year == start_year) |>
    pointblank::expect_col_vals_expr(~ code_end_year == end_year) |>
    # Only matches when iso3_code is not NA
    pointblank::expect_col_vals_expr(~ code_name == iso3_code) |>
    dplyr::select(-code_name, -code_start_year, -code_end_year)
}

testthat::test_that("get_polities is consistent", {
  cols <- c(
    "polity_name",
    "polity_code",
    "start_year",
    "end_year",
    "m49_code",
    "iso2_code",
    "iso3_code",
    "display_code"
  )

  int_cols <- c("start_year", "end_year")
  other_cols <- setdiff(cols, int_cols)
  non_null_cols <- c(
    "polity_name",
    "polity_code",
    "start_year",
    "end_year",
    "display_code"
  )

  get_polities() |>
    pointblank::expect_col_exists(all_of(cols)) |>
    pointblank::expect_col_is_integer(all_of(int_cols)) |>
    pointblank::expect_col_is_character(all_of(other_cols)) |>
    pointblank::expect_col_vals_not_null(all_of(non_null_cols)) |>
    pointblank::expect_rows_distinct(polity_name) |>
    pointblank::expect_rows_distinct(polity_code) |>
    pointblank::expect_rows_distinct(display_code) |>
    pointblank::expect_rows_distinct(
      m49_code,
      preconditions = \(df) df |> dplyr::filter(!is.na(m49_code))
    ) |>
    pointblank::expect_col_vals_expr(~ start_year <= end_year) |>
    .expect_consistent_code_info()
})
