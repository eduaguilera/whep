testthat::test_that("get_polities is consistent", {
  cols <- c(
    "polity_name",
    "polity_code",
    "start_year",
    "end_year",
    "m49_code",
    "iso2_code",
    "iso3_code",
    "notes_m49",
    "notes_faostat",
    "notes_federico_tena",
    "name_m49",
    "name_faostat",
    "name_federico_tena",
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
    pointblank::expect_rows_distinct(polity_code) |>
    pointblank::expect_rows_distinct(display_code) |>
    pointblank::expect_rows_distinct(
      m49_code,
      preconditions = \(df) df |> dplyr::filter(!is.na(m49_code))
    ) |>
    pointblank::col_vals_expr(~ start_year <= end_year)
})
