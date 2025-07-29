testthat::test_that("get_polities is consistent", {
  cols <- c(
    "polity_code",
    "polity_name",
    "display_code",
    "start_year",
    "end_year",
    "m49_code"
  )

  get_polities() |>
    pointblank::expect_col_exists(all_of(cols)) |>
    pointblank::expect_col_is_integer(
      c("polity_code", "start_year", "end_year")
    ) |>
    pointblank::expect_col_is_character(
      c("polity_name", "display_code", "m49_code")
    ) |>
    pointblank::expect_col_vals_not_null(
      setdiff(cols, "m49_code")
    ) |>
    pointblank::expect_rows_distinct(polity_code) |>
    pointblank::expect_rows_distinct(display_code) |>
    pointblank::expect_rows_distinct(
      m49_code,
      preconditions = \(df) df |> dplyr::filter(!is.na(m49_code))
    ) |>
    pointblank::col_vals_expr(~ start_year <= end_year)
})
