.expect_consistent_code_info <- function(polities) {
  polities |>
    tidyr::separate_wider_delim(
      polity_code,
      names = c("code_name", "code_start_year", "code_end_year"),
      delim = "-",
      cols_remove = FALSE
    ) |>
    pointblank::expect_col_vals_expr(
      ~ (is.na(start_year) & code_start_year == "") |
        (!is.na(start_year) & code_start_year == start_year)
    ) |>
    pointblank::expect_col_vals_expr(
      ~ (is.na(end_year) & code_end_year == "") |
        (!is.na(end_year) & code_end_year == end_year)
    ) |>
    dplyr::select(-code_name, -code_start_year, -code_end_year)
}

testthat::test_that("whep_polities is consistent", {
  int_cols <- c("start_year", "end_year")
  chr_cols <- c("polity_name", "polity_code", "display_code")
  all_cols <- c(int_cols, chr_cols, "geometry")

  non_null_cols <- c(
    "polity_name",
    "polity_code",
    "display_code",
    "geometry"
  )

  whep::whep_polities |>
    tibble::as_tibble() |>
    pointblank::expect_col_exists(all_of(all_cols)) |>
    pointblank::expect_col_is_integer(all_of(int_cols)) |>
    pointblank::expect_col_is_character(all_of(chr_cols)) |>
    pointblank::expect_col_vals_not_null(all_of(non_null_cols)) |>
    pointblank::expect_rows_distinct(polity_name) |>
    pointblank::expect_rows_distinct(polity_code) |>
    pointblank::expect_rows_distinct(display_code) |>
    pointblank::expect_col_vals_expr(~ start_year <= end_year) |>
    .expect_consistent_code_info()
})
