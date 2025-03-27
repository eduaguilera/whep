get_bilateral_trade <- function(file_path) {
  df <-
    file_path |>
    readr::read_csv() |>
    dplyr::rename_with(tolower) |>
    # TODO: Check why NAs in item column
    dplyr::filter(!is.na(item)) |>
    dplyr::group_by(year, area_code, area, element, item, unit) |>
    dplyr::mutate(
      my_cnt = dplyr::n(),
      my_sum_value = sum(value),
      my_country_share = value / sum(value),
      total_coountry_share = sum(country_share),
    ) |>
    dplyr::arrange(.by_group = TRUE)
    # dplyr::filter(my_cnt > 1, !dplyr::near(total_coountry_share, 1, 1e-6)) |>
    # tidyr::nest()

  browser()

  df |>
    pointblank::create_agent() |>
    pointblank::col_vals_expr(
      ~ dplyr::near(my_country_share, country_share, 1e-6)
    ) |>
    pointblank::interrogate() |>
    print()
  # dplyr::select(-area, -area_p) |>
  # dplyr::mutate(
  #   from_code = ifelse(element == "Export", area_code, area_code_p),
  #   to_code = ifelse(element == "Export", area_code_p, area_code),
  #   .keep = "unused",
  #   .before = 1
  # )
}
