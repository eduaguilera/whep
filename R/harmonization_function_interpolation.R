harmonization_function_interpolation_updated <- function(data) {
  data_groups <- data |>
    dplyr::filter(type == "1:N") |>
    dplyr::select(items, item_code_harm) |>
    dplyr::distinct()

  harm_groups <- data_groups |>
    dplyr::group_by(items) |>
    dplyr::summarize(harm_set = list(unique(item_code_harm)), .groups = "drop")

  df_simple <- data |>
    dplyr::filter(type == "Simple") |>
    dplyr::group_by(item_name_harm, item_code_harm, year, measurement) |>
    dplyr::summarize(value = sum(value), .groups = "drop") |>
    dplyr::ungroup()

  if (nrow(data_groups) == 0) {
    print(
      "only simple harmonization detected, returning simple harmonizations only"
    )
    return(
      df_simple |>
        dplyr::rename(item_code = item_code_harm, items = item_name_harm)
    )
  }

  group_year_presence <- df_simple |>
    dplyr::select(item_code_harm, year) |>
    dplyr::left_join(data_groups, by = "item_code_harm") |>
    dplyr::group_by(items, year) |>
    dplyr::summarize(
      observed_harm = list(unique(item_code_harm)),
      .groups = "drop"
    ) |>
    dplyr::left_join(harm_groups, by = "items") |>
    dplyr::mutate(
      all_present = purrr::map2_lgl(harm_set, observed_harm, ~ all(.x %in% .y))
    ) |>
    dplyr::filter(all_present)

  groups_with_complete_years <- unique(group_year_presence$items)
  all_groups <- unique(harm_groups$items)
  incomplete_groups <- setdiff(all_groups, groups_with_complete_years)

  if (length(incomplete_groups) > 0) {
    stop("error - incomplete groups. revise data.")
  }

  complex_shares <- data_groups |>
    dplyr::left_join(group_year_presence, by = "items") |>
    dplyr::select(items, item_code_harm, year) |>
    dplyr::left_join(
      df_simple |> dplyr::select(-item_name_harm, -measurement),
      by = c("year", "item_code_harm")
    ) |>
    dplyr::group_by(items, year) |>
    dplyr::mutate(
      total_value = sum(value, na.rm = TRUE),
      value_share = value / total_value,
      year = as.numeric(year)
    ) |>
    dplyr::select(-value, -total_value) |>
    dplyr::ungroup() |>
    tidyr::complete(items, item_code_harm, year = 1840:1960) |>
    dplyr::group_by(items, item_code_harm) |>
    Filling(value_share, year) |>
    dplyr::select(items, item_code_harm, year, value_share)

  return_tibble <- data |>
    dplyr::filter(type == "1:N") |>
    dplyr::left_join(
      complex_shares,
      by = c("items", "item_code_harm", "year")
    ) |>
    dplyr::mutate(value = value * value_share) |>
    dplyr::select(year, item_name_harm, item_code_harm, value, measurement) |>
    dplyr::bind_rows(df_simple) |>
    dplyr::rename(items = item_name_harm, item_code = item_code_harm) |>
    dplyr::group_by(items, item_code, year, measurement) |>
    dplyr::summarize(value = sum(value), .groups = "drop")

  return(return_tibble)
}
