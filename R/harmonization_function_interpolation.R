harmonization_function_interpolation <- function(data, ...) {
  grouping_cols <- rlang::enquos(...)

  data_groups <- data |>
    dplyr::filter(type == "1:N") |>
    dplyr::select(items, item_code_harm, !!!grouping_cols) |>
    dplyr::distinct()

  harm_groups <- data_groups |>
    dplyr::group_by(items, !!!grouping_cols) |>
    dplyr::summarize(harm_set = list(unique(item_code_harm)), .groups = "drop")

  df_simple <- data |>
    dplyr::filter(type == "Simple") |>
    dplyr::group_by(item_name_harm, item_code_harm, year, !!!grouping_cols) |>
    dplyr::summarize(value = sum(value), .groups = "drop") |>
    dplyr::ungroup()

  if (nrow(data_groups) == 0) {
    cat(
      "only simple harmonization detected, returning simple harmonizations only"
    )
    return(
      df_simple |>
        dplyr::rename(item_code = item_code_harm, items = item_name_harm)
    )
  }

  group_year_presence <- df_simple |>
    dplyr::select(item_code_harm, year, !!!grouping_cols) |>
    dplyr::left_join(
      data_groups,
      by = c("item_code_harm", names(dplyr::select(data_groups, !!!grouping_cols))),
      relationship = "many-to-many"
    ) |>
    dplyr::group_by(items, year, !!!grouping_cols) |>
    dplyr::summarize(
      observed_harm = list(unique(item_code_harm)),
      .groups = "drop"
    ) |>
    dplyr::left_join(
      harm_groups,
      by = c("items", names(dplyr::select(harm_groups, !!!grouping_cols))),
      relationship = "many-to-many"
    ) |>
    dplyr::mutate(
      all_present = purrr::map2_lgl(harm_set, observed_harm, ~ all(.x %in% .y))
    ) |>
    dplyr::filter(all_present)

  # Check for incomplete groups
  groups_with_complete_years <- group_year_presence |>
    dplyr::select(items, !!!grouping_cols) |>
    dplyr::distinct()

  all_groups <- harm_groups |>
    dplyr::select(items, !!!grouping_cols) |>
    dplyr::distinct()

  incomplete_groups <- dplyr::anti_join(
    all_groups,
    groups_with_complete_years,
    by = c("items", names(dplyr::select(all_groups, !!!grouping_cols)))
  )

  if (nrow(incomplete_groups) > 0) {
    cat(
      "ERROR: Incomplete 1:N groups detected \nRevise following groups to ensure items have data for all years: \n"
    )
    print(incomplete_groups)
    stop("error - incomplete groups. revise data.")
  }

  complex_shares <- data_groups |>
    dplyr::left_join(
      group_year_presence,
      by = c("items", names(dplyr::select(data_groups, !!!grouping_cols))),
      relationship = "many-to-many"
    ) |>
    dplyr::select(items, item_code_harm, year, !!!grouping_cols) |>
    dplyr::left_join(
      df_simple |> dplyr::select(-item_name_harm),
      by = c(
        "year",
        "item_code_harm",
        names(dplyr::select(data_groups, !!!grouping_cols))
      ),
      relationship = "many-to-many"
    ) |>
    dplyr::group_by(items, year, !!!grouping_cols) |>
    dplyr::mutate(
      total_value = sum(value, na.rm = TRUE),
      value_share = value / total_value,
      year = as.numeric(year)
    ) |>
    dplyr::select(-value, -total_value) |>
    dplyr::ungroup() |>
    tidyr::complete(items, item_code_harm, year = 1840:1960, !!!grouping_cols) |>
    dplyr::group_by(items, item_code_harm, !!!grouping_cols) |>
    Filling(value_share, year) |>
    dplyr::select(items, item_code_harm, year, value_share, !!!grouping_cols)

  return_tibble <- data |>
    dplyr::filter(type == "1:N") |>
    dplyr::left_join(
      complex_shares,
      by = c(
        "items",
        "item_code_harm",
        "year",
        names(dplyr::select(data, !!!grouping_cols))
      ),
      relationship = "many-to-many"
    ) |>
    dplyr::mutate(value = value * value_share) |>
    dplyr::select(year, item_name_harm, item_code_harm, value, !!!grouping_cols) |>
    dplyr::bind_rows(df_simple) |>
    dplyr::rename(items = item_name_harm, item_code = item_code_harm) |>
    dplyr::group_by(items, item_code, year, !!!grouping_cols) |>
    dplyr::summarize(value = sum(value), .groups = "drop")

  return(return_tibble)
}
