# convert quosures -> character column names
group_names <- function(grouping_cols) {
  purrr::map_chr(grouping_cols, rlang::as_name)
}
# helper for joins / .by / select / complete
group_by_chars <- function(..., grouping_cols) {
  c(..., group_names(grouping_cols))
}
# helper for group_by()
group_by_across <- function(..., grouping_cols) {
  dplyr::group_by(..., across(all_of(group_names(grouping_cols))))
}

#' harm_simple(data, ...)
#' Harmonizes simple data
#' Input
#'    - data (dataframe to be harmonized)
#'       - col1: item_code_harm
#'       - col2: year
#'       - col3: value
#'       - col4: type
#'    - ... (additional columns)
#' Returns: Dataframe of harmonized data
#' Return structure:
#'    - item_code_harm: numeric (harmonized code of item)
#'    - year: numeric (year of observation)
#'    - value: numeric (value of observation)
#'    - additional columns (...)

harm_simple <- function(data, ...) {
  grouping_cols <- rlang::enquos(...)
  data |>
    dplyr::filter(type == "Simple") |>
    dplyr::summarize(
      value = sum(value, na.rm = TRUE),
      .by = c(
        "item_code_harm",
        "year",
        !!!grouping_cols
      )
    )
}

#' check_all_simple(data_groups, df_simple)
#' Checks if all the series is all simple of if there are 1:N
#' Input
#'    - data_groups
#'    - df_simple
#' Return: df_simple
#'
#'

check_all_simple <- function(data_groups, df_simple) {
  if (nrow(data_groups) == 0) {
    cli::cli_inform(c(
      "i" = "only simple harmonization detected, returning simple harmonizations only"
    ))
    return(
      df_simple |>
        dplyr::rename(item_code = item_code_harm)
    )
  }
}

find_group_year_presence <- function(
  df_simple,
  data_groups,
  harm_groups,
  grouping_cols
) {
  grouping_names <- group_names(grouping_cols)
  df_simple |>
    dplyr::select(item_code_harm, year, all_of(grouping_names)) |>
    dplyr::left_join(
      data_groups,
      by = c("item_code_harm", grouping_names),
      relationship = "many-to-many"
    ) |>
    dplyr::summarize(
      observed_harm = list(unique(item_code_harm)),
      .by = group_by_chars("items", "year", grouping_cols = grouping_cols)
    ) |>
    dplyr::left_join(
      harm_groups,
      by = c("items", grouping_names),
      relationship = "many-to-many"
    ) |>
    dplyr::mutate(
      all_present = purrr::map2_lgl(harm_set, observed_harm, ~ all(.x %in% .y))
    ) |>
    dplyr::filter(all_present)
}

check_group_year_presence <- function(
  harm_groups,
  group_year_presence,
  grouping_cols
) {
  incomplete_groups <- dplyr::anti_join(
    harm_groups |>
      dplyr::select(items, !!!grouping_cols) |>
      dplyr::distinct(),
    group_year_presence |>
      dplyr::select(items, !!!grouping_cols) |>
      dplyr::distinct(),
    by = c(
      "items",
      names(dplyr::select(
        harm_groups |>
          select(items, !!!grouping_cols) |>
          distinct(),
        !!!grouping_cols
      ))
    )
  )
  if (nrow(incomplete_groups) > 0) {
    formatted_rows <- with(incomplete_groups, paste0(items, " in ", country))
    msg <- c(
      "x" = "ERROR: Incomplete 1:N groups detected",
      "i" = "Revise following groups to ensure items have data for all years:"
    )
    cli::cli_abort(c(
      msg,
      setNames(formatted_rows, rep("*", length(formatted_rows)))
    ))
  }
}

find_year_bounds <- function(group_year_presence, data, grouping_cols) {
  grouping_names <- group_names(grouping_cols)
  dplyr::bind_rows(
    group_year_presence |>
      dplyr::select(items, year, all_of(grouping_names)),

    data |>
      dplyr::filter(type == "1:N") |>
      dplyr::select(items, year, all_of(grouping_names))
  ) |>
    dplyr::group_by(items, across(all_of(grouping_names))) |>
    dplyr::summarize(
      start_year = min(year, na.rm = TRUE),
      end_year = max(year, na.rm = TRUE),
      .groups = "drop"
    )
}

calc_complex_shares <- function(
  data_groups,
  df_simple,
  year_bounds,
  grouping_cols
) {
  grouping_names <- group_names(grouping_cols)
  data_groups |>
    dplyr::left_join(
      year_bounds,
      by = c("items", grouping_names),
      relationship = "many-to-many"
    ) |>
    dplyr::mutate(
      year = purrr::map2(start_year, end_year, seq)
    ) |>
    tidyr::unnest(year) |>
    dplyr::select(-end_year, -start_year) |>
    dplyr::left_join(
      df_simple,
      by = c("year", "item_code_harm", grouping_names),
      relationship = "many-to-many"
    ) |>
    dplyr::mutate(
      total_value = sum(value, na.rm = TRUE),
      value_share = value / total_value,
      year = as.numeric(year),
      .by = group_by_chars("items", "year", grouping_cols = grouping_cols)
    ) |>
    dplyr::select(-value, -total_value) |>
    group_by_across(items, item_code_harm, grouping_cols = grouping_cols) |>
    Filling(value_share, year) |>
    dplyr::select(
      items,
      item_code_harm,
      year,
      value_share,
      all_of(grouping_names)
    )
}

calc_return_harm_int <- function(
  data,
  complex_shares,
  df_simple,
  grouping_cols
) {
  grouping_names <- group_names(grouping_cols)
  data |>
    dplyr::filter(type == "1:N") |>
    dplyr::left_join(
      complex_shares,
      by = c("items", "item_code_harm", "year", grouping_names),
      relationship = "many-to-many"
    ) |>
    dplyr::mutate(value = value * value_share) |>
    dplyr::select(
      year,
      item_code_harm,
      value,
      all_of(grouping_names)
    ) |>
    dplyr::bind_rows(df_simple) |>
    dplyr::rename(item_code = item_code_harm) |>
    dplyr::summarize(
      value = sum(value),
      .by = group_by_chars("item_code", "year", grouping_cols = grouping_cols)
    )
}

#' harm_advanced_int(data, ...)
#' Harmonizes data with simple 1:1, N:1, and 1:N combinations.
#' Input:
#'   - data
#'     - Dataframe with data you wish to harmonize, with harmonization table
#'       already attached
#'     - Columns
#'       - year (numeric): year of observation
#'       - value (numeric): value of observation
#'       - items (string): name of observation
#'       - item_code_harm (numeric): code of harmonized item
#'       - type (string): either "Simple" or "1:N"
#'   - ...
#'     - column names for grouping
#' Output:
#'   -

harm_advanced_int <- function(data, ...) {
  grouping_cols <- rlang::enquos(...)
  grouping_names <- group_names(grouping_cols)
  data_groups <- data |>
    dplyr::filter(type == "1:N") |>
    dplyr::select(items, item_code_harm, !!!grouping_cols) |>
    dplyr::distinct()
  df_simple <- harm_simple(data, ...)
  check_all_simple(data_groups, df_simple)
  harm_groups <- data_groups |>
    dplyr::summarize(
      harm_set = list(unique(item_code_harm)),
      .by = c(items, !!!grouping_cols)
    )
  group_year_presence <- find_group_year_presence(
    df_simple,
    data_groups,
    harm_groups,
    grouping_cols
  )
  check_group_year_presence(harm_groups, group_year_presence, grouping_cols)
  year_bounds <- find_year_bounds(group_year_presence, data, grouping_cols)
  print(year_bounds)
  complex_shares <- calc_complex_shares(
    data_groups,
    df_simple,
    year_bounds,
    grouping_cols
  )
  return(calc_return_harm_int(data, complex_shares, df_simple, grouping_cols))
}
