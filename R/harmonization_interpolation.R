Filling <- function(data, var, Index) {
  #data=dataframe, var=variable to be filled, Index=time index (usually year)
  data |>
    dplyr::mutate(
      Value_interpfilled = zoo::na.approx(
        {{ var }},
        x = zoo::index({{ Index }}),
        na.rm = FALSE
      ),
      Value_CarriedBackward = zoo::na.locf0({{ var }}),
      Value_CarriedForward = zoo::na.locf0({{ var }}, fromLast = TRUE),
      "Source_{{var}}" := ifelse(
        !is.na({{ var }}),
        "Original",
        ifelse(
          !is.na(Value_interpfilled),
          "Linear interpolation",
          ifelse(
            !is.na(Value_CarriedBackward),
            "Last value carried forward",
            "First value carried backwards"
          )
        )
      ),
      "{{var}}" := ifelse(
        !is.na({{ var }}),
        {{ var }},
        ifelse(
          !is.na(Value_interpfilled),
          Value_interpfilled,
          ifelse(
            !is.na(Value_CarriedBackward),
            Value_CarriedBackward,
            Value_CarriedForward
          )
        )
      )
    ) |>
    dplyr::select(
      -Value_interpfilled,
      -Value_CarriedForward,
      -Value_CarriedBackward,
      -Value_CarriedForward
    )
}

#' Bilateral trade data
#'
#' @description
#' Harmonizes trade data
#'
#' @param data original dataframe merged with harmonization table
#'
#' @returns
#' A tibble with the reported trade between countries.
#' It contains the following columns:
#' "items", "item_code", "year", "measurement"
#' - `year`: The year in which the trade occurred.
#' - `item_code`: FAOSTAT internal code for the product that is being traded
#' - `items`: Name of Item Traded, in FAOSTAT internal.
#' - `item`: Natural language name for the item that is being traded.
#' - `measurement`: Measure unit for the traded item. It can have two values:
#'      - 1000 MT: 1000 Metric Tonnes
#'      - 1000 Heads: 1000 Heads
#' - `value`: The amount traded in the corresponding measure measurement.
#'
#'
#' @export
#'
#' @examples
#'   test_tibble_input <- tibble(
#'      items = c("prodone, prodtwo, prodonetwo, prodonetwo, prodone, prodtwo"),
#'      measurement = "lbs",
#'      year = c(1900, 1900, 1901, 1901, 1902, 1902),
#'      value = c(10, 0, 10, 10, 0, 10),
#'      item_code_harm = c(1, 2, 1, 2, 1, 2),
#'      item_name_harm = c("one", "two", "one", "two", "one", "two"),
#'      type = c("Simple", "Simple", "1:N", "1:N", "Simple", "Simple")
#'   )
#'   harmonization_function_interpolation(test_tibble_input)
#'

harmonization_function_interpolation <- function(data) {
  data_groups <- data |>
    filter(type == "1:N") |>
    select(items, item_code_harm) |>
    distinct()

  harm_groups <- data_groups |>
    group_by(items) |>
    summarize(harm_set = list(unique(item_code_harm)), .groups = "drop")

  df_simple <- data |>
    filter(type == "Simple") |>
    group_by(item_name_harm, item_code_harm, year, measurement) |>
    summarize(value = sum(value)) |>
    ungroup()

  if (nrow(data_groups) == 0) {
    return(
      df_simple |>
        rename(item_code = item_code_harm, items = item_name_harm)
    )
  }

  group_year_presence <- df_simple |>
    select(item_code_harm, year) |>
    left_join(
      data_groups,
      by = "item_code_harm",
      relationship = "many-to-many"
    ) |>
    group_by(items, year) |>
    summarize(
      observed_harm = list(unique(item_code_harm)),
      .groups = "drop"
    ) |>
    left_join(harm_groups, by = "items", relationship = "many-to-many") |>
    mutate(
      all_present = map2_lgl(harm_set, observed_harm, ~ all(.x %in% .y))
    ) |>
    filter(all_present)

  complex_shares <- data_groups |>
    left_join(
      group_year_presence,
      by = "items",
      relationship = "many-to-many"
    ) |>
    select(items, item_code_harm, year) |>
    left_join(
      df_simple |> select(-item_name_harm, -measurement),
      by = c("year", "item_code_harm"),
      relationship = "many-to-many"
    ) |>
    group_by(items, year) |>
    mutate(
      total_value = sum(value, na.rm = T),
      value_share = value / total_value,
      year = as.numeric(year)
    ) |>
    select(-value, -total_value) |>
    ungroup() |>
    complete(items, item_code_harm, year = 1840:1960) |>
    group_by(items, item_code_harm) |>
    Filling(value_share, year) |>
    select(items, item_code_harm, year, value_share)

  return_tibble <- data |>
    filter(type == "1:N") |>
    left_join(
      complex_shares,
      by = c("items", "item_code_harm", "year"),
      relationship = "many-to-many"
    ) |>
    mutate(value = value * value_share) |>
    select(year, item_name_harm, item_code_harm, value, measurement) |>
    bind_rows(df_simple) |>
    rename(items = item_name_harm, item_code = item_code_harm) |>
    summarize(
      value = sum(value),
      .by = c("items", "item_code", "year", "measurement")
    )

  return(return_tibble)
}
