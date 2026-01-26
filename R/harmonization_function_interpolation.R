library(tidyverse)
library(openxlsx)
library(readxl)
library(googlesheets4)

# Begin googlesheets4 process
googlesheets4::gs4_deauth()

# Product Conversions
test_sheet_url <- "1Ei_cinVEXqDdp6DU83yAmCM63DBPn7LiFWgKa9B16aA"

test_data_sheet <-
  test_sheet_url |>
  googlesheets4::read_sheet(sheet = "data")

test_harm_sheet <-
  test_sheet_url |>
  googlesheets4::read_sheet(sheet = "harm table")


harmonization_function_interpolation_updated <- function(data) {
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
    print(
      'only simple harmonization detected, returning simple harmonizations only'
    )
    return(
      df_simple |>
        rename(item_code = item_code_harm, items = item_name_harm)
    )
  }

  group_year_presence <- df_simple |>
    select(item_code_harm, year) |>
    left_join(data_groups, by = "item_code_harm") |>
    group_by(items, year) |>
    summarize(
      observed_harm = list(unique(item_code_harm)),
      .groups = "drop"
    ) |>
    left_join(harm_groups, by = "items") |>
    mutate(
      all_present = map2_lgl(harm_set, observed_harm, ~ all(.x %in% .y))
    ) |>
    filter(all_present)

  # Check for incomplete groups
  groups_with_complete_years <- unique(group_year_presence$items)
  all_groups <- unique(harm_groups$items)
  incomplete_groups <- setdiff(all_groups, groups_with_complete_years)

  if (length(incomplete_groups) > 0) {
    stop("error - incomplete groups. revise data.")
  }

  complex_shares <- data_groups |>
    left_join(group_year_presence, by = "items") |>
    select(items, item_code_harm, year) |>
    left_join(
      df_simple |> select(-item_name_harm, -measurement),
      by = c("year", "item_code_harm")
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
    left_join(complex_shares, by = c("items", "item_code_harm", "year")) |>
    mutate(value = value * value_share) |>
    select(year, item_name_harm, item_code_harm, value, measurement) |>
    bind_rows(df_simple) |>
    rename(items = item_name_harm, item_code = item_code_harm) |>
    group_by(items, item_code, year, measurement) |>
    summarize(value = sum(value))

  return(return_tibble)
}


harmonization_function_interpolation <- function(data, ...) {
  grouping_cols <- enquos(...)

  data_groups <- data |>
    filter(type == "1:N") |>
    select(items, item_code_harm, !!!grouping_cols) |>
    distinct()

  harm_groups <- data_groups |>
    group_by(items, !!!grouping_cols) |>
    summarize(harm_set = list(unique(item_code_harm)), .groups = "drop")

  df_simple <- data |>
    filter(type == "Simple") |>
    group_by(item_name_harm, item_code_harm, year, !!!grouping_cols) |>
    summarize(value = sum(value), .groups = "drop") |>
    ungroup()

  if (nrow(data_groups) == 0) {
    cat(
      'only simple harmonization detected, returning simple harmonizations only'
    )
    return(
      df_simple |>
        rename(item_code = item_code_harm, items = item_name_harm)
    )
  }

  group_year_presence <- df_simple |>
    select(item_code_harm, year, !!!grouping_cols) |>
    left_join(
      data_groups,
      by = c("item_code_harm", names(select(data_groups, !!!grouping_cols))),
      relationship = "many-to-many"
    ) |>
    group_by(items, year, !!!grouping_cols) |>
    summarize(
      observed_harm = list(unique(item_code_harm)),
      .groups = "drop"
    ) |>
    left_join(
      harm_groups,
      by = c("items", names(select(harm_groups, !!!grouping_cols))),
      relationship = "many-to-many"
    ) |>
    mutate(
      all_present = map2_lgl(harm_set, observed_harm, ~ all(.x %in% .y))
    ) |>
    filter(all_present)

  # Check for incomplete groups
  groups_with_complete_years <- group_year_presence |>
    select(items, !!!grouping_cols) |>
    distinct()

  all_groups <- harm_groups |>
    select(items, !!!grouping_cols) |>
    distinct()

  incomplete_groups <- anti_join(
    all_groups,
    groups_with_complete_years,
    by = c("items", names(select(all_groups, !!!grouping_cols)))
  )

  if (nrow(incomplete_groups) > 0) {
    cat(
      "ERROR: Incomplete 1:N groups detected \nRevise following groups to ensure items have data for all years: \n"
    )
    print(incomplete_groups)
    stop("error - incomplete groups. revise data.")
  }

  complex_shares <- data_groups |>
    left_join(
      group_year_presence,
      by = c("items", names(select(data_groups, !!!grouping_cols))),
      relationship = "many-to-many"
    ) |>
    select(items, item_code_harm, year, !!!grouping_cols) |>
    left_join(
      df_simple |> select(-item_name_harm),
      by = c(
        "year",
        "item_code_harm",
        names(select(data_groups, !!!grouping_cols))
      ),
      relationship = "many-to-many"
    ) |>
    group_by(items, year, !!!grouping_cols) |>
    mutate(
      total_value = sum(value, na.rm = T),
      value_share = value / total_value,
      year = as.numeric(year)
    ) |>
    select(-value, -total_value) |>
    ungroup() |>
    complete(items, item_code_harm, year = 1840:1960, !!!grouping_cols) |>
    group_by(items, item_code_harm, !!!grouping_cols) |>
    Filling(value_share, year) |>
    select(items, item_code_harm, year, value_share, !!!grouping_cols)

  return_tibble <- data |>
    filter(type == "1:N") |>
    left_join(
      complex_shares,
      by = c(
        "items",
        "item_code_harm",
        "year",
        names(select(data, !!!grouping_cols))
      ),
      relationship = "many-to-many"
    ) |>
    mutate(value = value * value_share) |>
    select(year, item_name_harm, item_code_harm, value, !!!grouping_cols) |>
    bind_rows(df_simple) |>
    rename(items = item_name_harm, item_code = item_code_harm) |>
    group_by(items, item_code, year, !!!grouping_cols) |>
    summarize(value = sum(value), .groups = "drop")

  return(return_tibble)
}

test_data_sheet_err <- test_data_sheet |>
  pivot_longer(
    cols = starts_with("1"),
    names_to = "year",
    values_to = "value"
  ) |>
  mutate(year = as.numeric(year)) |>
  filter(items != "prodfivesix") |>
  left_join(test_harm_sheet, by = c("items"), relationship = "many-to-many") |>
  filter(!is.na(value)) |>
  harmonization_function_interpolation_extracols(measurement, country)


harmonization_function_interpolation_extracols <- function(data, ...) {
  # Capture additional grouping columns
  grouping_cols <- enquos(...)

  data_groups <- data |>
    filter(type == "1:N") |>
    select(items, item_code_harm, !!!grouping_cols) |>
    distinct()

  harm_groups <- data_groups |>
    group_by(items, !!!grouping_cols) |>
    summarize(harm_set = list(unique(item_code_harm)), .groups = "drop")

  df_simple <- data |>
    filter(type == "Simple") |>
    group_by(
      item_name_harm,
      item_code_harm,
      year,
      measurement,
      !!!grouping_cols
    ) |>
    summarize(value = sum(value), .groups = "drop") |>
    ungroup()

  if (nrow(data_groups) == 0) {
    cat(
      'only simple harmonization detected, returning simple harmonizations only'
    )
    return(
      df_simple |>
        rename(item_code = item_code_harm, items = item_name_harm)
    )
  }

  group_year_presence <- df_simple |>
    select(item_code_harm, year, !!!grouping_cols) |>
    left_join(
      data_groups,
      by = c("item_code_harm", names(select(data_groups, !!!grouping_cols))),
      relationship = "many-to-many"
    ) |>
    group_by(items, year, !!!grouping_cols) |>
    summarize(
      observed_harm = list(unique(item_code_harm)),
      .groups = "drop"
    ) |>
    left_join(
      harm_groups,
      by = c("items", names(select(harm_groups, !!!grouping_cols))),
      relationship = "many-to-many"
    ) |>
    mutate(
      all_present = map2_lgl(harm_set, observed_harm, ~ all(.x %in% .y))
    ) |>
    filter(all_present)

  # Check for incomplete groups
  groups_with_complete_years <- group_year_presence |>
    select(items, !!!grouping_cols) |>
    distinct()

  all_groups <- harm_groups |>
    select(items, !!!grouping_cols) |>
    distinct()

  incomplete_groups <- anti_join(
    all_groups,
    groups_with_complete_years,
    by = c("items", names(select(all_groups, !!!grouping_cols)))
  )

  if (nrow(incomplete_groups) > 0) {
    cat(
      "ERROR: Incomplete 1:N groups detected \nRevise following groups to ensure items have data for all years: \n"
    )
    print(incomplete_groups)
    stop("error - incomplete groups. revise data.")
  }

  complex_shares <- data_groups |>
    left_join(
      group_year_presence,
      by = c("items", names(select(data_groups, !!!grouping_cols))),
      relationship = "many-to-many"
    ) |>
    select(items, item_code_harm, year, !!!grouping_cols) |>
    left_join(
      df_simple |> select(-item_name_harm, -measurement),
      by = c(
        "year",
        "item_code_harm",
        names(select(data_groups, !!!grouping_cols))
      ),
      relationship = "many-to-many"
    ) |>
    group_by(items, year, !!!grouping_cols) |>
    mutate(
      total_value = sum(value, na.rm = T),
      value_share = value / total_value,
      year = as.numeric(year)
    ) |>
    select(-value, -total_value) |>
    ungroup() |>
    complete(items, item_code_harm, year = 1840:1960, !!!grouping_cols) |>
    group_by(items, item_code_harm, !!!grouping_cols) |>
    Filling(value_share, year) |>
    select(items, item_code_harm, year, value_share, !!!grouping_cols)

  return_tibble <- data |>
    filter(type == "1:N") |>
    left_join(
      complex_shares,
      by = c(
        "items",
        "item_code_harm",
        "year",
        names(select(data, !!!grouping_cols))
      ),
      relationship = "many-to-many"
    ) |>
    mutate(value = value * value_share) |>
    select(
      year,
      item_name_harm,
      item_code_harm,
      value,
      measurement,
      !!!grouping_cols
    ) |>
    bind_rows(df_simple) |>
    rename(items = item_name_harm, item_code = item_code_harm) |>
    group_by(items, item_code, year, measurement, !!!grouping_cols) |>
    summarize(value = sum(value), .groups = "drop")

  return(return_tibble)
}
