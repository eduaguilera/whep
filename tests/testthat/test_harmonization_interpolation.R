library("testthat")

test_tibble_input <- tibble(
  items = c("prodone, prodtwo, prodonetwo, prodonetwo, prodone, prodtwo"),
  measurement = "lbs",
  year = c(1900, 1900, 1901, 1901, 1902, 1902),
  value = c(10, 0, 10, 10, 0, 10),
  item_code_harm = c(1, 2, 1, 2, 1, 2),
  item_name_harm = c("one", "two", "one", "two", "one", "two"),
  type = c("Simple", "Simple", "1:N", "1:N", "Simple", "Simple")
)

test_tibble_output <- tibble(
  items = c("one", "two", "one", "two", "one", "two"),
  item_code = c(1, 2, 1, 2, 1, 2),
  year = c(1900, 1900, 1901, 1901, 1902, 1902),
  measurement = "lbs",
  value = c(10, 0, 5, 5, 0, 10)
)

print(
  harmonization_function_interpolation(test_tibble_input) |>
    arrange(year, item_code)
)


test_that("Harmonization is completed successfully", {
  test_tibble_input <- tibble(
    items = c("prodone, prodtwo, prodonetwo, prodonetwo, prodone, prodtwo"),
    measurement = "lbs",
    year = c(1900, 1900, 1901, 1901, 1902, 1902),
    value = c(10, 0, 10, 10, 0, 10),
    item_code_harm = c(1, 2, 1, 2, 1, 2),
    item_name_harm = c("one", "two", "one", "two", "one", "two"),
    type = c("Simple", "Simple", "1:N", "1:N", "Simple", "Simple")
  )
  test_tibble_output <- tibble(
    items = c("one", "two", "one", "two", "one", "two"),
    item_code = c(1, 2, 1, 2, 1, 2),
    year = c(1900, 1900, 1901, 1901, 1902, 1902),
    measurement = "lbs",
    value = c(10, 0, 5, 5, 0, 10)
  )

  actual <-
    test_tibble_input |>
    harmonization_function_interpolation(measurement) |>
    arrange(year, item_code)

  expect_equal(
    actual,
    test_tibble_output
  )
})


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
