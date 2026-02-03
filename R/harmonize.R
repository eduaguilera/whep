#' Harmonize rows labelled "Simple" by summing values
#'
#' @description Sum `value` for rows where `type == "Simple"`.
#'   The results are grouped by `item_code_harm`, `year` and any
#'   additional grouping columns supplied via `...`.
#' @param data A data frame containing at least `item_code_harm`,
#'   `year`, `value` and `type`.
#' @param ... Additional grouping columns supplied as bare names.
#' @return A tibble with columns `item_code_harm`, `year`, `value`
#'   and any additional grouping columns.
#' @export
#' @examples
#' # TODO: add example
#' # df |> harmonize_simple(country)
harmonize_simple <- function(data, ...) {
  data |>
    dplyr::filter(type == "Simple") |>
    dplyr::summarize(
      value = sum(value, na.rm = TRUE),
      .by = c("item_code_harm", "year", ...)
    )
}

#' Harmonize advanced cases with interpolation for 1:N groups
#'
#' @description Harmonize data containing simple, 1:1, N:1 and 1:N
#'   mappings. For 1:N groups this function computes shares across
#'   the full year range and applies them to split values.
#' @param data Data frame with columns `year`, `value`, `items`,
#'   `item_code_harm` and `type`.
#' @param ... Additional grouping columns provided as bare names.
#' @return Tibble with harmonized series (`item_code`, `year`, `value`)
#'   and grouping columns.
#' @export
#' @examples
#' # TODO: add example
#' # harmonize_interpolate(data, country)
harmonize_interpolate <- function(data, ...) {
  grouping_cols <- rlang::enquos(...)
  grouping_names <- .group_names(grouping_cols)
  data_groups <- data |>
    dplyr::filter(type == "1:N") |>
    dplyr::select(items, item_code_harm, !!!grouping_cols) |>
    dplyr::distinct()
  df_simple <- harmonize_simple(data, ...)
  .check_all_simple(data_groups, df_simple)
  harm_groups <- data_groups |>
    dplyr::summarize(
      harm_set = list(unique(item_code_harm)),
      .by = c(items, !!!grouping_cols)
    )
  group_year_presence <- .find_group_year_presence(
    df_simple,
    data_groups,
    harm_groups,
    grouping_cols
  )
  .check_group_year_presence(harm_groups, group_year_presence, grouping_cols)
  year_bounds <- .find_year_bounds(group_year_presence, data, grouping_cols)
  print(year_bounds)
  complex_shares <- .calc_complex_shares(
    data_groups,
    df_simple,
    year_bounds,
    grouping_cols
  )
  .calc_return_harm_int(data, complex_shares, df_simple, grouping_cols)
}

# Return simple harmonizations when no 1:N groups exist
#
# If `data_groups` is empty the function informs the
# user and returns `df_simple` with `item_code` renamed.
.check_all_simple <- function(data_groups, df_simple) {
  if (nrow(data_groups) == 0) {
    cli::cli_inform(c(
      "i" = "only simple harmonization detected, returning simple harmonizations only"
    ))

    df_simple |>
      dplyr::rename(item_code = item_code_harm)
  }
}

# Identify 1:N groups with full year presence
#
# For each `items` group determine which harm members
# are observed for each year and return only groups where all harm
# members are present.
.find_group_year_presence <- function(
  df_simple,
  data_groups,
  harm_groups,
  grouping_cols
) {
  grouping_names <- .group_names(grouping_cols)
  df_simple |>
    dplyr::select(item_code_harm, year, dplyr::all_of(grouping_names)) |>
    dplyr::left_join(
      data_groups,
      by = c("item_code_harm", grouping_names),
      relationship = "many-to-many"
    ) |>
    dplyr::summarize(
      observed_harm = list(unique(item_code_harm)),
      .by = .group_by_chars("items", "year", grouping_cols = grouping_cols)
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

# Abort on incomplete 1:N groups
#
# Abort with an informative message if any 1:N group is
# missing data for some years.
.check_group_year_presence <- function(
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
          dplyr::select(items, !!!grouping_cols) |>
          dplyr::distinct(),
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
      stats::setNames(formatted_rows, rep("*", length(formatted_rows)))
    ))
  }
}

# Compute start and end year bounds for each items group
#
# Combine observed group-year rows with original
# 1:N rows and compute start and end years per `items` group.
.find_year_bounds <- function(group_year_presence, data, grouping_cols) {
  grouping_names <- .group_names(grouping_cols)
  dplyr::bind_rows(
    group_year_presence |>
      dplyr::select(items, year, dplyr::all_of(grouping_names)),
    data |>
      dplyr::filter(type == "1:N") |>
      dplyr::select(items, year, dplyr::all_of(grouping_names))
  ) |>
    dplyr::group_by(items, dplyr::across(dplyr::all_of(grouping_names))) |>
    dplyr::summarize(
      start_year = min(year, na.rm = TRUE),
      end_year = max(year, na.rm = TRUE),
      .groups = "drop"
    )
}

# Calculate value shares for complex 1:N harmonizations
#
# Expand `data_groups` to all years between `start_year`
# and `end_year`, join simple values and compute value shares per
# year. Missing years are filled using `Filling()`.
.calc_complex_shares <- function(
  data_groups,
  df_simple,
  year_bounds,
  grouping_cols
) {
  grouping_names <- .group_names(grouping_cols)
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
      .by = .group_by_chars("items", "year", grouping_cols = grouping_cols)
    ) |>
    dplyr::select(-value, -total_value) |>
    linear_fill(
      value_share,
      year,
      .by = c("items", "item_code_harm", grouping_names)
    ) |>
    dplyr::select(
      items,
      item_code_harm,
      year,
      value_share,
      dplyr::all_of(grouping_names)
    )
}

# Apply computed shares and return harmonized series
#
# Multiply 1:N `value` by `value_share`, bind simple
# series and sum to return harmonized series by `item_code` and
# year.
.calc_return_harm_int <- function(
  data,
  complex_shares,
  df_simple,
  grouping_cols
) {
  grouping_names <- .group_names(grouping_cols)
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
      dplyr::all_of(grouping_names)
    ) |>
    dplyr::bind_rows(df_simple) |>
    dplyr::rename(item_code = item_code_harm) |>
    dplyr::summarize(
      value = sum(value),
      .by = .group_by_chars("item_code", "year", grouping_cols = grouping_cols)
    )
}

# Convert quosures to character column names
#
# Convert a list of quosures (from
# `rlang::enquos()`) to a character vector of column names.
.group_names <- function(grouping_cols) {
  purrr::map_chr(grouping_cols, rlang::as_name)
}

# Build character vector for joins and .by helpers
#
# Create a character vector combining provided prefixes
# and the names from quosures for use in joins, `.by`, `select` or
# complete.
.group_by_chars <- function(..., grouping_cols) {
  c(..., .group_names(grouping_cols))
}

# Group by across helper using quosures
#
# Wrapper around `dplyr::group_by()` that applies
# `dplyr::across()` to the columns named by quosures.
.group_by_across <- function(..., grouping_cols) {
  dplyr::group_by(
    ...,
    dplyr::across(dplyr::all_of(.group_names(grouping_cols)))
  )
}
