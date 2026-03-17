#' Harmonize rows labeled "simple" by summing values
#'
#' @description Sum `value` for rows where `type == "simple"`.
#'   This covers both 1:1 and N:1 item mappings, since in both
#'   cases the values are simply summed. The results are grouped
#'   by `item_code_harm`, `year` and any additional grouping
#'   columns supplied via `...`.
#' @param data A data frame containing at least columns:
#'   - `item_code_harm`: Numeric, code for harmonized item.
#'   - `year`: Numeric, year of observation.
#'   - `value`: Numeric, value of observation.
#'   - `type`: String, harmonization type. Only `"simple"` rows
#'     are used.
#' @param ... Additional grouping columns supplied as bare names.
#' @return A tibble with columns:
#'   - `item_code_harm`: Numeric, code for harmonized item.
#'   - `year`: Numeric, year of observation.
#'   - `value`: Numeric, summed value of observation.
#'   - and any additional grouping columns.
#' @export
#' @examples
#' # 1:1 mapping: one original item -> one harmonized code
#' df_one_to_one <- tibble::tribble(
#'   ~item_code_harm, ~year, ~value, ~type,
#'   1,               2000,  10,     "simple",
#'   2,               2000,   3,     "simple",
#'   1,               2001,  12,     "simple",
#'   2,               2001,   5,     "simple"
#' )
#' harmonize_simple(df_one_to_one)
#'
#' # N:1 mapping: multiple items map to the same code
#' df_many_to_one <- tibble::tribble(
#'   ~item_code_harm, ~year, ~value, ~type,
#'   1,               2000,   4,     "simple",
#'   1,               2000,   6,     "simple",
#'   2,               2000,   3,     "simple"
#' )
#' harmonize_simple(df_many_to_one)
#'
#' # With an extra grouping column (e.g. country)
#' df_grouped <- tibble::tribble(
#'   ~item_code_harm, ~year, ~value, ~type,    ~country,
#'   1,               2000,   4,     "simple", "usa",
#'   1,               2000,   6,     "simple", "usa",
#'   1,               2000,   9,     "simple", "germany",
#'   2,               2000,   3,     "simple", "usa"
#' )
#' harmonize_simple(df_grouped, country)
#'
#' # Rows with type != "simple" are ignored
#' df_mixed <- tibble::tribble(
#'   ~item_code_harm, ~year, ~value, ~type,
#'   1,               2000,  10,     "simple",
#'   1,               2000,  99,     "1:n",
#'   2,               2000,   3,     "simple"
#' )
#' harmonize_simple(df_mixed)
harmonize_simple <- function(data, ...) {
  data |>
    dplyr::filter(type == "simple") |>
    dplyr::summarise(
      value = sum(value, na.rm = TRUE),
      .by = c("item_code_harm", "year", ...)
    )
}

#' Harmonize advanced cases with interpolation for 1:N groups
#'
#' @description Harmonize data containing `"simple"` and `"1:n"`
#'   mappings. `"simple"` covers both 1:1 and N:1 relationships
#'   (values are summed). For `"1:n"` groups (one original item
#'   splits into several harmonized items) this function computes
#'   value shares across the full year range, interpolates missing
#'   shares, and applies them to split values.
#'
#'   **Important for 1:n mappings**: For each original item that
#'   splits into multiple harmonized items (e.g., "wheatrice" into
#'   "wheat" and "rice"), provide **one row per target
#'   `item_code_harm`**. Each row should have the same `item`,
#'   `year`, and `value`, differing only in `item_code_harm`. For
#'   example, to disaggregate "wheatrice":
#'   - Row 1: `item` = "wheatrice", `item_code_harm` = 1
#'   - Row 2: `item` = "wheatrice", `item_code_harm` = 2
#'
#'   Do not provide a single row; the function will not create
#'   duplicates automatically.
#' @param data A data frame containing at least columns:
#'   - `item`: String, original item name.
#'   - `item_code_harm`: Numeric, code for harmonized item.
#'   - `year`: Numeric, year of observation.
#'   - `value`: Numeric, value of observation.
#'   - `type`: String, `"simple"` or `"1:n"`.
#' @param ... Additional grouping columns provided as bare names.
#' @return A tibble with columns:
#'   - `item_code`: Numeric, code for harmonized item.
#'   - `year`: Numeric, year of observation.
#'   - `value`: Numeric, summed value of observation.
#'   - and any additional grouping columns.
#' @export
#' @examples
#' # Simple-only data (no 1:n rows)
#' df_simple <- tibble::tribble(
#'   ~item,      ~item_code_harm, ~year, ~value, ~type,
#'   "wheat",    1,               2000,   5,     "simple",
#'   "barley",   2,               2000,   3,     "simple",
#'   "oats",     2,               2000,   2,     "simple"
#' )
#' harmonize_interpolate(df_simple)
#'
#' # Mixed simple + 1:n data
#' df_mixed <- tibble::tribble(
#'   ~item,       ~item_code_harm, ~year, ~value, ~type,
#'   "wheatrice", 1,               2000,  20,     "1:n",
#'   "wheatrice", 2,               2000,  20,     "1:n",
#'   "wheat",     1,               2000,   8,     "simple",
#'   "rice",      2,               2000,  12,     "simple"
#' )
#' harmonize_interpolate(df_mixed)
#'
#' # Multiple years with share interpolation
#' # Shares are known in 2000 and 2002; 2001 is interpolated.
#' df_years <- tibble::tribble(
#'   ~item,       ~item_code_harm, ~year, ~value, ~type,
#'   "wheat",     1,               2000,   6,     "simple",
#'   "rice",      2,               2000,   4,     "simple",
#'   "wheatrice", 1,               2001,  10,     "1:n",
#'   "wheatrice", 2,               2001,  10,     "1:n",
#'   "wheat",     1,               2002,   8,     "simple",
#'   "rice",      2,               2002,   2,     "simple"
#' )
#' harmonize_interpolate(df_years)
#'
#' # With extra grouping columns
#' df_grouped <- tibble::tribble(
#'   ~item,       ~item_code_harm, ~year, ~value, ~type,    ~country,
#'   "wheat",     1,               2000,   6,     "simple", "usa",
#'   "rice",      2,               2000,   4,     "simple", "usa",
#'   "wheatrice", 1,               2001,  10,     "1:n",    "usa",
#'   "wheatrice", 2,               2001,  10,     "1:n",    "usa",
#'   "wheat",     1,               2002,   8,     "simple", "usa",
#'   "rice",      2,               2002,   2,     "simple", "usa",
#'   "wheat",     1,               2002,   8,     "simple", "germany"
#' )
#' harmonize_interpolate(df_grouped, country)
harmonize_interpolate <- function(data, ...) {
  grouping_cols <- rlang::enquos(...)
  data_groups <- data |>
    dplyr::filter(type == "1:n") |>
    dplyr::distinct(item, item_code_harm, !!!grouping_cols)
  df_simple <- harmonize_simple(data, ...)

  if (nrow(data_groups) == 0) {
    cli::cli_inform(c(
      "i" = "Only simple harmonization detected,
        returning simple harmonizations only."
    ))
    return(df_simple |> dplyr::rename(item_code = item_code_harm))
  }

  harm_groups <- data_groups |>
    dplyr::summarise(
      harm_set = list(unique(item_code_harm)),
      .by = c(item, !!!grouping_cols)
    )

  group_year_presence <- .find_group_year_presence(
    df_simple,
    data_groups,
    harm_groups,
    grouping_cols
  )

  .check_group_year_presence(harm_groups, group_year_presence, grouping_cols)

  year_bounds <- .find_year_bounds(group_year_presence, data, grouping_cols)
  complex_shares <- .calc_complex_shares(
    data_groups,
    df_simple,
    year_bounds,
    grouping_cols
  )

  .calc_return_harm_int(data, complex_shares, df_simple, grouping_cols)
}

# Identify 1:n groups with full year presence
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
    dplyr::summarise(
      observed_harm = list(unique(item_code_harm)),
      .by = c("item", "year", dplyr::all_of(grouping_names))
    ) |>
    dplyr::left_join(
      harm_groups,
      by = c("item", grouping_names),
      relationship = "many-to-many"
    ) |>
    dplyr::mutate(
      all_present = purrr::map2_lgl(harm_set, observed_harm, ~ all(.x %in% .y))
    ) |>
    dplyr::filter(all_present)
}

# Abort on incomplete 1:n groups
#
# Abort with an informative message if any 1:n group is
# missing data for some years.
.check_group_year_presence <- function(
  harm_groups,
  group_year_presence,
  grouping_cols
) {
  incomplete_groups <- dplyr::anti_join(
    harm_groups |>
      dplyr::distinct(item, !!!grouping_cols),
    group_year_presence |>
      dplyr::distinct(item, !!!grouping_cols),
    by = c(
      "item",
      names(dplyr::select(
        harm_groups |>
          dplyr::distinct(item, !!!grouping_cols),
        !!!grouping_cols
      ))
    )
  )

  if (nrow(incomplete_groups) > 0) {
    formatted_rows <- with(incomplete_groups, paste0(item))
    msg <- c(
      "x" = "ERROR: Incomplete 1:n groups detected",
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
# 1:n rows and compute start and end years per `items` group.
.find_year_bounds <- function(group_year_presence, data, grouping_cols) {
  grouping_names <- .group_names(grouping_cols)

  dplyr::bind_rows(
    group_year_presence |>
      dplyr::select(item, year, dplyr::all_of(grouping_names)),
    data |>
      dplyr::filter(type == "1:n") |>
      dplyr::select(item, year, dplyr::all_of(grouping_names))
  ) |>
    dplyr::summarise(
      start_year = min(year, na.rm = TRUE),
      end_year = max(year, na.rm = TRUE),
      .by = c("item", dplyr::all_of(grouping_names))
    )
}

# Calculate value shares for complex 1:n harmonizations
#
# Expand `data_groups` to all years between `start_year`
# and `end_year`, join simple values and compute value shares per
# year. Missing years are filled using `linear_fill()`.
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
      by = c("item", grouping_names),
      relationship = "many-to-many"
    ) |>
    dplyr::mutate(year = purrr::map2(start_year, end_year, seq)) |>
    tidyr::unnest(year) |>
    dplyr::left_join(
      df_simple,
      by = c("year", "item_code_harm", grouping_names),
      relationship = "many-to-many"
    ) |>
    dplyr::mutate(
      total_value = sum(value, na.rm = TRUE),
      value_share = value / total_value,
      year = as.numeric(year),
      .by = c("item", "year", dplyr::all_of(grouping_names))
    ) |>
    fill_linear(
      value_share,
      year,
      .by = c("item", "item_code_harm", grouping_names)
    ) |>
    dplyr::select(
      item,
      item_code_harm,
      year,
      value_share,
      dplyr::all_of(grouping_names)
    )
}

# Apply computed shares and return harmonized series
#
# Multiply 1:n `value` by `value_share`, bind simple
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
    dplyr::filter(type == "1:n") |>
    dplyr::left_join(
      complex_shares,
      by = c("item", "item_code_harm", "year", grouping_names),
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
    dplyr::summarise(
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
