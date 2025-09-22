# Simple functions to fill gaps (NA values) in a time-dependent variable,
# creating complete time series.
#' Fill gaps by linear interpolation, or carrying forward or backwards.
#'
#' @description
#' Fills gaps (NA values) in a time-dependent variable by
#' linear interpolation between two points, or carrying forward or backwards
#' the last or initial values, respectively. It also creates a new variable
#' indicating the source of the filled values.
#'
#' @param df A tibble data frame containing one observation per row.
#' @param var The variable of df containing gaps to be filled.
#' @param time_index The time index variable (usually year).
#' @param interpolate Logical. If TRUE (default), performs linear interpolation.
#' @param fill_forward Logical. If TRUE (default), carries last value forward.
#' @param fill_backward Logical. If TRUE (default), carries first value backward.
#' @param ... The grouping variables (optional).
#'
#' @return A tibble data frame (ungrouped) where gaps in var have been filled,
#'   and a new "source" variable has been created indicating if the value is
#'   original or, in case it has been estimated, the gapfilling method that has
#'   been used.
#'
#' @export
#'
#' @examples
#' sample_tibble <- tibble::tibble(
#'   category = c("a", "a", "a", "a", "a", "a", "b", "b", "b", "b", "b", "b"),
#'   year = c("2015", "2016", "2017", "2018", "2019", "2020",
#'            "2015", "2016", "2017", "2018", "2019", "2020"),
#'   value = c(NA, 3, NA, NA, 0, NA, 1, NA, NA, NA, 5, NA),
#' )
#' linear_fill(sample_tibble, value, year, category)
#' linear_fill(sample_tibble, value, year, category, interpolate = FALSE)
linear_fill <- function(
  df,
  var,
  time_index,
  ...,
  interpolate = TRUE,
  fill_forward = TRUE,
  fill_backward = TRUE
) {
  df |>
    dplyr::group_by(...) |>
    dplyr::mutate(
      value_interpfilled = if (interpolate) {
        zoo::na.approx({{ var }}, x = {{ time_index }}, na.rm = FALSE)
      } else {
        NA_real_
      },
      value_carried_forward = if (fill_forward) {
        zoo::na.locf0({{ var }})
      } else {
        NA_real_
      },
      value_carried_backward = if (fill_backward) {
        zoo::na.locf0({{ var }}, fromLast = TRUE)
      } else {
        NA_real_
      },
      "source_{{var}}" := dplyr::case_when(
        !is.na({{ var }}) ~ "Original",
        !is.na(value_interpfilled) ~ "Linear interpolation",
        !is.na(value_carried_backward) ~ "First value carried backwards",
        !is.na(value_carried_forward) ~ "Last value carried forward",
        TRUE ~ "Gap not filled"
      ),
      "{{var}}" := dplyr::coalesce(
        {{ var }},
        value_interpfilled,
        value_carried_backward,
        value_carried_forward
      )
    ) |>
    dplyr::select(
      -value_interpfilled,
      -value_carried_forward,
      -value_carried_backward
    ) |>
    dplyr::ungroup()
}

#' Fill gaps using a proxy variable.
#'
#' @description
#' Fills gaps in a variable based on changes in a proxy variable, using ratios between
#' the filled variable and the proxy variable, and labels output accordingly.
#'
#' @param df A tibble data frame containing one observation per row.
#' @param var The variable of df containing gaps to be filled.
#' @param proxy_var The variable to be used as proxy.
#' @param time_index The time index variable (usually year).
#' @param ... The grouping variables (optional).
#'
#' @return A tibble dataframe (ungrouped) where gaps in var have been filled,
#'   a new proxy_ratio variable has been created,
#'   and a new "source" variable has been created indicating if the value is
#'   original or, in case it has been estimated, the gapfilling method that has
#'   been used.
#'
#' @export
#'
#' @examples
#' sample_tibble <- tibble::tibble(
#'   category = c("a", "a", "a", "a", "a", "a", "b", "b", "b", "b", "b", "b"),
#'   year = c("2015", "2016", "2017", "2018", "2019", "2020",
#'            "2015", "2016", "2017", "2018", "2019", "2020"),
#'   value = c(NA, 3, NA, NA, 0, NA, 1, NA, NA, NA, 5, NA),
#'   proxy_variable = c(1,2,2,2,2,2,1,2,3,4,5,6)
#' )
#' proxy_fill(sample_tibble, value, proxy_variable, year, category)
proxy_fill <- function(df, var, proxy_var, time_index, ...) {
  df |>
    dplyr::mutate(proxy_ratio = {{ var }} / {{ proxy_var }}) |>
    linear_fill(proxy_ratio, {{ time_index }}, ...) |>
    dplyr::mutate(
      "source_{{var}}" := dplyr::case_when(
        !is.na({{ var }}) ~ "Original",
        source_proxy_ratio == "Linear interpolation" ~ "Proxy interpolated",
        source_proxy_ratio == "Last value carried forward" ~
          "Proxy carried forward",
        source_proxy_ratio == "First value carried backwards" ~
          "Proxy carried backwards",
        .default = NA_character_
      ),
      "{{var}}" := dplyr::coalesce({{ var }}, proxy_ratio * {{ proxy_var }})
    )
}

#' Fill gaps summing the previous value of a variable to the value of another variable.
#'
#' Fill gaps in a variable with the sum of its previous value and the value
#' of another variable. When a gap has multiple observations, the values are
#' accumulated along the series. When there is a gap at the start of the
#' series, it can either remain unfilled or assume an invisible 0 value before
#' the first observation and start filling with cumulative sum.
#'
#' @param df A tibble data frame containing one observation per row.
#' @param var The variable of df containing gaps to be filled.
#' @param change_var The variable whose values will be used to fill the gaps.
#' @param start_with_zero Logical. If TRUE, assumes an invisible 0 value before
#'   the first observation and fills with cumulative sum starting from the first
#'   change_var value. If FALSE (default), starting NA values remain unfilled.
#' @param ... The grouping variables (optional).
#'
#' @return A tibble dataframe (ungrouped) where gaps in var have been filled,
#' and a new "source" variable has been created indicating if the value is
#' original or, in case it has been estimated, the gapfilling method that has
#' been used.
#'
#' @export
#'
#' @examples
#' sample_tibble <- tibble::tibble(
#'   category = c("a", "a", "a", "a", "a", "a", "b", "b", "b", "b", "b", "b"),
#'   year = c("2015", "2016", "2017", "2018", "2019", "2020",
#'            "2015", "2016", "2017", "2018", "2019", "2020"),
#'   value = c(NA, 3, NA, NA, 0, NA, 1, NA, NA, NA, 5, NA),
#'   change_variable =c(1,2,3,4,1,1,0,0,0,0,0,1)
#' )
#' sum_fill(sample_tibble, value, change_variable, category)
#' sum_fill(sample_tibble, value, change_variable, TRUE, category)
sum_fill <- function(df, var, change_var, start_with_zero = FALSE, ...) {
  df |>
    dplyr::group_by(...) |>
    dplyr::mutate(
      original_value = {{ var }},
      groups = cumsum(!is.na({{ var }})),
      all_na = all(is.na({{ var }})),
      # When start_with_zero = TRUE, assume invisible 0 before first observation
      # so cumsum starts from change_var values, not from 0
      cumsum_start_zero = cumsum({{ change_var }}),
      prefilled = dplyr::coalesce({{ var }}, {{ change_var }}),
      cumsum_regular = ave(prefilled, groups, FUN = cumsum),
      cumsum_value = dplyr::case_when(
        start_with_zero & all_na ~ cumsum_start_zero,
        groups != 0 ~ cumsum_regular,
        start_with_zero ~ cumsum_start_zero,
        .default = NA_real_
      ),
      value = dplyr::coalesce(original_value, cumsum_value),
      source_value = dplyr::case_when(
        !is.na(original_value) ~ "Original",
        !is.na(value) ~ "Filled with sum",
        .default = NA_character_
      )
    ) |>
    dplyr::select(
      -original_value,
      -cumsum_value,
      -all_na,
      -prefilled,
      -cumsum_regular,
      -cumsum_start_zero
    ) |>
    dplyr::ungroup()
}
