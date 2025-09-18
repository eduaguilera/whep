# Simple functions to fill gaps (NA values) in a time-dependent variable, creating complete time series


#' Fill gaps in a variable by linear interpolation between two points, or carrying forward or backwards 
#' the last or initial values, respectively, and label output accordingly
#' 
#' @param df A tibble data frame containing one observation per row
#' @param var The variable of df containing gaps to be filled
#' @param time_index The time index variable (usually year)
#' @param ... The grouping variables (optional)
#' 
#' @return A tibble data frame (ungrouped) where gaps in var have been filled, 
#' and a new "Source" variable has been created indicating if the value is original or,
#' in case it has been estimated, the gapfilling method that has been used
#'
#' @export
#'
#' @examples
#' sample_tibble <- tibble::tibble(
#'   category = c("a", "a", "a", "a", "a", "a", "b", "b", "b", "b", "b", "b"),
#'   year = c("2015", "2016", "2017", "2018", "2019", "2020", "2015", "2016", "2017", "2018", "2019", "2020"),
#'   value = c(NA, 3, NA, NA, 0, NA, 1, NA, NA, NA, 5, NA),
#' )
#'linear_fill(sample_tibble, value, year, category)
linear_fill <- function(df, var, time_index, ...) { # df = data frame; var = variable to be filled; time_index = time index (usually year); ... = grouping variables
  df |> 
    dplyr::group_by(...) |> 
    dplyr::mutate(
      Value_interpfilled = zoo::na.approx({{ var }},
                                          x = zoo::index({{ time_index }}),
                                          na.rm = FALSE
      ),
      Value_CarriedBackward = zoo::na.locf0({{ var }}),
      Value_CarriedForward = zoo::na.locf0({{ var }}, fromLast = TRUE),
      "Source_{{var}}" := ifelse(!is.na({{ var }}),
                                 "Original",
                                 ifelse(!is.na(Value_interpfilled),
                                        "Linear interpolation",
                                        ifelse(!is.na(Value_CarriedBackward),
                                               "Last value carried forward",
                                               "First value carried backwards"
                                        )
                                 )
      ),
      "{{var}}" := ifelse(!is.na({{ var }}),
                          {{ var }},
                          ifelse(!is.na(Value_interpfilled),
                                 Value_interpfilled,
                                 ifelse(!is.na(Value_CarriedBackward),
                                        Value_CarriedBackward,
                                        Value_CarriedForward
                                 )
                          )
      )
    ) |>
    dplyr::select(-Value_interpfilled, -Value_CarriedForward, -Value_CarriedBackward, -Value_CarriedForward) |>
    dplyr::ungroup()
}

#' Fills gaps in a variable based on changes in a proxy variable, using ratios between 
#' the filled variable and the proxy variable, and labels output accordingly
#' 
#' @param df A tibble data frame containing one observation per row
#' @param var The variable of df containing gaps to be filled
#' @param proxy_var The variable to be used as proxy
#' @param time_index The time index variable (usually year)
#' @param ... The grouping variables (optional)
#' 
#' @return A tibble dataframe (ungrouped) where gaps in var have been filled, 
#' a new proxy_ratio variable has been created,
#' and a new "Source" variable has been created indicating if the value is original or,
#' in case it has been estimated, the gapfilling method that has been used
#'
#' @export
#'
#' @examples
#' sample_tibble <- tibble::tibble(
#'   category = c("a", "a", "a", "a", "a", "a", "b", "b", "b", "b", "b", "b"),
#'   year = c("2015", "2016", "2017", "2018", "2019", "2020", "2015", "2016", "2017", "2018", "2019", "2020"),
#'   value = c(NA, 3, NA, NA, 0, NA, 1, NA, NA, NA, 5, NA),
#'   proxy_variable = c(1,2,2,2,2,2,1,2,3,4,5,6)
#' )
#'proxy_fill(sample_tibble, value, proxy_variable, year, category)
proxy_fill <- function(df, var, proxy_var, time_index, ...) {
  df |>
    dplyr::mutate(proxy_ratio = {{ var }} / {{ proxy_var }}) |>
    linear_fill(proxy_ratio, {{ time_index }}, ...) |>
    dplyr::mutate(
      "Source_{{var}}" := ifelse(!is.na({{ var }}),
                                 "Original",
                                 ifelse(Source_proxy_ratio == "Linear interpolation",
                                        "Proxy interpolated",
                                        ifelse(Source_proxy_ratio == "Last value carried forward",
                                               "Proxy carried forward",
                                               ifelse(Source_proxy_ratio == "First value carried backwards",
                                                      "Proxy carried backwards",
                                                      NA
                                               )
                                        )
                                 )
      ),
      "{{var}}" := ifelse(!is.na({{ var }}),
                          {{ var }},
                          proxy_ratio * {{ proxy_var }}
      )
    )
}


#' Fill gaps in a variable with the sum of its previous value and the value of another variable. 
#' When a gap has multiple observations, the values are accumulated along the series.
#' 
#' @param df A tibble data frame containing one observation per row
#' @param var The variable of df containing gaps to be filled
#' @param time_index The time index variable (usually year)
#' @param change_var The variable whose values will be used to fill the gaps
#' @param ... The grouping variables (optional)
#' 
#' @return A tibble dataframe (ungrouped) where gaps in var have been filled, 
#' and a new "Source" variable has been created indicating if the value is original or,
#' in case it has been estimated, the gapfilling method that has been used
#'
#' @export
#'
#' @examples
#' sample_tibble <- tibble::tibble(
#'   category = c("a", "a", "a", "a", "a", "a", "b", "b", "b", "b", "b", "b"),
#'   year = c("2015", "2016", "2017", "2018", "2019", "2020", "2015", "2016", "2017", "2018", "2019", "2020"),
#'   value = c(NA, 3, NA, NA, 0, NA, 1, NA, NA, NA, 5, NA),
#'   change_variable =c(1,1,1,1,1,1,0,0,0,0,0,0)
#' )
#'sum_fill(sample_tibble, value, change_variable, category)
sum_fill <- function(df, var, change_var, ...) {
  var_sym <- rlang::ensym(var)
  change_var_sym <- rlang::ensym(change_var)
  var_name <- rlang::as_string(var_sym)
  change_var_name <- rlang::as_string(change_var_sym)
  
  group_syms <- rlang::ensyms(...)
  
  df <- dplyr::group_by(df, !!!group_syms) |>
    dplyr::group_modify(function(.x, .y) {
      var_vec <- .x[[var_name]]
      change_vec <- .x[[change_var_name]]
      
      status_vec <- ifelse(is.na(var_vec), NA, "Original")
      
      for (i in seq_along(var_vec)) {
        if (is.na(var_vec[i]) && i > 1) {
          var_vec[i] <- var_vec[i - 1] + change_vec[i]
          status_vec[i] <- "Filled with sum"
        }
      }
      
      .x[[var_name]] <- var_vec
      .x[[paste0("Source_",var_name)]] <- status_vec
      return(.x)
    }) |>
    dplyr::ungroup()
  
  return(df)
}