#' Fill the gaps (NA values) in a time-dependent variable, creating complete time series
#'
#' @param trade_sources A tibble dataframe
#' where each row contains the year range
#'
#' @return A tibble dataframe where each row
#' corresponds to a single year for a given source
#'
#' @export
#'
#' @examples

sample_tibble <- tibble::tibble(
  category = c("a", "a", "a", "a", "a", "a", "b", "b", "b", "b", "b", "b"),
  year = c("2015", "2016", "2017", "2018", "2019", "2020", "2015", "2016", "2017", "2018", "2019", "2020"),
  value = c(NA, 3, NA, NA, 0, NA, 1, NA, NA, NA, 5, NA),
  proxy_variable = c(1,2,2,2,2,2,1,2,3,4,5,6),
  change_variable =c(1,1,1,1,1,1,0,0,0,0,0,0)
)

filled_tibble <- linear_fill(sample_tibble, value, year, category)

# Fills gaps by linear interpolation, when possible, or carrying forward or backwards, when not possible, and labels output accordingly
# The data should be he data when needed before running the function
linear_fill <- function(data, var, time_index, ...) { # data= data frame; var = variable to be filled; time_index = time index (usually year); ... = grouping variables
  data |> 
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

# Fills gaps by using changes in a proxy variable, using ratios between filled variable and proxy variable, and labels output accordingly
# Remember to group the data when needed before running the function
proxy_fill <- function(data, var, proxyvar, time_index, ...) { # data=dataframe, var=variable to be filled, proxyvar=variable used as proxy, Index=time index (usually year)
  data %>%
    dplyr::mutate(proxy_ratio = {{ var }} / {{ proxyvar }}) %>%
    linear_fill(proxy_ratio, time_index) %>%
    dplyr::mutate(
      "Source_{{var}}" := ifelse(!is.na({{ var }}),
                                 "Original",
                                 ifelse(Source_Proxy_ratio == "Linear interpolation",
                                        "Proxy interpolated",
                                        ifelse(Source_Proxy_ratio == "Last value carried forward",
                                               "Proxy carried forward",
                                               ifelse(Source_Proxy_ratio == "First value carried backwards",
                                                      "Proxy carried backwards",
                                                      NA
                                               )
                                        )
                                 )
      ),
      "{{var}}" := ifelse(!is.na({{ var }}),
                          {{ var }},
                          proxy_ratio * {{ proxyvar }}
      )
    )
}

# Function to fill gaps in a given variable (var) of a time series with the sum of the previous value of var and the value of another column (change_var). The values of var are accumulated along the series.
sum_fill <- function(var, change_var) {
  for (i in seq_along(var)) {
    if (is.na(var[i]) && i > 1) {
      var1[i] <- var1[i - 1] + change_var[i]
    }
  }
  return(var)
}