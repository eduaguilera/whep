# Fills gaps by linear interpolation, when possible, or carrying forward or backwards, when not possible, and labels output accordingly
# Remember to group the data when needed before running the function
Linear_filling <- function(data, var, Index) { # data=dataframe, var=variable to be filled, Index=time index (usually year)
  data %>%
    dplyr::mutate(
      Value_interpfilled = zoo::na.approx({{ var }},
                                          x = index({{ Index }}),
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
    ) %>%
    dplyr::select(-Value_interpfilled, -Value_CarriedForward, -Value_CarriedBackward, -Value_CarriedForward)
}