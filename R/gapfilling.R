# Simple functions to fill gaps (NA values) in a time-dependent variable,
# creating complete time series.

# Global variables to avoid R CMD check NOTE
utils::globalVariables(c(
  ".",
  ".N",
  ".fill_scope",
  ".smooth_var",
  "ma_base",
  "lag_source",
  "lag_year",
  "lag_weight",
  "growth_weight",
  "individual_growth"
))

#' Fill gaps by linear interpolation, or carrying forward or backward.
#'
#' @description
#' Fills gaps (`NA` values) in a time-dependent variable by
#' linear interpolation between two points, or carrying forward or backwards
#' the last or initial values, respectively. It also creates a new variable
#' indicating the source of the filled values.
#'
#' @param data A data frame containing one observation per row.
#' @param value_col The column containing gaps to be filled.
#' @param time_col Character. Name of the time column. Default: "year".
#' @param interpolate Logical. If `TRUE` (default),
#'   performs linear interpolation.
#' @param fill_forward Logical. If `TRUE` (default),
#'   carries last value forward.
#' @param fill_backward Logical. If `TRUE` (default),
#'   carries first value backward.
#' @param value_smooth_window An integer specifying the window size for a
#'   centered moving average applied to the variable before gap-filling. Useful
#'   for variables with high inter-annual variability. If `NULL` (default), no
#'   smoothing is applied.
#' @param .by A character vector with the grouping variables (optional).
#'
#' @return A tibble data frame (ungrouped) where gaps in value_col have been
#'   filled, and a new "source" variable has been created indicating if the
#'   value is original or, in case it has been estimated, the gapfilling method
#'   that has been used.
#'
#' @export
#'
#' @examples
#' sample_tibble <- tibble::tibble(
#'   category = c("a", "a", "a", "a", "a", "a", "b", "b", "b", "b", "b", "b"),
#'   year = c(
#'     "2015", "2016", "2017", "2018", "2019", "2020",
#'     "2015", "2016", "2017", "2018", "2019", "2020"
#'   ),
#'   value = c(NA, 3, NA, NA, 0, NA, 1, NA, NA, NA, 5, NA),
#' )
#' fill_linear(sample_tibble, value, .by = c("category"))
#' fill_linear(
#'   sample_tibble,
#'   value,
#'   interpolate = FALSE,
#'   .by = c("category"),
#' )
fill_linear <- function(
  data,
  value_col,
  time_col = "year",
  interpolate = TRUE,

  fill_forward = TRUE,
  fill_backward = TRUE,
  value_smooth_window = NULL,
  .by = NULL
) {
  # Apply smoothing before main mutate to avoid tidy eval issues with if
  use_smoothing <- !is.null(value_smooth_window)

  # Create smoothed variable in separate step
  if (use_smoothing) {
    data <- data |>
      dplyr::mutate(
        .smooth_var = zoo::rollmean(
          {{ value_col }},
          k = value_smooth_window,
          fill = NA,
          align = "center"
        ),
        .by = dplyr::all_of(.by)
      )
  } else {
    data <- data |>
      dplyr::mutate(
        .smooth_var = {{ value_col }},
        .by = dplyr::all_of(.by)
      )
  }

  data |>
    dplyr::mutate(
      # relative to first/last non-NA (use smoothed values for position)
      place = dplyr::case_when(
        !cummax(!is.na(.smooth_var)) ~ "left",
        rev(!cummax(rev(!is.na(.smooth_var)))) ~ "right",
        .default = "middle"
      ),
      fill_value = dplyr::case_when(
        place == "left" & fill_backward ~
          zoo::na.locf0(.smooth_var, fromLast = TRUE),
        place == "right" & fill_forward ~
          zoo::na.locf0(.smooth_var, fromLast = FALSE),
        place == "middle" & interpolate ~
          zoo::na.approx(
            .smooth_var,
            x = .data[[time_col]],
            na.rm = FALSE
          ),
        .default = NA_real_
      ),
      # Use original values where available, fill otherwise
      fill_value = dplyr::coalesce({{ value_col }}, fill_value),
      "source_{{value_col}}" := dplyr::case_when(
        !is.na({{ value_col }}) ~ "Original",
        place == "left" & !is.na(fill_value) ~ "First value carried backwards",
        place == "right" & !is.na(fill_value) ~ "Last value carried forward",
        place == "middle" & !is.na(fill_value) ~ "Linear interpolation",
        TRUE ~ "Gap not filled"
      ),
      "{{value_col}}" := fill_value,
      place = NULL,
      fill_value = NULL,
      .smooth_var = NULL,
      .by = dplyr::all_of(.by)
    )
}

#' Fill gaps summing the previous value of a variable to the value of
#' another variable.
#'
#' @description
#' Fills gaps in a variable with the sum of its previous value and the value
#' of another variable. When a gap has multiple observations, the values are
#' accumulated along the series. When there is a gap at the start of the
#' series, it can either remain unfilled or assume an invisible 0 value before
#' the first observation and start filling with cumulative sum.
#'
#' @param data A data frame containing one observation per row.
#' @param value_col The column containing gaps to be filled.
#' @param change_col The column whose values will be used to fill the gaps.
#' @param time_col Character. Name of the time column. Default: "year".
#' @param start_with_zero Logical. If TRUE, assumes an invisible 0 value before
#'   the first observation and fills with cumulative sum starting from the first
#'   change_col value. If FALSE (default), starting NA values remain unfilled.
#' @param .by A character vector with the grouping variables (optional).
#'
#' @return A tibble dataframe (ungrouped) where gaps in value_col have been
#'   filled, and a new "source" variable has been created indicating if the
#'   value is original or, in case it has been estimated, the gapfilling method
#'   that has been used.
#'
#' @export
#'
#' @importFrom stats ave
#'
#' @examples
#' sample_tibble <- tibble::tibble(
#'   category = c("a", "a", "a", "a", "a", "a", "b", "b", "b", "b", "b", "b"),
#'   year = c(
#'     "2015", "2016", "2017", "2018", "2019", "2020",
#'     "2015", "2016", "2017", "2018", "2019", "2020"
#'   ),
#'   value = c(NA, 3, NA, NA, 0, NA, 1, NA, NA, NA, 5, NA),
#'   change_variable = c(1, 2, 3, 4, 1, 1, 0, 0, 0, 0, 0, 1)
#' )
#' fill_sum(
#'   sample_tibble,
#'   value,
#'   change_variable,
#'   start_with_zero = FALSE,
#'   .by = c("category")
#' )
#' fill_sum(
#'   sample_tibble,
#'   value,
#'   change_variable,
#'   start_with_zero = TRUE,
#'   .by = c("category")
#' )
fill_sum <- function(
  data,
  value_col,
  change_col,
  time_col = "year",
  start_with_zero = TRUE,
  .by = NULL
) {
  data |>
    dplyr::arrange(dplyr::across(dplyr::all_of(c(.by, time_col)))) |>
    dplyr::mutate(
      groups = cumsum(!is.na({{ value_col }})),
      prefilled = dplyr::coalesce({{ value_col }}, {{ change_col }}),
      source_value = ifelse(
        is.na({{ value_col }}),
        "Filled with sum",
        "Original"
      ),
      "{{ value_col }}" := ave(prefilled, groups, FUN = cumsum),
      "{{ value_col }}" := if (start_with_zero) {{ value_col }} else {
        ifelse(groups == 0, NA, {{ value_col }})
      },
      source_value = ifelse(
        is.na({{ value_col }}),
        NA_character_,
        source_value
      ),
      groups = NULL,
      prefilled = NULL,
      .by = dplyr::all_of(.by)
    )
}

#' Fill gaps using growth rates from proxy variables
#'
#' @description
#'   Fills missing values using growth rates from a proxy variable (reference
#'   series). Supports regional aggregations, weighting, and linear
#'   interpolation for small gaps.
#'
#' @param data A data frame containing time series data.
#' @param value_col Character. Name of the column with values to fill.
#' @param proxy_col Character or vector. Proxy variable(s) for calculating
#'   growth rates. Supports multiple syntax formats:
#'   - **Simple numeric proxy** (e.g., `"population"`): Auto-detects numeric
#'     columns and uses them as proxy variable. Inherits the `group_by`
#'     parameter to compute proxy values per group.
#'   - **Simple categorical proxy** (e.g., `"region"`): Auto-detects
#'     categorical columns and interprets as `value_col:region`. Aggregates
#'     `value_col` by the specified groups.
#'   - **Advanced syntax** (e.g., `"gdp:region"`): Format is
#'     `"variable:group1+group2"`. Aggregates variable by specified groups.
#'   - **Hierarchical fallback** (e.g., `c("population", "gdp:region")`):
#'     Tries first proxy, falls back to second if first fails.
#'   - **Weighted aggregation** (e.g., `"gdp[population]"`): Weight variable
#'     by specified column during aggregation.
#' @param time_col Character. Name of the time column. Default: "year".
#' @param group_by Character vector of grouping column names. Default: NULL.
#' @param max_gap Numeric. Maximum gap size to fill using growth method.
#'   Default: Inf.
#' @param max_gap_linear Numeric. Maximum gap size for linear interpolation
#'   fallback. Default: 3.
#' @param fill_scope Quosure. Filter expression to limit filling scope.
#'   Default: NULL.
#' @param smooth_window Integer. Window size for moving average smoothing of
#'   proxy reference values before computing growth rates. Default: NULL.
#' @param output_format Character. Output format: "clean" or "detailed".
#'   Default: "clean".
#' @param verbose Logical. Print progress messages. Default: TRUE.
#'
#' @return
#'   A data frame with filled values. If output_format = "clean", returns
#'   original columns with updated value_col and added method_col. If
#'   "detailed", includes all intermediate columns.
#'
#' @details
#'   **Combined Growth Sequence (Hierarchical Interpolation):**
#'
#'   When using multiple proxies with hierarchical fallback, the function
#'   implements an intelligent combined growth sequence strategy:
#'
#'   1. Better proxies (earlier in hierarchy) are tried first for each gap.
#'   2. If a better proxy has partial coverage within a gap, those growth
#'      rates are used for the covered positions.
#'   3. Fallback proxies fill only the remaining positions where better
#'      proxies are not available.
#'   4. Values filled by better proxies are protected from being overwritten.
#'
#' @export
#'
#' @examples
#' # Fill GDP using population as proxy
#' data <- tibble::tibble(
#'   country = rep("ESP", 4),
#'   year = 2010:2013,
#'   gdp = c(1000, NA, NA, 1200),
#'   population = c(46, 46.5, 47, 47.5)
#' )
#' filled <- fill_growth(
#'   data,
#'   value_col = "gdp",
#'   proxy_col = "population",
#'   group_by = "country"
#' )
#'
#' @seealso [fill_linear()], [fill_sum()]
fill_growth <- function(
  data,
  value_col,
  proxy_col,
  time_col = "year",
  group_by = NULL,
  max_gap = Inf,
  max_gap_linear = 3,
  fill_scope = NULL,
  smooth_window = NULL,
  output_format = "clean",
  verbose = TRUE
) {
  # Validate inputs

  if (!time_col %in% names(data)) {
    stop("Time column '", time_col, "' not found in data")
  }

  if (!value_col %in% names(data)) {
    stop("Value column '", value_col, "' not found in data")
  }

  if (!is.null(smooth_window)) {
    if (
      !is.numeric(smooth_window) ||
        length(smooth_window) != 1 ||
        smooth_window < 1
    ) {
      stop("`smooth_window` must be NULL or a positive integer")
    }
    smooth_window <- as.integer(smooth_window)
  } else {
    smooth_window <- 1L
  }

  fill_scope_expr <- rlang::enquo(fill_scope)

  scope_mask <- rep(TRUE, nrow(data))
  if (!rlang::quo_is_null(fill_scope_expr)) {
    scope_mask <- data |>
      dplyr::mutate(.fill_scope = !!fill_scope_expr) |>
      dplyr::pull(.fill_scope)
    if (!is.logical(scope_mask) || length(scope_mask) != nrow(data)) {
      stop(
        "`fill_scope` must evaluate to a logical vector ",
        "with length equal to nrow(data)"
      )
    }
    scope_mask[is.na(scope_mask)] <- FALSE
  }

  if (!is.null(group_by)) {
    missing <- setdiff(group_by, names(data))
    if (length(missing) > 0) {
      stop("Group columns not found: ", paste(missing, collapse = ", "))
    }
  }

  max_gap_linear <- max(0L, as.integer(max_gap_linear))

  # Helper: Compute moving average base for growth rate calculation
  compute_ma_base <- function(dt, value_var, window, group_vars = NULL) {
    if (window <= 1) {
      return(dt)
    }

    compute_ma <- function(vals, w) {
      ma <- rep(NA_real_, length(vals))
      for (i in seq_along(vals)) {
        if (i > 1) {
          start_idx <- max(1, i - w)
          window_vals <- vals[start_idx:(i - 1)]
          window_vals <- window_vals[!is.na(window_vals)]
          if (length(window_vals) > 0) {
            ma[i] <- mean(
              window_vals[
                max(1, length(window_vals) - w + 1):length(window_vals)
              ]
            )
          }
        }
      }
      ma
    }

    if (length(group_vars) > 0) {
      dt[, ma_base := compute_ma(get(value_var), window), by = group_vars]
    } else {
      dt[, ma_base := compute_ma(get(value_var), window)]
    }
    dt
  }

  # Create working copy
  function_tag <- "proxy"
  raw_original_col <- paste0(value_col, "_raw_original")
  if (!raw_original_col %in% names(data)) {
    data[[raw_original_col]] <- suppressWarnings(as.numeric(data[[value_col]]))
  }

  snapshot_base <- paste0(value_col, "_raw_", function_tag)
  snapshot_col <- snapshot_base
  snapshot_index <- 1L
  while (snapshot_col %in% names(data)) {
    snapshot_index <- snapshot_index + 1L
    snapshot_col <- paste0(snapshot_base, "_", snapshot_index)
  }
  data[[snapshot_col]] <- suppressWarnings(as.numeric(data[[value_col]]))

  raw_col <- snapshot_col
  raw_numeric_col <- paste0(raw_col, "_numeric")
  raw_missing_col <- paste0(raw_col, "_missing")
  method_col <- paste0("method_", value_col)
  has_method_col <- method_col %in% names(data)

  original_value_numeric <- suppressWarnings(as.numeric(data[[value_col]]))
  original_value_numeric[!is.finite(original_value_numeric)] <- NA_real_
  original_method <- if (has_method_col) {
    as.character(data[[method_col]])
  } else {
    ifelse(is.na(original_value_numeric), "missing", "original")
  }

  data_work <- data |>
    dplyr::mutate(
      !!raw_col := .data[[value_col]],
      !!raw_numeric_col := suppressWarnings(as.numeric(.data[[value_col]]))
    ) |>
    dplyr::mutate(
      !!raw_numeric_col := replace(
        .data[[raw_numeric_col]],
        !is.finite(.data[[raw_numeric_col]]),
        NA_real_
      ),
      !!raw_missing_col := is.na(.data[[raw_numeric_col]]),
      !!method_col := if (has_method_col) {
        .data[[method_col]]
      } else {
        ifelse(.data[[raw_missing_col]], "missing", "original")
      },
      !!value_col := .data[[raw_numeric_col]]
    )

  data_work[[method_col]][is.na(data_work[[value_col]])] <- "missing"

  if (verbose) {
    n_missing <- sum(is.na(data_work[[value_col]]))
    n_total <- nrow(data_work)
    message(
      "Starting with ",
      n_missing,
      " missing values out of ",
      n_total,
      " total observations"
    )
  }

  # Parse proxy specification
  parse_proxy_spec <- function(spec) {
    if (!grepl(":", spec)) {
      weight_col <- NULL
      col_name <- spec
      if (grepl("\\[", spec)) {
        start_pos <- regexpr("\\[", spec)[1]
        if (start_pos > 0) {
          after_bracket <- substr(spec, start_pos + 1, nchar(spec))
          end_pos <- regexpr("\\]", after_bracket)[1]
          if (end_pos > 0) {
            weight_col <- trimws(substr(after_bracket, 1, end_pos - 1))
          }
        }
        col_name <- sub("\\[.*$", "", spec)
      }

      col_name <- trimws(col_name)

      if (col_name %in% c("global", "all", "total")) {
        if (verbose) {
          message("Using global aggregation (no grouping)")
        }
        return(list(
          source_var = value_col,
          group_vars = NULL,
          weight_col = weight_col,
          spec_name = paste0("global", if (!is.null(weight_col)) "_w" else "")
        ))
      }

      if (col_name %in% names(data)) {
        col_data <- data[[col_name]]
        is_numeric <- is.numeric(col_data) ||
          all(!is.na(suppressWarnings(as.numeric(as.character(col_data)))))

        if (is_numeric) {
          if (verbose) {
            message(
              "Auto-detected '",
              col_name,
              "' as numeric -> ",
              "using as source variable"
            )
          }
          return(list(
            source_var = col_name,
            group_vars = group_by,
            weight_col = weight_col,
            spec_name = paste0(
              col_name,
              if (!is.null(weight_col)) "_w" else ""
            )
          ))
        } else {
          if (verbose) {
            message(
              "Auto-detected '",
              col_name,
              "' as categorical -> ",
              "using as grouping variable"
            )
          }
          return(list(
            source_var = value_col,
            group_vars = col_name,
            weight_col = weight_col,
            spec_name = paste0(
              value_col,
              "_",
              col_name,
              if (!is.null(weight_col)) "_w" else ""
            )
          ))
        }
      } else {
        warning("Column '", col_name, "' not found in data")
        return(list(
          source_var = value_col,
          group_vars = col_name,
          weight_col = weight_col,
          spec_name = paste0(
            value_col,
            "_",
            col_name,
            if (!is.null(weight_col)) "_w" else ""
          )
        ))
      }
    }

    parts <- strsplit(spec, ":")[[1]]
    source_var <- parts[1]
    rhs <- if (length(parts) > 1) parts[2] else ""

    if (is.na(rhs)) {
      rhs <- ""
    }

    weight_col <- NULL
    if (nchar(rhs) > 0 && grepl("\\[", rhs)) {
      start_pos <- regexpr("\\[", rhs)[1]
      if (start_pos > 0) {
        after_bracket <- substr(rhs, start_pos + 1, nchar(rhs))
        end_pos <- regexpr("\\]", after_bracket)[1]
        if (end_pos > 0) {
          weight_col <- trimws(substr(after_bracket, 1, end_pos - 1))
        }
      }
      rhs <- sub("\\[.*$", "", rhs)
    }

    group_vars <- if (rhs == "") NULL else trimws(strsplit(rhs, "\\+")[[1]])

    if (is.null(group_vars) || length(group_vars) == 0) {
      spec_name <- paste0(source_var, "_global")
    } else {
      spec_name <- paste0(source_var, "_", paste(group_vars, collapse = "_"))
    }
    if (!is.null(weight_col)) {
      spec_name <- paste0(spec_name, "_w")
    }

    list(
      source_var = source_var,
      group_vars = group_vars,
      weight_col = weight_col,
      spec_name = spec_name
    )
  }

  # Calculate growth rates for a specification
  calculate_growth_for_spec <- function(
    data,
    spec_info,
    growth_col_name,
    obs_col_name
  ) {
    source_var <- spec_info$source_var
    group_vars <- spec_info$group_vars
    weight_col <- spec_info$weight_col
    has_weight <- !is.null(weight_col) && weight_col %in% names(data)

    if (!is.null(weight_col) && !has_weight && verbose) {
      message(
        "Weight column '",
        weight_col,
        "' not found; falling back to unweighted means"
      )
    }

    if (verbose) {
      message("Calculating growth rates for: ", spec_info$spec_name)
    }

    if (!source_var %in% names(data)) {
      warning("Source variable '", source_var, "' not found")
      data[[growth_col_name]] <- NA_real_
      data[[obs_col_name]] <- 0L
      return(data)
    }

    present_group_vars <- group_vars[group_vars %in% names(data)]
    missing_group_vars <- setdiff(group_vars, present_group_vars)
    if (length(missing_group_vars) > 0 && verbose) {
      message(
        "Ignoring missing grouping columns for growth: ",
        paste(missing_group_vars, collapse = ", ")
      )
    }

    lag_group_vars <- if (!is.null(group_by)) {
      unique(c(group_by, present_group_vars))
    } else {
      present_group_vars
    }
    lag_group_vars <- lag_group_vars[lag_group_vars %in% names(data)]

    subset_cols <- unique(c("year", lag_group_vars, source_var))
    if (has_weight) {
      subset_cols <- unique(c(subset_cols, weight_col))
    }
    dt_growth <- data.table::as.data.table(data[, subset_cols, drop = FALSE])
    order_cols <- unique(c(lag_group_vars, "year"))
    data.table::setorderv(dt_growth, order_cols)

    if (smooth_window > 1) {
      dt_growth <- compute_ma_base(
        dt_growth,
        source_var,
        smooth_window,
        lag_group_vars
      )
      if (length(lag_group_vars) > 0) {
        dt_growth[,
          lag_source := data.table::shift(ma_base),
          by = lag_group_vars
        ]
        dt_growth[, lag_year := data.table::shift(year), by = lag_group_vars]
      } else {
        dt_growth[, lag_source := data.table::shift(ma_base)]
        dt_growth[, lag_year := data.table::shift(year)]
      }
    } else {
      if (length(lag_group_vars) > 0) {
        dt_growth[,
          lag_source := data.table::shift(get(source_var)),
          by = lag_group_vars
        ]
        dt_growth[, lag_year := data.table::shift(year), by = lag_group_vars]
      } else {
        dt_growth[, lag_source := data.table::shift(get(source_var))]
        dt_growth[, lag_year := data.table::shift(year)]
      }
    }

    if (has_weight) {
      if (length(lag_group_vars) > 0) {
        dt_growth[,
          lag_weight := data.table::shift(get(weight_col)),
          by = lag_group_vars
        ]
      } else {
        dt_growth[, lag_weight := data.table::shift(get(weight_col))]
      }
      dt_growth[, growth_weight := suppressWarnings(as.numeric(lag_weight))]
      dt_growth[
        !is.finite(growth_weight) | growth_weight <= 0,
        growth_weight := NA_real_
      ]
    } else {
      dt_growth[, growth_weight := NA_real_]
    }

    if (length(lag_group_vars) > 0) {
      dt_growth[,
        individual_growth := data.table::fifelse(
          year == lag_year + 1 &
            !is.na(get(source_var)) &
            !is.na(lag_source) &
            lag_source > 0,
          (get(source_var) - lag_source) / lag_source,
          NA_real_
        ),
        by = lag_group_vars
      ]
    } else {
      dt_growth[,
        individual_growth := data.table::fifelse(
          year == lag_year + 1 &
            !is.na(get(source_var)) &
            !is.na(lag_source) &
            lag_source > 0,
          (get(source_var) - lag_source) / lag_source,
          NA_real_
        )
      ]
    }

    dt_growth <- dt_growth[!is.na(individual_growth)]

    if (nrow(dt_growth) == 0) {
      data[[growth_col_name]] <- NA_real_
      data[[obs_col_name]] <- 0L
      return(data)
    }

    group_summary_cols <- if (length(present_group_vars) == 0) {
      "year"
    } else {
      c("year", present_group_vars)
    }

    summary_dt <- dt_growth[,
      {
        g <- individual_growth
        if (has_weight) {
          w <- growth_weight
          valid <- !is.na(w) & is.finite(w) & w > 0
          if (any(valid)) {
            denom <- sum(w[valid], na.rm = TRUE)
            if (!is.na(denom) && denom > 0) {
              growth_val <- sum(g[valid] * w[valid], na.rm = TRUE) / denom
              obs_val <- sum(valid)
            } else {
              growth_val <- mean(g, na.rm = TRUE)
              obs_val <- .N
            }
          } else {
            growth_val <- mean(g, na.rm = TRUE)
            obs_val <- .N
          }
        } else {
          growth_val <- mean(g, na.rm = TRUE)
          obs_val <- .N
        }
        .(.growth_value = growth_val, .obs_value = obs_val)
      },
      by = group_summary_cols
    ]

    data.table::setnames(
      summary_dt,
      c(".growth_value", ".obs_value"),
      c(growth_col_name, obs_col_name)
    )
    summary_tbl <- tibble::as_tibble(summary_dt)

    if (length(present_group_vars) == 0) {
      data <- dplyr::left_join(data, summary_tbl, by = "year")
    } else {
      join_cols <- c("year", present_group_vars)
      data <- dplyr::left_join(data, summary_tbl, by = join_cols)
    }

    data
  }

  # Step 1: Calculate growth rates
  if (verbose) {
    message("Step 1: Calculating growth rate columns...")
  }

  for (i in seq_along(proxy_col)) {
    spec_info <- parse_proxy_spec(proxy_col[i])
    growth_col_name <- paste0("growth_", i, "_", spec_info$spec_name)
    obs_col_name <- paste0("n_obs_", i, "_", spec_info$spec_name)
    data_work <- calculate_growth_for_spec(
      data_work,
      spec_info,
      growth_col_name,
      obs_col_name
    )
  }

  # Step 2: Apply filling
  if (verbose) {
    message("Step 2: Applying hierarchical filling...")
  }

  fill_with_growth <- function(
    df,
    growth_col_name,
    method_name,
    current_proxy_level = 1,
    all_proxy_specs = NULL
  ) {
    df <- df[order(df[[time_col]]), , drop = FALSE]
    values <- df[[value_col]]
    methods <- df[[method_col]]
    growth <- df[[growth_col_name]]
    years <- df[[time_col]]

    original_na_runs <- list()
    if (raw_missing_col %in% names(df)) {
      original_missing <- df[[raw_missing_col]]
      original_missing[is.na(original_missing)] <- TRUE

      rle_result <- rle(original_missing)
      run_ends <- cumsum(rle_result$lengths)
      run_starts <- c(1, run_ends[-length(run_ends)] + 1)

      for (i in seq_along(rle_result$values)) {
        if (rle_result$values[i]) {
          start_i <- run_starts[i]
          end_i <- run_ends[i]
          internal_i <- FALSE
          if (start_i > 1 && end_i < length(original_missing)) {
            left_ok <- !isTRUE(original_missing[start_i - 1])
            right_ok <- !isTRUE(original_missing[end_i + 1])
            internal_i <- left_ok && right_ok
          }
          original_na_runs[[length(original_na_runs) + 1]] <- list(
            start = start_i,
            end = end_i,
            length = rle_result$lengths[i],
            internal = internal_i
          )
        }
      }
    }

    exceeds_max_gap <- function(idx) {
      if (is.null(max_gap) || !is.finite(max_gap)) {
        return(FALSE)
      }
      for (run in original_na_runs) {
        if (idx >= run$start && idx <= run$end && run$length > max_gap) {
          return(TRUE)
        }
      }
      FALSE
    }

    exceeds_max_gap_linear <- function(idx) {
      if (is.null(max_gap_linear) || !is.finite(max_gap_linear)) {
        return(FALSE)
      }
      for (run in original_na_runs) {
        if (
          isTRUE(run$internal) &&
            idx >= run$start &&
            idx <= run$end &&
            run$length > max_gap_linear
        ) {
          return(TRUE)
        }
      }
      FALSE
    }

    # Forward fill
    for (idx in seq_along(values)) {
      if (!is.na(values[idx])) {
        next
      }
      if (idx == 1) {
        next
      }
      if (is.na(growth[idx])) {
        next
      }
      if (exceeds_max_gap(idx)) {
        next
      }
      if (exceeds_max_gap_linear(idx)) {
        next
      }

      prev_indices <- which(!is.na(values[seq_len(idx - 1)]))
      if (length(prev_indices) == 0) {
        next
      }
      prev_idx <- prev_indices[length(prev_indices)]

      if (is.na(values[prev_idx]) || values[prev_idx] <= 0) {
        next
      }

      candidate <- values[prev_idx] * (1 + growth[idx])
      if (is.na(candidate) || !is.finite(candidate)) {
        next
      }

      values[idx] <- candidate
      if (methods[idx] == "missing") {
        methods[idx] <- method_name
      }
    }

    method_name_back <- paste0(method_name, "_backfill")

    # Backward fill
    for (idx in seq(length(values) - 1L, 1L)) {
      if (!is.na(values[idx])) {
        next
      }
      if (idx == length(values)) {
        next
      }
      if (exceeds_max_gap(idx)) {
        next
      }
      if (exceeds_max_gap_linear(idx)) {
        next
      }

      next_idx <- idx + 1L
      if (is.na(values[next_idx])) {
        next
      }

      growth_val <- growth[next_idx]
      denom <- 1 + growth_val
      if (is.na(denom) || abs(denom) < .Machine$double.eps) {
        next
      }

      candidate <- values[next_idx] / denom
      if (is.na(candidate) || !is.finite(candidate) || candidate <= 0) {
        next
      }

      values[idx] <- candidate
      if (methods[idx] == "missing") {
        methods[idx] <- method_name_back
      }
    }

    df[[value_col]] <- values
    df[[method_col]] <- methods

    # Bridge filling for larger gaps
    if (raw_missing_col %in% names(df) && length(values) > 1) {
      original_missing_flags <- df[[raw_missing_col]]
      original_missing_flags[is.na(original_missing_flags)] <- TRUE
      anchor_idx <- which(!original_missing_flags & !is.na(values))

      if (length(anchor_idx) >= 2) {
        for (anchor_pos in seq_len(length(anchor_idx) - 1L)) {
          start_idx <- anchor_idx[anchor_pos]
          end_idx <- anchor_idx[anchor_pos + 1L]
          if (end_idx - start_idx <= 1L) {
            next
          }

          gap_range <- (start_idx + 1L):(end_idx - 1L)
          gap_missing <- gap_range[original_missing_flags[gap_range]]
          if (length(gap_missing) == 0) {
            next
          }

          num_consecutive_nas <- length(gap_missing)
          if (
            !is.null(max_gap) &&
              is.finite(max_gap) &&
              num_consecutive_nas > max_gap
          ) {
            next
          }

          # Linear interpolation for small gaps
          if (num_consecutive_nas <= max_gap_linear) {
            start_val <- values[start_idx]
            end_val <- values[end_idx]
            if (
              !is.finite(start_val) ||
                !is.finite(end_val) ||
                start_val <= 0 ||
                end_val <= 0
            ) {
              next
            }

            start_year <- years[start_idx]
            end_year <- years[end_idx]
            approx_vals <- stats::approx(
              x = c(start_year, end_year),
              y = c(start_val, end_val),
              xout = years[gap_missing]
            )$y
            if (any(!is.finite(approx_vals))) {
              next
            }

            values[gap_missing] <- approx_vals
            methods[gap_missing] <- "linear_interp"

            df[[value_col]] <- values
            df[[method_col]] <- methods
            next
          }

          # Bridge with lambda adjustment for larger gaps
          growth_seq_indices <- (start_idx + 1L):end_idx

          if (!is.null(all_proxy_specs) && length(all_proxy_specs) >= 1) {
            combined_growth <- rep(NA_real_, length(growth_seq_indices))
            combined_source <- rep(NA_character_, length(growth_seq_indices))
            for (lvl in seq_len(current_proxy_level)) {
              spec_name_lvl <- all_proxy_specs[[lvl]]
              growth_col_lvl <- paste0("growth_", lvl, "_", spec_name_lvl)
              if (growth_col_lvl %in% names(df)) {
                vals_lvl <- df[[growth_col_lvl]][growth_seq_indices]
                take <- is.na(combined_growth) & is.finite(vals_lvl)
                if (any(take)) {
                  combined_growth[take] <- vals_lvl[take]
                  combined_source[take] <- spec_name_lvl
                }
              }
            }
          } else {
            combined_growth <- growth[growth_seq_indices]
            combined_source <- rep(
              spec_info$spec_name,
              length(growth_seq_indices)
            )
          }

          if (any(is.na(combined_growth)) || any(!is.finite(combined_growth))) {
            next
          }

          predicted_end <- values[start_idx]
          if (!is.finite(predicted_end) || predicted_end <= 0) {
            next
          }
          valid_sequence <- TRUE
          for (g_val in combined_growth) {
            factor_val <- 1 + g_val
            if (!is.finite(factor_val) || factor_val <= 0) {
              valid_sequence <- FALSE
              break
            }
            predicted_end <- predicted_end * factor_val
          }
          if (
            !valid_sequence ||
              !is.finite(predicted_end) ||
              predicted_end <= 0
          ) {
            next
          }

          actual_end <- values[end_idx]
          if (is.na(actual_end) || !is.finite(actual_end) || actual_end <= 0) {
            next
          }

          ratio <- actual_end / predicted_end
          if (!is.finite(ratio) || ratio <= 0) {
            next
          }

          steps <- length(combined_growth)
          lambda <- ratio^(1 / steps)
          if (!is.finite(lambda) || lambda <= 0) {
            next
          }

          running <- values[start_idx]
          for (step_idx in seq_len(steps)) {
            target_idx <- start_idx + step_idx
            running <- running * ((1 + combined_growth[step_idx]) * lambda)
            if (
              target_idx < end_idx &&
                original_missing_flags[target_idx] &&
                is.na(values[target_idx])
            ) {
              values[target_idx] <- running
              src <- combined_source[step_idx]
              if (is.na(src) || !nzchar(src)) {
                src <- spec_info$spec_name
              }
              methods[target_idx] <- paste0("growth_", src, "_bridge")
            }
          }

          df[[value_col]] <- values
          df[[method_col]] <- methods
        }
      }
    }
    df
  }

  all_proxy_spec_names <- vector("list", length(proxy_col))
  for (j in seq_along(proxy_col)) {
    spec <- parse_proxy_spec(proxy_col[j])
    all_proxy_spec_names[[j]] <- spec$spec_name
  }

  for (i in seq_along(proxy_col)) {
    spec_info <- parse_proxy_spec(proxy_col[i])
    growth_col_name <- paste0("growth_", i, "_", spec_info$spec_name)
    method_name <- paste0("growth_", spec_info$spec_name)

    if (verbose) {
      message("Applying proxy level ", i, ": ", proxy_col[i])
    }

    missing_before <- sum(is.na(data_work[[value_col]]))

    if (!is.null(group_by) && length(group_by) > 0) {
      data_work <- data_work |>
        dplyr::group_by(dplyr::across(dplyr::all_of(group_by))) |>
        dplyr::group_modify(
          ~ fill_with_growth(
            .x,
            growth_col_name,
            method_name,
            current_proxy_level = i,
            all_proxy_specs = all_proxy_spec_names
          )
        ) |>
        dplyr::ungroup()
    } else {
      data_work <- fill_with_growth(
        data_work,
        growth_col_name,
        method_name,
        current_proxy_level = i,
        all_proxy_specs = all_proxy_spec_names
      )
    }

    missing_after <- sum(is.na(data_work[[value_col]]))
    filled_this_round <- missing_before - missing_after

    if (verbose) {
      message(
        "   Filled ",
        filled_this_round,
        " values (",
        missing_after,
        " remaining)"
      )
    }

    if (missing_after == 0) {
      if (verbose) {
        message("All missing values resolved; stopping proxy traversal")
      }
      break
    }
  }

  # Finalize
  data_work[[value_col]] <- suppressWarnings(as.numeric(data_work[[value_col]]))
  data_work[[value_col]][!is.finite(data_work[[value_col]])] <- NA_real_
  data_work[[method_col]][is.na(data_work[[value_col]])] <- "missing"

  if (!all(scope_mask)) {
    data_work[[value_col]][!scope_mask] <- original_value_numeric[!scope_mask]
    data_work[[method_col]][!scope_mask] <- original_method[!scope_mask]
  }

  if (identical(output_format, "clean")) {
    growth_cols <- names(data_work)[grepl("^growth_", names(data_work))]
    obs_cols <- names(data_work)[grepl("^n_obs_", names(data_work))]
    raw_cols <- names(data_work)[grepl("_raw_", names(data_work))]
    dropable <- c(
      growth_cols,
      obs_cols,
      raw_cols,
      raw_numeric_col,
      raw_missing_col
    )
    data_work <- data_work |> dplyr::select(-dplyr::any_of(dropable))
  } else {
    data_work[[raw_col]] <- data_work[[value_col]]
    if (!all(scope_mask)) {
      data_work[[raw_col]][!scope_mask] <- original_value_numeric[!scope_mask]
    }
    data_work <- data_work |>
      dplyr::select(-dplyr::any_of(c(raw_numeric_col, raw_missing_col)))
  }

  if (verbose) {
    n_original_missing <- sum(is.na(data[[value_col]]))
    n_final_missing <- sum(is.na(data_work[[value_col]]))
    n_filled_total <- n_original_missing - n_final_missing
    message(
      "Total filled: ",
      n_filled_total,
      " out of ",
      n_original_missing,
      " missing values"
    )
  }

  data_work
}
