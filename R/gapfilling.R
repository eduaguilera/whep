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
  "individual_growth",
  "ind_growth",
  "lag_src",
  "lag_yr",
  "w",
  "g_val",
  "o_val"
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
#'
#' fill_growth(
#'   data,
#'   value_col = "gdp",
#'   proxy_col = "population",
#'   group_by = "country"
#' )
#'
#' @importFrom data.table :=
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
  smooth_window = 1,
  output_format = "clean",
  verbose = TRUE
) {
  # 1. Setup and Validation
  setup <- .fg_setup(
    data,
    value_col,
    time_col,
    group_by,
    fill_scope,
    smooth_window,
    max_gap_linear
  )

  # 2. Calculate Growth Rates
  data_work <- .fg_compute_all_growth_rates(
    setup$data_work,
    proxy_col,
    value_col,
    group_by,
    setup$inputs$smooth_window,
    verbose
  )

  # 3. Apply Filling (Hierarchical)
  data_work <- .fg_apply_hierarchical_filling(
    data_work,
    proxy_col,
    value_col,
    setup$cols,
    max_gap,
    setup$inputs$max_gap_linear,
    time_col,
    group_by,
    verbose
  )

  # 4. Finalize
  .fg_finalize_and_report(
    data_work,
    data,
    value_col,
    setup$cols,
    setup$scope_mask,
    output_format,
    verbose
  )
}

# --- 1. Setup & Validation Helpers ---

.fg_setup <- function(
  data,
  value_col,
  time_col,
  group_by,
  fill_scope,
  smooth_window,
  max_gap_linear
) {
  inputs <- .fg_validate_inputs(
    data,
    value_col,
    time_col,
    group_by,
    smooth_window,
    max_gap_linear
  )

  scope_mask <- .fg_calculate_scope_mask(inputs$data, fill_scope)
  prep <- .fg_prepare_working_columns(inputs$data, value_col)

  list(
    inputs = inputs,
    scope_mask = scope_mask,
    data_work = prep$data_work,
    cols = list(
      method = prep$method_col,
      raw_missing = prep$raw_missing_col,
      raw_numeric = prep$raw_numeric_col,
      raw = prep$raw_col,
      orig_val = prep$original_value_numeric,
      orig_method = prep$original_method
    )
  )
}

.fg_validate_inputs <- function(
  data,
  value_col,
  time_col,
  group_by,
  smooth_window,
  max_gap_linear
) {
  if (!rlang::has_name(data, time_col)) {
    cli::cli_abort("Time column '{time_col}' not found in data")
  }
  if (!rlang::has_name(data, value_col)) {
    cli::cli_abort("Value column '{value_col}' not found in data")
  }
  if (!rlang::is_scalar_integerish(smooth_window) || smooth_window < 1) {
    cli::cli_abort("`smooth_window` must be a positive integer")
  }
  smooth_window <- as.integer(smooth_window)

  if (!is.null(group_by)) {
    missing <- setdiff(group_by, names(data))
    if (length(missing) > 0) {
      cli::cli_abort(
        "Group columns not found: {paste(missing, collapse = ', ')}"
      )
    }
  }
  max_gap_linear <- max(0L, as.integer(max_gap_linear))
  list(
    data = data,
    smooth_window = smooth_window,
    max_gap_linear = max_gap_linear
  )
}

.fg_calculate_scope_mask <- function(data, fill_scope) {
  if (is.null(fill_scope)) {
    return(rep(TRUE, nrow(data)))
  }
  fill_scope_expr <- rlang::enquo(fill_scope)
  scope_mask <- data |>
    dplyr::mutate(scope_mask = !!fill_scope_expr) |>
    dplyr::pull()
  if (!is.logical(scope_mask) || length(scope_mask) != nrow(data)) {
    cli::cli_abort(
      "`fill_scope` must evaluate to a logical vector with length equal to nrow(data)"
    )
  }
  scope_mask[is.na(scope_mask)] <- FALSE
  scope_mask
}

.fg_prepare_working_columns <- function(data, value_col) {
  function_tag <- "proxy"
  # Original raw capture
  raw_original_col <- paste0(value_col, "_raw_original")
  if (!raw_original_col %in% names(data)) {
    data[[raw_original_col]] <- suppressWarnings(as.numeric(data[[value_col]]))
  }
  # Snapshot columns logic
  snapshot_base <- paste0(value_col, "_raw_", function_tag)
  snapshot_col <- snapshot_base
  snapshot_index <- 1L
  while (snapshot_col %in% names(data)) {
    snapshot_index <- snapshot_index + 1L
    snapshot_col <- paste0(snapshot_base, "_", snapshot_index)
  }
  data[[snapshot_col]] <- suppressWarnings(as.numeric(data[[value_col]]))

  # Define working column names
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

  # Add working columns
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

  list(
    data_work = data_work,
    method_col = method_col,
    raw_missing_col = raw_missing_col,
    raw_numeric_col = raw_numeric_col,
    raw_col = raw_col,
    original_value_numeric = original_value_numeric,
    original_method = original_method
  )
}

.fg_finalize_and_report <- function(
  data_work,
  original_data,
  value_col,
  cols,
  scope_mask,
  output_format,
  verbose
) {
  result <- .fg_finalize_output(
    data_work,
    value_col,
    cols,
    scope_mask,
    output_format
  )

  if (verbose) {
    n_orig <- sum(is.na(original_data[[value_col]]))
    n_final <- sum(is.na(result[[value_col]]))
    message(
      "Total filled: ",
      n_orig - n_final,
      " out of ",
      n_orig,
      " missing values"
    )
  }
  result
}

# --- 2. Growth Calculation Logic ---

.fg_compute_all_growth_rates <- function(
  data_work,
  proxy_col,
  value_col,
  group_by,
  smooth_window,
  verbose
) {
  for (i in seq_along(proxy_col)) {
    spec <- .parse_proxy_spec(
      proxy_col[i],
      data_work,
      value_col,
      group_by,
      verbose
    )
    # Calculate for this specific proxy
    data_work <- .fg_calc_single_spec(
      data_work,
      spec,
      i,
      smooth_window,
      group_by,
      verbose
    )
  }
  data_work
}

.fg_calc_single_spec <- function(
  data,
  spec,
  idx,
  smooth_window,
  group_by,
  verbose
) {
  growth_col <- paste0("growth_", idx, "_", spec$spec_name)
  obs_col <- paste0("n_obs_", idx, "_", spec$spec_name)

  if (verbose) {
    message("Calculating growth rates for: ", spec$spec_name)
  }

  # 1. Prepare Data.Table with Lags
  dt <- .fg_growth_prep_dt(data, spec, group_by)
  if (nrow(dt) == 0) {
    return(.fg_add_empty_cols(data, growth_col, obs_col))
  }

  # 2. Compute Individual Row Growth (with Smoothing)
  dt <- .fg_growth_calc_individual(dt, spec, smooth_window)

  # 3. Aggregate to Groups
  summary_dt <- .fg_growth_aggregate(dt, spec, growth_col, obs_col)

  # 4. Join back
  join_keys <- c("year", spec$present_group_vars)
  if (length(spec$present_group_vars) == 0) {
    join_keys <- "year"
  }

  dplyr::left_join(data, tibble::as_tibble(summary_dt), by = join_keys)
}

.fg_growth_prep_dt <- function(data, spec, group_by) {
  if (!spec$source_var %in% names(data)) {
    return(data.table::data.table())
  }

  lag_vars <- unique(c(group_by, spec$present_group_vars))
  lag_vars <- lag_vars[lag_vars %in% names(data)]

  cols <- unique(c("year", lag_vars, spec$source_var, spec$weight_col))
  dt <- data.table::as.data.table(data[, cols, drop = FALSE])

  data.table::setorderv(dt, unique(c(lag_vars, "year")))
  dt
}

.fg_growth_calc_individual <- function(dt, spec, window) {
  # Helper to manage lag vars
  by_vars <- if (length(spec$present_group_vars) > 0) {
    spec$present_group_vars
  } else {
    NULL
  }

  # Apply Smoothing (Moving Average)
  if (window > 1) {
    dt <- .compute_ma_base(dt, spec$source_var, window, by_vars)
    val_col <- "ma_base"
  } else {
    val_col <- spec$source_var
  }

  # Create Lags
  if (!is.null(by_vars)) {
    dt[,
      `:=`(
        lag_src = data.table::shift(get(val_col)),
        lag_yr = data.table::shift(year)
      ),
      by = by_vars
    ]
  } else {
    dt[, `:=`(
      lag_src = data.table::shift(get(val_col)),
      lag_yr = data.table::shift(year)
    )]
  }

  # Calculate Growth
  dt[,
    ind_growth := data.table::fifelse(
      year == lag_yr + 1 &
        !is.na(get(val_col)) &
        !is.na(lag_src) &
        lag_src > 0,
      (get(val_col) - lag_src) / lag_src,
      NA_real_
    )
  ]

  dt[!is.na(ind_growth)]
}

.fg_growth_aggregate <- function(dt, spec, growth_col, obs_col) {
  by_vars <- c("year", spec$present_group_vars)
  if (length(spec$present_group_vars) == 0) {
    by_vars <- "year"
  }

  # Setup weights
  has_w <- !is.null(spec$weight_col)
  if (has_w) {
    # Shift weights to align with growth period (previous year weight)
    grp <- if (length(spec$present_group_vars) > 0) {
      spec$present_group_vars
    } else {
      NULL
    }
    if (!is.null(grp)) {
      dt[, w := data.table::shift(get(spec$weight_col)), by = grp]
    } else {
      dt[, w := data.table::shift(get(spec$weight_col))]
    }
  }

  res <- dt[,
    {
      val <- mean(ind_growth) # Default unweighted
      cnt <- .N

      if (has_w) {
        valid_w <- !is.na(w) & is.finite(w) & w > 0
        if (any(valid_w)) {
          val <- sum(ind_growth[valid_w] * w[valid_w]) / sum(w[valid_w])
          cnt <- sum(valid_w)
        }
      }
      .(g_val = val, o_val = cnt)
    },
    by = by_vars
  ]

  data.table::setnames(res, c("g_val", "o_val"), c(growth_col, obs_col))
  res
}

.fg_add_empty_cols <- function(data, g_col, o_col) {
  data[[g_col]] <- NA_real_
  data[[o_col]] <- 0L
  data
}

# --- 3. Filling Logic ---

.fg_apply_hierarchical_filling <- function(
  data,
  proxy_cols,
  value_col,
  cols,
  max_gap,
  max_gap_lin,
  time_col,
  group_by,
  verbose
) {
  if (verbose) {
    message("Step 2: Applying hierarchical filling...")
  }

  # Pre-calculate spec names for bridge fallback
  specs <- lapply(proxy_cols, function(p) {
    .parse_proxy_spec(p, data, value_col, group_by, FALSE)$spec_name
  })

  for (i in seq_along(proxy_cols)) {
    if (sum(is.na(data[[value_col]])) == 0) {
      break
    }

    spec_name <- specs[[i]]
    g_col <- paste0("growth_", i, "_", spec_name)
    m_name <- paste0("growth_", spec_name)

    if (verbose) {
      message("  Applying proxy level ", i, ": ", proxy_cols[i])
    }

    # Define filling function wrapper
    fill_fun <- function(df) {
      .fg_fill_sequence(
        df,
        g_col,
        m_name,
        value_col,
        cols$method,
        cols$raw_missing,
        max_gap,
        max_gap_lin,
        time_col,
        i,
        specs
      )
    }

    if (!is.null(group_by)) {
      data <- data |>
        dplyr::group_by(dplyr::across(dplyr::all_of(group_by))) |>
        dplyr::group_modify(~ fill_fun(.x)) |>
        dplyr::ungroup()
    } else {
      data <- fill_fun(data)
    }
  }
  data
}

.fg_fill_sequence <- function(
  df,
  growth_col,
  method_name,
  val_col,
  met_col,
  miss_col,
  max_gap,
  max_lin,
  time_col,
  level,
  all_specs
) {
  # Ensure Order
  df <- df[order(df[[time_col]]), , drop = FALSE]

  # 1. Identify Valid NA Runs (gaps that are allowed to be filled)
  na_runs <- .fg_identify_na_runs(df[[miss_col]])

  # 2. Forward Fill
  df <- .fg_fill_direction(
    df,
    "forward",
    val_col,
    growth_col,
    met_col,
    method_name,
    na_runs,
    max_gap,
    max_lin
  )

  # 3. Backward Fill
  df <- .fg_fill_direction(
    df,
    "backward",
    val_col,
    growth_col,
    met_col,
    paste0(method_name, "_back"),
    na_runs,
    max_gap,
    max_lin
  )

  # 4. Bridge Fill (Linear + Geometric)
  df <- .fg_fill_bridge(
    df,
    val_col,
    growth_col,
    met_col,
    miss_col,
    time_col,
    na_runs,
    max_gap,
    max_lin,
    level,
    all_specs,
    method_name
  )

  df
}

.fg_identify_na_runs <- function(missing_vec) {
  if (is.null(missing_vec)) {
    return(list())
  }

  # Convert to boolean, treating existing NAs as missing
  is_miss <- missing_vec
  is_miss[is.na(is_miss)] <- TRUE

  rle_res <- rle(is_miss)
  ends <- cumsum(rle_res$lengths)
  starts <- c(1, ends[-length(ends)] + 1)

  runs <- list()
  for (i in seq_along(rle_res$values)) {
    if (rle_res$values[i]) {
      # Check if "internal" (surrounded by data, not at ends of series)
      is_internal <- (starts[i] > 1) &&
        (ends[i] < length(missing_vec)) &&
        (!isTRUE(missing_vec[starts[i] - 1])) &&
        (!isTRUE(missing_vec[ends[i] + 1]))

      runs[[length(runs) + 1]] <- list(
        start = starts[i],
        end = ends[i],
        len = rle_res$lengths[i],
        internal = is_internal
      )
    }
  }
  runs
}

.fg_fill_direction <- function(
  df,
  direction,
  v_col,
  g_col,
  m_col,
  m_name,
  runs,
  max_gap,
  max_lin
) {
  vals <- df[[v_col]]
  grw <- df[[g_col]]
  mets <- df[[m_col]]
  indices <- if (direction == "forward") {
    seq_along(vals)
  } else {
    rev(seq_along(vals))
  }

  for (i in indices) {
    if (!is.na(vals[i])) {
      next
    }

    # Check Constraints
    is_valid_gap <- TRUE
    for (r in runs) {
      if (i >= r$start && i <= r$end) {
        if (!is.null(max_gap) && r$len > max_gap) {
          is_valid_gap <- FALSE
        }
        if (
          !is.null(max_lin) &&
            r$internal &&
            r$len > max_lin
        ) {
          is_valid_gap <- FALSE
        }
      }
    }
    if (!is_valid_gap) {
      next
    }

    # Calc Value
    if (direction == "forward") {
      if (i == 1) {
        next
      }
      prev <- vals[i - 1]
      if (!is.na(prev) && prev > 0 && !is.na(grw[i])) {
        vals[i] <- prev * (1 + grw[i])
        if (mets[i] == "missing") mets[i] <- m_name
      }
    } else {
      if (i == length(vals)) {
        next
      }
      nxt <- vals[i + 1]
      nxt_g <- grw[i + 1]
      if (!is.na(nxt) && !is.na(nxt_g) && (1 + nxt_g) != 0) {
        res <- nxt / (1 + nxt_g)
        if (is.finite(res) && res > 0) {
          vals[i] <- res
          if (mets[i] == "missing") mets[i] <- m_name
        }
      }
    }
  }
  df[[v_col]] <- vals
  df[[m_col]] <- mets
  df
}

.fg_fill_bridge <- function(
  df,
  v_col,
  g_col,
  m_col,
  miss_col,
  t_col,
  runs,
  max_gap,
  max_lin,
  level,
  specs,
  base_method
) {
  # Only process internal gaps (bridges)
  bridges <- Filter(function(x) x$internal, runs)

  for (run in bridges) {
    if (!is.null(max_gap) && run$len > max_gap) {
      next
    }

    # 1. Linear Interpolation (Small Gaps)
    if (run$len <= max_lin) {
      df <- .fg_bridge_linear(df, run, v_col, m_col, t_col)
      next
    }

    # 2. Geometric Bridge (Large Gaps with Lambda)
    df <- .fg_bridge_geometric(
      df,
      run,
      v_col,
      g_col,
      m_col,
      level,
      specs,
      base_method
    )
  }
  df
}

.fg_bridge_linear <- function(df, run, v_col, m_col, t_col) {
  # Indices
  idx_start <- run$start - 1
  idx_end <- run$end + 1
  idx_gap <- run$start:run$end

  v0 <- df[[v_col]][idx_start]
  v1 <- df[[v_col]][idx_end]

  if (is.finite(v0) && is.finite(v1) && v0 > 0 && v1 > 0) {
    approx_vals <- stats::approx(
      x = c(df[[t_col]][idx_start], df[[t_col]][idx_end]),
      y = c(v0, v1),
      xout = df[[t_col]][idx_gap]
    )$y

    df[[v_col]][idx_gap] <- approx_vals
    df[[m_col]][idx_gap] <- "linear_interp"
  }
  df
}

.fg_bridge_geometric <- function(
  df,
  run,
  v_col,
  g_col,
  m_col,
  level,
  specs,
  base_method
) {
  idx_start <- run$start - 1
  idx_end <- run$end + 1
  idx_seq <- (idx_start + 1):idx_end # Growth rates needed for these positions

  # Combine growth rates from hierarchy (Step 1-current)
  combined <- .fg_combine_growth_hierarchy(df, idx_seq, level, specs)
  rates <- combined$rates
  sources <- combined$sources

  # Lambda Adjustment
  v_start <- df[[v_col]][idx_start]
  v_end <- df[[v_col]][idx_end]

  pred_end <- v_start * prod(1 + rates)

  if (is.finite(pred_end) && pred_end > 0 && is.finite(v_end) && v_end > 0) {
    lambda <- (v_end / pred_end)^(1 / length(rates))

    curr_val <- v_start
    # Iterate and fill
    for (k in seq_along(rates)) {
      target_idx <- idx_start + k
      curr_val <- curr_val * (1 + rates[k]) * lambda

      # Only overwrite if it's strictly inside the gap (not the end anchor)
      if (target_idx < idx_end) {
        df[[v_col]][target_idx] <- curr_val
        src <- sources[k]
        if (is.na(src)) {
          src <- base_method
        }
        df[[m_col]][target_idx] <- paste0("growth_", src, "_bridge")
      }
    }
  }
  df
}

.fg_combine_growth_hierarchy <- function(df, indices, level, specs) {
  # Default to current level
  rates <- df[[paste0("growth_", level, "_", specs[[level]])]][indices]
  sources <- rep(specs[[level]], length(indices))

  # Overwrite with better (lower index) proxies if available
  if (level > 1) {
    for (l in 1:(level - 1)) {
      col <- paste0("growth_", l, "_", specs[[l]])
      if (col %in% names(df)) {
        better_rates <- df[[col]][indices]
        # Take if available
        mask <- !is.na(better_rates)
        rates[mask] <- better_rates[mask]
        sources[mask] <- specs[[l]]
      }
    }
  }
  list(rates = rates, sources = sources)
}

.fg_finalize_output <- function(data, value_col, cols, mask, format) {
  # Restore non-scope values
  if (!all(mask)) {
    data[[value_col]][!mask] <- cols$orig_val[!mask]
    data[[cols$method]][!mask] <- cols$orig_method[!mask]
  }

  if (format == "clean") {
    # Remove all temp columns (growth_, n_obs_, raw_)
    ptn <- "^(growth_|n_obs_|.*_raw_).*"
    data <- data |> dplyr::select(-dplyr::matches(ptn))
  } else {
    # Detailed: keep debug cols but clean raw numeric
    data <- data |>
      dplyr::select(-dplyr::any_of(c(cols$raw_numeric, cols$raw_missing)))
  }
  data
}

# --- Shared Helpers (Original) ---

.compute_ma_base <- function(dt, value_var, window, group_vars = NULL) {
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

.parse_proxy_spec <- function(spec, data, value_col, group_by, verbose) {
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
