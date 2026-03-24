# Simple functions to fill gaps (NA values) in a time-dependent variable,
# creating complete time series.

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
#' @param time_col The column containing time values. Default: `year`.
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
  time_col = year,
  interpolate = TRUE,
  fill_forward = TRUE,
  fill_backward = TRUE,
  value_smooth_window = NULL,
  .by = NULL
) {
  value_col_name <- rlang::as_name(rlang::enquo(value_col))
  time_col_name <- rlang::as_name(rlang::enquo(time_col))
  source_col_name <- paste0("source_", value_col_name)
  by_cols <- .by %||% character(0)

  n <- nrow(data)
  if (n == 0L) {
    data[[source_col_name]] <- character(0)
    return(data)
  }

  # Fast duplicate check: anyDuplicated first, expensive summarise only
  # if needed
  dup_cols <- c(by_cols, time_col_name)
  if (anyDuplicated(data[, dup_cols, drop = FALSE]) > 0L) {
    dups <- data |>
      dplyr::summarise(
        n = dplyr::n(),
        .by = dplyr::all_of(dup_cols)
      ) |>
      dplyr::filter(n > 1)
    if (nrow(dups) > 0) {
      caller <- tryCatch(
        deparse(sys.call(sys.parent(1L))),
        error = function(e) "unknown"
      )
      cli::cli_warn(
        "Duplicate {time_col_name} values found within groups. \\
        {nrow(dups)} group/time combination(s) have more than one row. \\
        [called from: {caller}]"
      )
    }
  }

  # No groups: simple path
  if (length(by_cols) == 0L) {
    orig_vals <- data[[value_col_name]]
    times <- data[[time_col_name]]
    smooth_vals <- if (!is.null(value_smooth_window)) {
      zoo::rollmean(
        orig_vals,
        k = value_smooth_window,
        fill = NA,
        align = "center"
      )
    } else {
      orig_vals
    }
    res <- .fill_linear_vec(
      orig_vals,
      smooth_vals,
      times,
      interpolate,
      fill_forward,
      fill_backward
    )
    data[[value_col_name]] <- res$value
    data[[source_col_name]] <- res$source
    return(data)
  }

  # Grouped path: sort once, iterate over contiguous group ranges.
  # Use stable group IDs that preserve NA patterns across grouping keys.
  grp_id <- vctrs::vec_group_id(data[by_cols])
  times <- data[[time_col_name]]
  ord <- order(grp_id, times)
  inv_ord <- integer(n)
  inv_ord[ord] <- seq_len(n)

  orig_vals <- data[[value_col_name]][ord]
  times_sorted <- times[ord]
  grp_sorted <- grp_id[ord]

  # Smoothing (on sorted data)
  use_smoothing <- !is.null(value_smooth_window)
  if (use_smoothing) {
    smooth_vals <- .grouped_rollmean(
      orig_vals,
      grp_sorted,
      value_smooth_window
    )
  } else {
    smooth_vals <- orig_vals
  }

  # Find contiguous group boundaries
  breaks <- which(diff(grp_sorted) != 0L)
  starts <- c(1L, breaks + 1L)
  ends <- c(breaks, n)
  n_groups <- length(starts)

  # Pre-allocate results
  filled <- orig_vals
  sources <- ifelse(is.na(orig_vals), "Gap not filled", "Original")

  for (g in seq_len(n_groups)) {
    i1 <- starts[g]
    i2 <- ends[g]
    rng <- i1:i2

    ov <- orig_vals[rng]
    sv <- smooth_vals[rng]
    tv <- times_sorted[rng]
    m <- length(rng)

    valid <- which(!is.na(sv))
    if (length(valid) == 0L) {
      next
    }

    first_v <- valid[1L]
    last_v <- valid[length(valid)]

    # Carry backward
    if (fill_backward && first_v > 1L) {
      na_left <- which(is.na(ov[seq_len(first_v - 1L)]))
      if (length(na_left) > 0L) {
        filled[i1 - 1L + na_left] <- sv[first_v]
        sources[i1 - 1L + na_left] <- "First value carried backwards"
      }
    }

    # Carry forward
    if (fill_forward && last_v < m) {
      tail_idx <- (last_v + 1L):m
      na_right <- tail_idx[is.na(ov[tail_idx])]
      if (length(na_right) > 0L) {
        filled[i1 - 1L + na_right] <- sv[last_v]
        sources[i1 - 1L + na_right] <- "Last value carried forward"
      }
    }

    # Interpolate
    if (interpolate && length(valid) >= 2L) {
      mid_idx <- (first_v + 1L):(last_v - 1L)
      if (length(mid_idx) > 0L) {
        na_mid <- mid_idx[is.na(ov[mid_idx])]
        if (length(na_mid) > 0L) {
          interp <- .safe_na_approx(sv, x = tv, na.rm = FALSE)
          if (length(interp) == m) {
            fill_mask <- na_mid[!is.na(interp[na_mid])]
            if (length(fill_mask) > 0L) {
              filled[i1 - 1L + fill_mask] <- interp[fill_mask]
              sources[i1 - 1L + fill_mask] <- "Linear interpolation"
            }
          }
        }
      }
    }

    # Preserve originals
    has_orig <- which(!is.na(ov))
    filled[i1 - 1L + has_orig] <- ov[has_orig]
    sources[i1 - 1L + has_orig] <- "Original"
  }

  # Unsort back to original order
  data[[value_col_name]] <- filled[inv_ord]
  data[[source_col_name]] <- sources[inv_ord]
  data
}

# Grouped rolling mean without split/interaction overhead
.grouped_rollmean <- function(vals, grp_sorted, k) {
  n <- length(vals)
  result <- rep(NA_real_, n)
  breaks <- which(diff(grp_sorted) != 0L)
  starts <- c(1L, breaks + 1L)
  ends <- c(breaks, n)
  for (g in seq_along(starts)) {
    rng <- starts[g]:ends[g]
    result[rng] <- zoo::rollmean(
      vals[rng],
      k = k,
      fill = NA,
      align = "center"
    )
  }
  result
}

# Base R vectorized core for fill_linear (no dplyr)
.fill_linear_vec <- function(
  orig_vals,
  smooth_vals,
  times,
  interpolate,
  fill_forward,
  fill_backward
) {
  n <- length(orig_vals)
  filled <- orig_vals
  source <- rep("Gap not filled", n)
  source[!is.na(orig_vals)] <- "Original"

  if (n == 0L) {
    return(list(value = filled, source = source))
  }

  valid <- which(!is.na(smooth_vals))
  if (length(valid) == 0L) {
    return(list(value = filled, source = source))
  }

  first_valid <- valid[1L]
  last_valid <- valid[length(valid)]

  # Carry backward (left region)
  if (fill_backward && first_valid > 1L) {
    left_idx <- seq_len(first_valid - 1L)
    na_left <- left_idx[is.na(filled[left_idx])]
    if (length(na_left) > 0L) {
      filled[na_left] <- smooth_vals[first_valid]
      source[na_left] <- "First value carried backwards"
    }
  }

  # Carry forward (right region)
  if (fill_forward && last_valid < n) {
    right_idx <- (last_valid + 1L):n
    na_right <- right_idx[is.na(filled[right_idx])]
    if (length(na_right) > 0L) {
      filled[na_right] <- smooth_vals[last_valid]
      source[na_right] <- "Last value carried forward"
    }
  }

  # Interpolate (middle region)
  if (interpolate && length(valid) >= 2L) {
    mid_idx <- (first_valid + 1L):(last_valid - 1L)
    if (length(mid_idx) > 0L) {
      na_mid <- mid_idx[is.na(filled[mid_idx])]
      if (length(na_mid) > 0L) {
        interp <- .safe_na_approx(smooth_vals, x = times, na.rm = FALSE)
        if (length(interp) == n) {
          fill_mask <- na_mid[!is.na(interp[na_mid])]
          filled[fill_mask] <- interp[fill_mask]
          source[fill_mask] <- "Linear interpolation"
        }
      }
    }
  }

  # Preserve original values
  has_orig <- !is.na(orig_vals)
  filled[has_orig] <- orig_vals[has_orig]
  source[has_orig] <- "Original"

  list(value = filled, source = source)
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
#' @param time_col The column containing time values. Default: `year`.
#' @param start_with_zero Logical. If TRUE (default), assumes an invisible 0
#'   value before the first observation and fills with cumulative sum starting
#'   from the first change_col value. If FALSE, starting NA values remain
#'   unfilled.
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
  time_col = year,
  start_with_zero = TRUE,
  .by = NULL
) {
  value_col_name <- rlang::as_name(rlang::enquo(value_col))
  time_col_name <- rlang::as_name(rlang::enquo(time_col))
  source_col_name <- paste0("source_", value_col_name)

  data |>
    dplyr::arrange(dplyr::across(dplyr::all_of(c(.by, time_col_name)))) |>
    dplyr::mutate(
      groups = cumsum(!is.na({{ value_col }})),
      prefilled = dplyr::coalesce({{ value_col }}, {{ change_col }}),
      .source_temp = ifelse(
        is.na({{ value_col }}),
        "Filled with sum",
        "Original"
      ),
      "{{ value_col }}" := ave(prefilled, groups, FUN = cumsum),
      "{{ value_col }}" := if (start_with_zero) {{ value_col }} else {
        ifelse(groups == 0, NA, {{ value_col }})
      },
      .source_temp = ifelse(
        is.na({{ value_col }}),
        NA_character_,
        .source_temp
      ),
      groups = NULL,
      prefilled = NULL,
      .by = dplyr::all_of(.by)
    ) |>
    dplyr::rename(!!source_col_name := .source_temp)
}

#' Fill gaps using growth rates from proxy variables
#'
#' @description
#'   Fills missing values using growth rates from a proxy variable (reference
#'   series). Supports regional aggregations, weighting, and linear
#'   interpolation for small gaps.
#'
#' @param data A data frame containing time series data.
#' @param value_col The column containing values to fill.
#' @param proxy_col Character or vector. Proxy variable(s) for calculating
#'   growth rates. Supports multiple syntax formats:
#'   - **Simple numeric proxy** (e.g., `"population"`): Auto-detects numeric
#'     columns and uses them as proxy variable. Inherits the `.by`
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
#' @param time_col The column containing time values. Default: `year`.
#' @param .by A character vector with the grouping variables (optional).
#' @param max_gap Numeric. Maximum gap size to fill using growth method.
#'   Default: Inf.
#' @param max_gap_linear Numeric. Maximum gap size for linear interpolation
#'   fallback. Default: 3.
#' @param fill_scope Quosure. Filter expression to limit filling scope.
#'   Default: NULL.
#' @param value_smooth_window Integer. Window size for a centered moving
#'   average applied to the value column before gap-filling. Useful for
#'   variables with high inter-annual variability. If `NULL` (default), no
#'   smoothing is applied.
#' @param proxy_smooth_window Integer. Window size for moving average smoothing
#'   of proxy reference values before computing growth rates. Default: 1.
#' @param output_format Character. Output format: "clean" or "detailed".
#'   Default: "clean".
#' @param verbose Logical. Print progress messages. Default: TRUE.
#'
#' @return
#'   A data frame with filled values. If output_format = "clean", returns
#'   original columns with updated value_col and added source column. If
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
#' fill_proxy_growth(
#'   data,
#'   value_col = gdp,
#'   proxy_col = "population",
#'   .by = "country"
#' )
#'
#' @seealso [fill_linear()], [fill_sum()]
fill_proxy_growth <- function(
  data,
  value_col,
  proxy_col,
  time_col = year,
  .by = NULL,
  max_gap = Inf,
  max_gap_linear = 3,
  fill_scope = NULL,
  value_smooth_window = NULL,
  proxy_smooth_window = 1,
  output_format = "clean",
  verbose = TRUE
) {
  # Convert column names to strings for internal use
  value_col_name <- rlang::as_name(rlang::enquo(value_col))
  time_col_name <- rlang::as_name(rlang::enquo(time_col))

  # 1. Setup and Validation
  setup <- .fg_setup(
    data,
    value_col_name,
    time_col_name,
    .by,
    fill_scope,
    value_smooth_window,
    proxy_smooth_window,
    max_gap_linear
  )

  # 2. Calculate Growth Rates
  data_work <- .fg_compute_all_growth_rates(
    setup$data_work,
    proxy_col,
    value_col_name,
    .by,
    setup$inputs$proxy_smooth_window,
    time_col_name,
    verbose
  )

  # 3. Apply Filling (Hierarchical)
  data_work <- .fg_apply_hierarchical_filling(
    data_work,
    proxy_col,
    value_col_name,
    setup$cols,
    max_gap,
    setup$inputs$max_gap_linear,
    time_col_name,
    .by,
    verbose
  )

  # 4. Finalize
  .fg_finalize_and_report(
    data_work,
    data,
    value_col_name,
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
  .by,
  fill_scope,
  value_smooth_window,
  proxy_smooth_window,
  max_gap_linear
) {
  inputs <- .fg_validate_inputs(
    data,
    value_col,
    time_col,
    .by,
    value_smooth_window,
    proxy_smooth_window,
    max_gap_linear
  )

  scope_mask <- .fg_calculate_scope_mask(inputs$data, fill_scope)
  prep <- .fg_prepare_working_columns(
    inputs$data,
    value_col,
    inputs$value_smooth_window,
    .by
  )

  list(
    inputs = inputs,
    scope_mask = scope_mask,
    data_work = prep$data_work,
    cols = list(
      source = prep$source_col,
      raw_missing = prep$raw_missing_col,
      raw_numeric = prep$raw_numeric_col,
      raw = prep$raw_col,
      orig_val = prep$original_value_numeric,
      orig_source = prep$original_source
    )
  )
}

.fg_validate_inputs <- function(
  data,
  value_col,
  time_col,
  .by,
  value_smooth_window,
  proxy_smooth_window,
  max_gap_linear
) {
  if (!rlang::has_name(data, time_col)) {
    cli::cli_abort("Time column '{time_col}' not found in data")
  }
  if (!rlang::has_name(data, value_col)) {
    cli::cli_abort("Value column '{value_col}' not found in data")
  }
  if (
    !is.null(value_smooth_window) &&
      (!rlang::is_scalar_integerish(value_smooth_window) ||
        value_smooth_window < 1)
  ) {
    cli::cli_abort("`value_smooth_window` must be a positive integer or NULL")
  }
  if (
    !rlang::is_scalar_integerish(proxy_smooth_window) ||
      proxy_smooth_window < 1
  ) {
    cli::cli_abort("`proxy_smooth_window` must be a positive integer")
  }
  proxy_smooth_window <- as.integer(proxy_smooth_window)

  if (!is.null(.by)) {
    missing <- setdiff(.by, names(data))
    if (length(missing) > 0) {
      cli::cli_abort(
        "Group columns not found: {paste(missing, collapse = ', ')}"
      )
    }
  }
  max_gap_linear <- max(0L, as.integer(max_gap_linear))
  list(
    data = data,
    value_smooth_window = value_smooth_window,
    proxy_smooth_window = proxy_smooth_window,
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

.fg_prepare_working_columns <- function(
  data,
  value_col,
  value_smooth_window,
  .by
) {
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
  source_col <- paste0("source_", value_col)
  has_source_col <- source_col %in% names(data)

  original_value_numeric <- suppressWarnings(as.numeric(data[[value_col]]))
  original_value_numeric[!is.finite(original_value_numeric)] <- NA_real_
  original_source <- if (has_source_col) {
    as.character(data[[source_col]])
  } else {
    ifelse(is.na(original_value_numeric), "missing", "original")
  }

  # Apply smoothing to value column if requested
  # Smoothed values are used for interpolation anchors, but original non-NA

  # values are always preserved (smoothing only affects gap-filling behavior)
  use_smoothing <- !is.null(value_smooth_window)
  if (use_smoothing) {
    data <- data |>
      dplyr::mutate(
        .smooth_var = zoo::rollmean(
          original_value_numeric,
          k = value_smooth_window,
          fill = NA,
          align = "center"
        ),
        .by = dplyr::all_of(.by)
      )
  } else {
    data <- data |>
      dplyr::mutate(.smooth_var = original_value_numeric)
  }

  # Add working columns
  # raw_missing tracks what was ORIGINALLY missing (before smoothing)
  # value_col uses smoothed values for gap-filling, but coalesces with original
  data_work <- data |>
    dplyr::mutate(
      !!raw_col := .data[[value_col]],
      !!raw_numeric_col := .smooth_var,
      !!raw_missing_col := is.na(original_value_numeric),
      .smooth_var = NULL
    ) |>
    dplyr::mutate(
      !!source_col := if (has_source_col) {
        .data[[source_col]]
      } else {
        ifelse(.data[[raw_missing_col]], "missing", "original")
      },
      # Use original where available, smoothed otherwise (for anchor points)
      !!value_col := dplyr::coalesce(
        original_value_numeric,
        .data[[raw_numeric_col]]
      )
    )
  data_work[[source_col]][is.na(data_work[[value_col]])] <- "missing"

  list(
    data_work = data_work,
    source_col = source_col,
    raw_missing_col = raw_missing_col,
    raw_numeric_col = raw_numeric_col,
    raw_col = raw_col,
    original_value_numeric = original_value_numeric,
    original_source = original_source
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
  .by,
  smooth_window,
  time_col,
  verbose
) {
  for (i in seq_along(proxy_col)) {
    spec <- .parse_proxy_spec(
      proxy_col[i],
      data_work,
      value_col,
      .by,
      verbose
    )
    # Calculate for this specific proxy
    data_work <- .fg_calc_single_spec(
      data_work,
      spec,
      i,
      smooth_window,
      .by,
      time_col,
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
  .by,
  time_col,
  verbose
) {
  growth_col <- paste0("growth_", idx, "_", spec$spec_name)
  obs_col <- paste0("n_obs_", idx, "_", spec$spec_name)

  if (verbose) {
    message("Calculating growth rates for: ", spec$spec_name)
  }

  # 1. Prepare tibble with relevant columns
  prep <- .fg_growth_prep(data, spec, .by, time_col)
  if (nrow(prep) == 0) {
    return(.fg_add_empty_cols(data, growth_col, obs_col))
  }

  # 2. Compute Individual Row Growth (with Smoothing)
  prep <- .fg_growth_calc_individual(prep, spec, smooth_window, time_col)

  # 3. Aggregate to Groups
  summary_tbl <- .fg_growth_aggregate(
    prep,
    spec,
    growth_col,
    obs_col,
    time_col
  )

  # 4. Join back
  join_keys <- c(time_col, spec$present_group_vars)
  if (length(spec$present_group_vars) == 0) {
    join_keys <- time_col
  }

  dplyr::left_join(data, summary_tbl, by = join_keys)
}

.fg_growth_prep <- function(data, spec, .by, time_col) {
  if (!spec$source_var %in% names(data)) {
    return(tibble::tibble())
  }

  lag_vars <- unique(c(.by, spec$present_group_vars))
  lag_vars <- lag_vars[lag_vars %in% names(data)]

  cols <- unique(c(time_col, lag_vars, spec$source_var, spec$weight_col))
  cols <- cols[!is.na(cols)]

  data |>
    dplyr::select(dplyr::all_of(cols)) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(unique(c(lag_vars, time_col)))))
}

.fg_growth_calc_individual <- function(data, spec, window, time_col) {
  # Helper to manage lag vars
  by_vars <- if (length(spec$present_group_vars) > 0) {
    spec$present_group_vars
  } else {
    NULL
  }

  # Apply Smoothing (Moving Average)
  if (window > 1) {
    data <- .compute_ma_base_dplyr(data, spec$source_var, window, by_vars)
    val_col <- "ma_base"
  } else {
    val_col <- spec$source_var
  }

  # Create Lags and Calculate Growth
  data <- data |>
    dplyr::mutate(
      lag_src = dplyr::lag(.data[[val_col]]),
      lag_yr = dplyr::lag(.data[[time_col]]),
      ind_growth = dplyr::if_else(
        .data[[time_col]] == lag_yr + 1 &
          !is.na(.data[[val_col]]) &
          !is.na(lag_src) &
          lag_src > 0,
        (.data[[val_col]] - lag_src) / lag_src,
        NA_real_
      ),
      .by = dplyr::all_of(by_vars)
    )

  data |>
    dplyr::filter(!is.na(ind_growth))
}

.fg_growth_aggregate <- function(
  data,
  spec,
  growth_col,
  obs_col,
  time_col
) {
  by_vars <- c(time_col, spec$present_group_vars)
  if (length(spec$present_group_vars) == 0) {
    by_vars <- time_col
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

    data <- data |>
      dplyr::mutate(
        w = dplyr::lag(.data[[spec$weight_col]]),
        .by = dplyr::all_of(grp)
      )

    # Weighted aggregation
    data |>
      dplyr::summarise(
        !!growth_col := {
          valid_w <- !is.na(w) & is.finite(w) & w > 0
          if (any(valid_w)) {
            sum(ind_growth[valid_w] * w[valid_w]) / sum(w[valid_w])
          } else {
            mean(ind_growth)
          }
        },
        !!obs_col := {
          valid_w <- !is.na(w) & is.finite(w) & w > 0
          if (any(valid_w)) sum(valid_w) else dplyr::n()
        },
        .by = dplyr::all_of(by_vars)
      )
  } else {
    # Unweighted aggregation
    data |>
      dplyr::summarise(
        !!growth_col := mean(ind_growth),
        !!obs_col := dplyr::n(),
        .by = dplyr::all_of(by_vars)
      )
  }
}

.fg_add_empty_cols <- function(data, g_col, o_col) {
  data[[g_col]] <- NA_real_
  data[[o_col]] <- 0L
  data
}


# --- 3. Filling Logic (Vectorized) ---

.fg_apply_hierarchical_filling <- function(
  data,
  proxy_cols,
  value_col,
  cols,
  max_gap,
  max_gap_lin,
  time_col,
  .by,
  verbose
) {
  if (verbose) {
    message("Step 2: Applying hierarchical filling...")
  }

  by_cols <- .by %||% character(0)
  specs <- lapply(proxy_cols, function(p) {
    .parse_proxy_spec(p, data, value_col, .by, FALSE)$spec_name
  })

  # Save original order, sort by group + time
  data$.orig_order <- seq_len(nrow(data))
  sort_cols <- unique(c(by_cols, time_col))
  data <- data |>
    dplyr::arrange(dplyr::across(dplyr::all_of(sort_cols)))

  # Compute run structure from raw_missing (static across all levels)
  data <- .fg_add_run_info(data, cols$raw_missing, by_cols)

  # Gap eligibility (run length within max_gap)
  if (is.null(max_gap) || is.infinite(max_gap)) {
    gap_ok <- rep(TRUE, nrow(data))
  } else {
    gap_ok <- !data$.run_is_na | data$.run_len <= max_gap
  }

  for (i in seq_along(proxy_cols)) {
    if (!anyNA(data[[value_col]])) {
      break
    }

    spec_name <- specs[[i]]
    g_col <- paste0("growth_", i, "_", spec_name)
    m_name <- paste0("growth_", spec_name)

    if (verbose) {
      message("  Applying proxy level ", i, ": ", proxy_cols[i])
    }

    # Direction fill: skip internal runs exceeding max_gap_lin
    dir_ok <- gap_ok &
      data$.run_is_na &
      !(data$.run_internal & data$.run_len > max_gap_lin)

    data <- .fg_fill_forward_vec(
      data,
      value_col,
      g_col,
      cols$source,
      m_name,
      dir_ok,
      by_cols
    )
    data <- .fg_fill_backward_vec(
      data,
      value_col,
      g_col,
      cols$source,
      paste0(m_name, "_back"),
      dir_ok,
      by_cols
    )

    # Bridge linear: internal, small gaps
    lin_ok <- gap_ok &
      data$.run_is_na &
      data$.run_internal &
      data$.run_len <= max_gap_lin
    data <- .fg_fill_bridge_linear_vec(
      data,
      value_col,
      cols$source,
      time_col,
      lin_ok,
      by_cols
    )

    # Bridge geometric: internal, larger gaps
    geo_ok <- gap_ok &
      data$.run_is_na &
      data$.run_internal &
      data$.run_len > max_gap_lin
    data <- .fg_fill_bridge_geo_vec(
      data,
      value_col,
      g_col,
      cols$source,
      i,
      specs,
      m_name,
      geo_ok,
      by_cols
    )
  }

  # Clean up and restore original order
  data <- data[order(data$.orig_order), , drop = FALSE]
  data$.run_id <- NULL
  data$.run_len <- NULL
  data$.run_is_na <- NULL
  data$.run_internal <- NULL
  data$.orig_order <- NULL
  data
}

.fg_add_run_info <- function(data, miss_col, by_cols) {
  is_miss <- data[[miss_col]]
  is_miss[is.na(is_miss)] <- TRUE
  data$.is_miss_tmp <- is_miss

  if (length(by_cols) > 0) {
    data <- data |>
      dplyr::mutate(
        .grp_pos = dplyr::row_number(),
        .grp_n = dplyr::n(),
        .run_change = .is_miss_tmp !=
          dplyr::lag(.is_miss_tmp, default = !.is_miss_tmp[1]),
        .by = dplyr::all_of(by_cols)
      )
  } else {
    n <- nrow(data)
    data$.grp_pos <- seq_len(n)
    data$.grp_n <- n
    data$.run_change <- c(TRUE, diff(is_miss) != 0)
  }

  data$.run_id <- cumsum(data$.run_change)

  run_meta <- data |>
    dplyr::summarise(
      .run_len = dplyr::n(),
      .run_is_na = .is_miss_tmp[1],
      .run_internal = .is_miss_tmp[1] &
        .grp_pos[1] > 1L &
        .grp_pos[dplyr::n()] < .grp_n[1],
      .by = dplyr::all_of(".run_id")
    )

  data <- dplyr::left_join(data, run_meta, by = ".run_id")

  data$.is_miss_tmp <- NULL
  data$.run_change <- NULL
  data$.grp_pos <- NULL
  data$.grp_n <- NULL
  data
}

.fg_fill_forward_vec <- function(
  data,
  val_col,
  g_col,
  met_col,
  m_name,
  eligible,
  by_cols
) {
  vals <- data[[val_col]]
  growth <- data[[g_col]]
  mets <- data[[met_col]]

  target <- is.na(vals) & eligible
  if (!any(target)) {
    return(data)
  }

  is_missing <- !is.na(mets) & mets == "missing"

  # Forward anchor: last non-NA value within group
  anchor_raw <- ifelse(is.na(vals), NA_real_, vals)
  if (length(by_cols) > 0) {
    data$.anchor_tmp <- anchor_raw
    data <- data |>
      dplyr::mutate(
        .anchor_tmp = zoo::na.locf(.anchor_tmp, na.rm = FALSE),
        .by = dplyr::all_of(by_cols)
      )
    anchor <- data$.anchor_tmp
    data$.anchor_tmp <- NULL
  } else {
    anchor <- zoo::na.locf(anchor_raw, na.rm = FALSE)
  }

  # Segments: consecutive target positions, respecting run boundaries
  seg_change <- c(TRUE, diff(target) != 0 | diff(data$.run_id) != 0)
  seg_id <- cumsum(seg_change)

  # Growth factor and cumulative product per segment
  g_factor <- ifelse(is.na(growth), NA_real_, 1 + growth)
  filled <- ave(g_factor, seg_id, FUN = cumprod) * anchor

  # Validity: anchor > 0, result finite
  valid <- target &
    !is.na(anchor) &
    anchor > 0 &
    !is.na(filled) &
    is.finite(filled)
  valid <- as.logical(ave(as.numeric(valid), seg_id, FUN = cummin)) & target

  vals[valid] <- filled[valid]
  mets[valid & is_missing] <- m_name

  data[[val_col]] <- vals
  data[[met_col]] <- mets
  data
}

.fg_fill_backward_vec <- function(
  data,
  val_col,
  g_col,
  met_col,
  m_name,
  eligible,
  by_cols
) {
  vals <- data[[val_col]]
  growth <- data[[g_col]]
  mets <- data[[met_col]]

  target <- is.na(vals) & eligible
  if (!any(target)) {
    return(data)
  }

  is_missing <- !is.na(mets) & mets == "missing"

  # Backward anchor: next non-NA value within group
  anchor_raw <- ifelse(is.na(vals), NA_real_, vals)
  if (length(by_cols) > 0) {
    data$.anchor_tmp <- anchor_raw
    data$.g_tmp <- growth
    data <- data |>
      dplyr::mutate(
        .anchor_tmp = zoo::na.locf(.anchor_tmp, na.rm = FALSE, fromLast = TRUE),
        .g_shifted = dplyr::lead(.g_tmp),
        .by = dplyr::all_of(by_cols)
      )
    anchor <- data$.anchor_tmp
    g_shifted <- data$.g_shifted
    data$.anchor_tmp <- NULL
    data$.g_tmp <- NULL
    data$.g_shifted <- NULL
  } else {
    anchor <- zoo::na.locf(anchor_raw, na.rm = FALSE, fromLast = TRUE)
    g_shifted <- c(growth[-1], NA_real_)
  }

  # Segments: consecutive target positions, respecting run boundaries
  seg_change <- c(TRUE, diff(target) != 0 | diff(data$.run_id) != 0)
  seg_id <- cumsum(seg_change)

  # Reverse cumprod within each segment
  g_factor <- ifelse(is.na(g_shifted), NA_real_, 1 + g_shifted)
  rev_cp <- ave(g_factor, seg_id, FUN = function(x) rev(cumprod(rev(x))))
  filled <- anchor / rev_cp

  # Validity: result must be finite and > 0
  valid <- target & !is.na(filled) & is.finite(filled) & filled > 0
  valid <- as.logical(
    ave(as.numeric(valid), seg_id, FUN = function(x) rev(cummin(rev(x))))
  ) &
    target

  vals[valid] <- filled[valid]
  mets[valid & is_missing] <- m_name

  data[[val_col]] <- vals
  data[[met_col]] <- mets
  data
}

.fg_fill_bridge_linear_vec <- function(
  data,
  val_col,
  met_col,
  time_col,
  eligible,
  by_cols
) {
  vals <- data[[val_col]]
  mets <- data[[met_col]]
  if (!any(eligible)) {
    return(data)
  }

  # Anchors: value and time before/after each position
  if (length(by_cols) > 0) {
    data <- data |>
      dplyr::mutate(
        .v_bef = dplyr::lag(.data[[val_col]]),
        .v_aft = dplyr::lead(.data[[val_col]]),
        .t_bef = dplyr::lag(.data[[time_col]]),
        .t_aft = dplyr::lead(.data[[time_col]]),
        .by = dplyr::all_of(by_cols)
      )
  } else {
    n <- length(vals)
    times <- data[[time_col]]
    data$.v_bef <- c(NA_real_, vals[-n])
    data$.v_aft <- c(vals[-1], NA_real_)
    data$.t_bef <- c(NA_real_, times[-n])
    data$.t_aft <- c(times[-1], NA_real_)
  }

  # Run-level anchor summary
  run_anchors <- data |>
    dplyr::filter(eligible) |>
    dplyr::summarise(
      .v0 = dplyr::first(.v_bef),
      .v1 = dplyr::last(.v_aft),
      .t0 = dplyr::first(.t_bef),
      .t1 = dplyr::last(.t_aft),
      .by = dplyr::all_of(".run_id")
    ) |>
    dplyr::filter(is.finite(.v0) & .v0 > 0 & is.finite(.v1) & .v1 > 0)

  if (nrow(run_anchors) > 0) {
    data <- dplyr::left_join(data, run_anchors, by = ".run_id")
    interp <- data$.v0 +
      (data$.v1 - data$.v0) *
        (data[[time_col]] - data$.t0) /
        (data$.t1 - data$.t0)
    fill_mask <- eligible &
      data$.run_id %in% run_anchors$.run_id &
      !is.na(interp)
    vals[fill_mask] <- interp[fill_mask]
    mets[fill_mask] <- "linear_interp"
    data$.v0 <- NULL
    data$.v1 <- NULL
    data$.t0 <- NULL
    data$.t1 <- NULL
  }

  data$.v_bef <- NULL
  data$.v_aft <- NULL
  data$.t_bef <- NULL
  data$.t_aft <- NULL
  data[[val_col]] <- vals
  data[[met_col]] <- mets
  data
}

.fg_fill_bridge_geo_vec <- function(
  data,
  val_col,
  g_col,
  met_col,
  level,
  specs,
  base_method,
  eligible,
  by_cols
) {
  vals <- data[[val_col]]
  mets <- data[[met_col]]
  if (!any(eligible)) {
    return(data)
  }

  # Combine growth rates from hierarchy (best proxy first via coalesce)
  combined <- data[[paste0("growth_", level, "_", specs[[level]])]]
  combined_src <- rep(specs[[level]], nrow(data))
  if (level > 1) {
    for (l in seq_len(level - 1)) {
      g_name <- paste0("growth_", l, "_", specs[[l]])
      if (g_name %in% names(data)) {
        better <- data[[g_name]]
        mask <- !is.na(better)
        combined[mask] <- better[mask]
        combined_src[mask] <- specs[[l]]
      }
    }
  }

  # Anchors and lead growth
  if (length(by_cols) > 0) {
    data$.comb_g <- combined
    data <- data |>
      dplyr::mutate(
        .v_bef = dplyr::lag(.data[[val_col]]),
        .v_aft = dplyr::lead(.data[[val_col]]),
        .g_lead = dplyr::lead(.comb_g),
        .by = dplyr::all_of(by_cols)
      )
    g_lead <- data$.g_lead
    data$.comb_g <- NULL
    data$.g_lead <- NULL
  } else {
    n <- length(vals)
    data$.v_bef <- c(NA_real_, vals[-n])
    data$.v_aft <- c(vals[-1], NA_real_)
    g_lead <- c(combined[-1], NA_real_)
  }

  # Run-level: compute v_start, v_end, product of growth rates, lambda
  data$.comb_g_tmp <- combined
  data$.g_lead_tmp <- g_lead
  run_meta <- data |>
    dplyr::filter(eligible) |>
    dplyr::summarise(
      .v_start = dplyr::first(.v_bef),
      .v_end = dplyr::last(.v_aft),
      .prod_inner = prod(1 + .comb_g_tmp, na.rm = FALSE),
      .g_end = dplyr::last(.g_lead_tmp),
      .rlen = dplyr::n(),
      .by = dplyr::all_of(".run_id")
    ) |>
    dplyr::mutate(
      .prod_total = .prod_inner * (1 + .g_end),
      .n_rates = .rlen + 1L,
      .pred_end = .v_start * .prod_total,
      .lambda = dplyr::if_else(
        is.finite(.pred_end) & .pred_end > 0 & is.finite(.v_end) & .v_end > 0,
        (.v_end / .pred_end)^(1 / .n_rates),
        NA_real_
      )
    ) |>
    dplyr::filter(!is.na(.lambda)) |>
    dplyr::select(dplyr::all_of(".run_id"), .v_start, .lambda)
  data$.comb_g_tmp <- NULL
  data$.g_lead_tmp <- NULL

  if (nrow(run_meta) > 0) {
    data <- dplyr::left_join(data, run_meta, by = ".run_id")
    geo_target <- eligible & !is.na(data$.lambda)

    if (any(geo_target)) {
      adj_factor <- (1 + combined) * data$.lambda
      adj_factor[!geo_target] <- 1
      filled <- ave(adj_factor, data$.run_id, FUN = cumprod) * data$.v_start

      valid <- geo_target & !is.na(filled) & is.finite(filled) & filled > 0
      vals[valid] <- filled[valid]

      src <- combined_src
      src[is.na(src)] <- base_method
      mets[valid] <- paste0("growth_", src[valid], "_bridge")
    }

    data$.v_start <- NULL
    data$.lambda <- NULL
  }

  data$.v_bef <- NULL
  data$.v_aft <- NULL
  data[[val_col]] <- vals
  data[[met_col]] <- mets
  data
}

.fg_finalize_output <- function(data, value_col, cols, mask, format) {
  # Restore non-scope values
  if (!all(mask)) {
    data[[value_col]][!mask] <- cols$orig_val[!mask]
    data[[cols$source]][!mask] <- cols$orig_source[!mask]
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

.compute_ma_base_dplyr <- function(data, value_var, window, group_vars = NULL) {
  if (window <= 1) {
    return(data)
  }

  data |>
    dplyr::mutate(
      ma_base = .compute_ma_vec(.data[[value_var]], window),
      .by = dplyr::all_of(group_vars)
    )
}

# Vectorized backward-looking moving average using cumsum
.compute_ma_vec <- function(vals, w) {
  n <- length(vals)
  if (n < 2L) {
    return(rep(NA_real_, n))
  }

  not_na <- !is.na(vals)
  vals_0 <- ifelse(not_na, vals, 0)
  cs <- cumsum(vals_0)
  cc <- cumsum(as.numeric(not_na))

  # At position i (2:n), mean of non-NA vals in [max(1, i-w) : (i-1)]
  i <- 2:n
  end_idx <- i - 1L
  start_idx <- pmax(1L, i - w)
  s <- cs[end_idx] - ifelse(start_idx > 1L, cs[start_idx - 1L], 0)
  cnt <- cc[end_idx] - ifelse(start_idx > 1L, cc[start_idx - 1L], 0)

  result <- rep(NA_real_, n)
  valid <- cnt > 0
  result[i[valid]] <- s[valid] / cnt[valid]
  result
}

.safe_na_approx <- function(object, x, ...) {
  valid <- !is.na(object)
  if (length(unique(x[valid])) < 2) {
    return(NA_real_)
  }
  zoo::na.approx(object, x = x, ...)
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
