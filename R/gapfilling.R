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

  # Convert once and reuse for both duplicate check and main work
  is_dt <- data.table::is.data.table(data)
  dt_work <- if (is_dt) {
    data.table::copy(data)
  } else {
    data.table::as.data.table(data)
  }

  # Fast duplicate check
  dup_cols <- c(by_cols, time_col_name)
  if (anyDuplicated(dt_work[, ..dup_cols]) > 0L) {
    dups <- dt_work[, .(n = .N), by = dup_cols][n > 1]
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

  # Grouped path: fully vectorized using nafill + boundary correction.
  # Avoids any per-group R expression evaluation (2M+ groups).
  # ~3x faster than data.table by= with R body.
  use_smoothing <- !is.null(value_smooth_window)

  if (use_smoothing) {
    # Smoothing requires per-group evaluation; fall back to by= path.
    return(.fill_linear_by_group(
      dt_work,
      value_col_name,
      time_col_name,
      source_col_name,
      by_cols,
      interpolate,
      fill_forward,
      fill_backward,
      value_smooth_window,
      is_dt
    ))
  }

  dt <- dt_work
  sort_cols <- c(by_cols, time_col_name)
  prev_sorted <- attr(data, ".whep_sorted_by")
  already_sorted <- identical(data.table::key(dt), sort_cols) ||
    identical(prev_sorted, sort_cols)
  if (!already_sorted) {
    data.table::setkeyv(dt, sort_cols)
  } else if (is.null(data.table::key(dt))) {
    data.table::setattr(dt, "sorted", sort_cols)
  }

  val <- dt[[value_col_name]]
  tm <- as.numeric(dt[[time_col_name]])
  nn <- length(val)
  is_na <- is.na(val)

  # Group boundaries via rleid on the by columns
  grp <- data.table::rleidv(dt, cols = by_cols)

  # Global nafill — crosses group boundaries (fixed below)
  locf_raw <- data.table::nafill(val, "locf")
  nocb_raw <- data.table::nafill(val, "nocb")

  # Track which non-NA each fill came from via index-nafill
  valid_idx <- data.table::fifelse(!is_na, seq_len(nn), NA_integer_)
  locf_idx <- data.table::nafill(valid_idx, "locf")
  nocb_idx <- data.table::nafill(valid_idx, "nocb")

  # Discard fills that crossed a group boundary
  locf_ok <- !is.na(locf_idx) & grp[locf_idx] == grp
  nocb_ok <- !is.na(nocb_idx) & grp[nocb_idx] == grp

  locf <- data.table::fifelse(locf_ok, locf_raw, NA_real_)
  nocb <- data.table::fifelse(nocb_ok, nocb_raw, NA_real_)

  # Fill result + source
  filled <- val
  source <- data.table::fifelse(is_na, "Gap not filled", "Original")

  # Interpolation (NAs with valid values both before and after in group)
  if (interpolate) {
    time_valid <- data.table::fifelse(!is_na, tm, NA_real_)
    tl <- data.table::fifelse(
      locf_ok,
      data.table::nafill(time_valid, "locf"),
      NA_real_
    )
    tn <- data.table::fifelse(
      nocb_ok,
      data.table::nafill(time_valid, "nocb"),
      NA_real_
    )
    denom <- tn - tl
    frac <- data.table::fifelse(
      !is.na(denom) & denom != 0,
      (tm - tl) / denom,
      NA_real_
    )
    interp <- locf + (nocb - locf) * frac
    mask <- is_na & !is.na(interp) & locf_ok & nocb_ok
    filled[mask] <- interp[mask]
    source[mask] <- "Linear interpolation"
  }

  # Carry backward (NAs before first valid in group)
  if (fill_backward) {
    mask <- is_na & !locf_ok & nocb_ok
    filled[mask] <- nocb[mask]
    source[mask] <- "First value carried backwards"
  }

  # Carry forward (NAs after last valid in group)
  if (fill_forward) {
    mask <- is_na & locf_ok & !nocb_ok
    filled[mask] <- locf[mask]
    source[mask] <- "Last value carried forward"
  }

  data.table::set(dt, j = value_col_name, value = filled)
  data.table::set(dt, j = source_col_name, value = source)
  data.table::setcolorder(dt, names(data))
  if (!is_dt) {
    dt <- tibble::as_tibble(dt)
  }
  attr(dt, ".whep_sorted_by") <- sort_cols
  dt
}

# Fallback grouped path for smoothing case (rare).
.fill_linear_by_group <- function(
  data,
  value_col_name,
  time_col_name,
  source_col_name,
  by_cols,
  interpolate,
  fill_forward,
  fill_backward,
  value_smooth_window,
  is_dt
) {
  dt <- data
  sort_cols <- c(by_cols, time_col_name)
  if (!identical(data.table::key(dt), sort_cols)) {
    data.table::setkeyv(dt, sort_cols)
  }

  dt[,
    c(value_col_name, source_col_name) := {
      v <- .SD[[1L]]
      tm <- .SD[[2L]]
      m <- .N
      src <- rep("Original", m)
      filled <- v
      nna <- is.na(v)

      if (any(nna) && !all(nna)) {
        sv <- zoo::rollmean(
          v,
          k = value_smooth_window,
          fill = NA,
          align = "center"
        )
        valid <- which(!is.na(sv))
        first_v <- valid[1L]
        last_v <- valid[length(valid)]

        if (interpolate && length(valid) >= 2L && first_v < last_v) {
          interp <- .safe_na_approx(sv, x = tm, na.rm = FALSE)
          if (length(interp) == m) {
            mask <- nna &
              !is.na(interp) &
              seq_len(m) > first_v &
              seq_len(m) < last_v
            filled[mask] <- interp[mask]
            src[mask] <- "Linear interpolation"
          }
        }
        if (fill_backward && first_v > 1L) {
          mask <- nna & seq_len(m) < first_v
          filled[mask] <- sv[first_v]
          src[mask] <- "First value carried backwards"
        }
        if (fill_forward && last_v < m) {
          mask <- nna & seq_len(m) > last_v
          filled[mask] <- sv[last_v]
          src[mask] <- "Last value carried forward"
        }
        src[is.na(filled)] <- "Gap not filled"
      } else if (all(nna)) {
        src[] <- "Gap not filled"
      }
      list(filled, src)
    },
    by = by_cols,
    .SDcols = c(value_col_name, time_col_name)
  ]

  data.table::setcolorder(dt, names(data))
  if (!is_dt) {
    dt <- tibble::as_tibble(dt)
  }
  attr(dt, ".whep_sorted_by") <- c(by_cols, time_col_name)
  dt
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
  change_col_name <- rlang::as_name(rlang::enquo(change_col))
  source_col_name <- paste0("source_", value_col_name)

  dt <- data.table::as.data.table(data)
  sort_cols <- c(.by, time_col_name)
  data.table::setorderv(dt, sort_cols)

  by_cols <- .by

  # Compute within groups (or globally if no groups)
  val <- as.double(dt[[value_col_name]])
  chg <- as.double(dt[[change_col_name]])

  if (length(by_cols) > 0) {
    # Grouped computation
    dt[,
      c(
        value_col_name,
        source_col_name
      ) := {
        v <- as.double(get(value_col_name))
        ch <- as.double(get(change_col_name))
        groups <- cumsum(!is.na(v))
        prefilled <- data.table::fifelse(is.na(v), ch, v)
        src <- data.table::fifelse(is.na(v), "Filled with sum", "Original")
        filled <- ave(prefilled, groups, FUN = cumsum)
        if (!start_with_zero) {
          filled <- data.table::fifelse(groups == 0L, NA_real_, filled)
        }
        src <- data.table::fifelse(is.na(filled), NA_character_, src)
        list(filled, src)
      },
      by = by_cols
    ]
  } else {
    # No grouping
    groups <- cumsum(!is.na(val))
    prefilled <- data.table::fifelse(is.na(val), chg, val)
    src <- data.table::fifelse(is.na(val), "Filled with sum", "Original")
    filled <- ave(prefilled, groups, FUN = cumsum)
    if (!start_with_zero) {
      filled <- data.table::fifelse(groups == 0L, NA_real_, filled)
    }
    src <- data.table::fifelse(is.na(filled), NA_character_, src)
    data.table::set(dt, j = value_col_name, value = filled)
    data.table::set(dt, j = source_col_name, value = src)
  }

  tibble::as_tibble(dt)
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
  dt <- data.table::as.data.table(data)
  scope_mask <- dt[, eval(rlang::quo_get_expr(fill_scope_expr), .SD)]
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
    dt <- data.table::as.data.table(data)
    if (length(.by) > 0) {
      dt[,
        .smooth_var := zoo::rollmean(
          original_value_numeric,
          k = value_smooth_window,
          fill = NA,
          align = "center"
        ),
        by = .by
      ]
    } else {
      dt[,
        .smooth_var := zoo::rollmean(
          original_value_numeric,
          k = value_smooth_window,
          fill = NA,
          align = "center"
        )
      ]
    }
    data <- as.data.frame(dt)
  } else {
    data$.smooth_var <- original_value_numeric
  }

  # Add working columns
  # raw_missing tracks what was ORIGINALLY missing (before smoothing)
  # value_col uses smoothed values for gap-filling, but coalesces with original
  data[[raw_col]] <- data[[value_col]]
  data[[raw_numeric_col]] <- data$.smooth_var
  data[[raw_missing_col]] <- is.na(original_value_numeric)
  data$.smooth_var <- NULL

  if (has_source_col) {
    # source_col already exists, keep it
  } else {
    data[[source_col]] <- ifelse(
      data[[raw_missing_col]],
      "missing",
      "original"
    )
  }
  # Use original where available, smoothed otherwise (for anchor points)
  data[[value_col]] <- data.table::fifelse(
    !is.na(original_value_numeric),
    original_value_numeric,
    data[[raw_numeric_col]]
  )
  data[[source_col]][is.na(data[[value_col]])] <- "missing"

  list(
    data_work = data,
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

  # 1. Prepare data with relevant columns
  prep <- .fg_growth_prep(data, spec, .by, time_col)
  if (nrow(prep) == 0) {
    return(.fg_add_empty_cols(data, growth_col, obs_col))
  }

  # 2. Compute Individual Row Growth (with Smoothing)
  prep <- .fg_growth_calc_individual(prep, spec, smooth_window, time_col)

  # 3. Aggregate to Groups
  summary_dt <- .fg_growth_aggregate(
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

  dt_data <- data.table::as.data.table(data)
  dt_summary <- data.table::as.data.table(summary_dt)
  result <- merge(dt_data, dt_summary, by = join_keys, all.x = TRUE)
  as.data.frame(result)
}

.fg_growth_prep <- function(data, spec, .by, time_col) {
  if (!spec$source_var %in% names(data)) {
    return(data.frame())
  }

  lag_vars <- unique(c(.by, spec$present_group_vars))
  lag_vars <- lag_vars[lag_vars %in% names(data)]

  cols <- unique(c(time_col, lag_vars, spec$source_var, spec$weight_col))
  cols <- cols[!is.na(cols)]

  if (data.table::is.data.table(data)) {
    dt <- data[, ..cols]
  } else {
    dt <- data.table::as.data.table(data[, cols, drop = FALSE])
  }
  sort_cols <- unique(c(lag_vars, time_col))
  data.table::setorderv(dt, sort_cols)
  as.data.frame(dt)
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
    data <- .compute_ma_base_dt(data, spec$source_var, window, by_vars)
    val_col <- "ma_base"
  } else {
    val_col <- spec$source_var
  }

  # Create Lags and Calculate Growth
  dt <- data.table::as.data.table(data)
  if (length(by_vars) > 0) {
    dt[,
      `:=`(
        lag_src = data.table::shift(get(val_col), 1L, type = "lag"),
        lag_yr = data.table::shift(get(time_col), 1L, type = "lag")
      ),
      by = by_vars
    ]
  } else {
    dt[, `:=`(
      lag_src = data.table::shift(get(val_col), 1L, type = "lag"),
      lag_yr = data.table::shift(get(time_col), 1L, type = "lag")
    )]
  }
  dt[,
    ind_growth := data.table::fifelse(
      get(time_col) == lag_yr + 1 &
        !is.na(get(val_col)) &
        !is.na(lag_src) &
        lag_src > 0,
      (get(val_col) - lag_src) / lag_src,
      NA_real_
    )
  ]

  # Filter to non-NA growth
  dt <- dt[!is.na(ind_growth)]
  as.data.frame(dt)
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

  dt <- data.table::as.data.table(data)

  if (has_w) {
    # Shift weights to align with growth period (previous year weight)
    grp <- if (length(spec$present_group_vars) > 0) {
      spec$present_group_vars
    } else {
      NULL
    }

    if (length(grp) > 0) {
      dt[,
        w := data.table::shift(get(spec$weight_col), 1L, type = "lag"),
        by = grp
      ]
    } else {
      dt[, w := data.table::shift(get(spec$weight_col), 1L, type = "lag")]
    }

    # Weighted aggregation
    result <- dt[,
      {
        valid_w <- !is.na(w) & is.finite(w) & w > 0
        g_val <- if (any(valid_w)) {
          sum(ind_growth[valid_w] * w[valid_w]) / sum(w[valid_w])
        } else {
          mean(ind_growth)
        }
        o_val <- if (any(valid_w)) sum(valid_w) else .N
        list(g_val, o_val)
      },
      by = by_vars
    ]
    data.table::setnames(result, c("V1", "V2"), c(growth_col, obs_col))
  } else {
    # Unweighted aggregation
    result <- dt[,
      .(
        g_val = mean(ind_growth),
        o_val = .N
      ),
      by = by_vars
    ]
    data.table::setnames(result, c("g_val", "o_val"), c(growth_col, obs_col))
  }

  as.data.frame(result)
}

.fg_add_empty_cols <- function(data, g_col, o_col) {
  n <- nrow(data)
  data[[g_col]] <- rep(NA_real_, n)
  data[[o_col]] <- rep(0L, n)
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
  dt <- data.table::as.data.table(data)
  data.table::setorderv(dt, sort_cols)
  data <- as.data.frame(dt)

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
    dt <- data.table::as.data.table(data)
    dt[,
      `:=`(
        .grp_pos = seq_len(.N),
        .grp_n = .N,
        .run_change = .is_miss_tmp !=
          data.table::shift(
            .is_miss_tmp,
            1L,
            type = "lag",
            fill = !.is_miss_tmp[1L]
          )
      ),
      by = by_cols
    ]
    data <- as.data.frame(dt)
  } else {
    n <- nrow(data)
    data$.grp_pos <- seq_len(n)
    data$.grp_n <- n
    data$.run_change <- c(TRUE, diff(is_miss) != 0)
  }

  data$.run_id <- cumsum(data$.run_change)

  dt <- data.table::as.data.table(data)
  run_meta <- dt[,
    .(
      .run_len = .N,
      .run_is_na = .is_miss_tmp[1L],
      .run_internal = .is_miss_tmp[1L] &
        .grp_pos[1L] > 1L &
        .grp_pos[.N] < .grp_n[1L]
    ),
    by = .run_id
  ]

  data <- as.data.frame(
    merge(dt, run_meta, by = ".run_id", all.x = TRUE)
  )

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

  # Forward anchor: last non-NA value within group (vectorized LOCF)
  anchor_raw <- data.table::fifelse(is.na(vals), NA_real_, vals)
  locf_raw <- data.table::nafill(anchor_raw, "locf")
  if (length(by_cols) > 0) {
    nn <- length(anchor_raw)
    grp <- data.table::rleidv(
      data.table::as.data.table(data),
      cols = by_cols
    )
    valid_idx <- data.table::fifelse(
      !is.na(anchor_raw),
      seq_len(nn),
      NA_integer_
    )
    locf_idx <- data.table::nafill(valid_idx, "locf")
    locf_ok <- !is.na(locf_idx) & grp[locf_idx] == grp
    anchor <- data.table::fifelse(locf_ok, locf_raw, NA_real_)
  } else {
    anchor <- locf_raw
  }

  # Segments: consecutive target positions, respecting run boundaries
  seg_change <- c(TRUE, diff(target) != 0 | diff(data$.run_id) != 0)
  seg_id <- cumsum(seg_change)

  # Growth factor and cumulative product per segment
  g_factor <- data.table::fifelse(is.na(growth), NA_real_, 1 + growth)
  filled <- .seg_cumprod(g_factor, seg_id) * anchor

  # Validity: anchor > 0, result finite
  valid <- target &
    !is.na(anchor) &
    anchor > 0 &
    !is.na(filled) &
    is.finite(filled)
  valid <- as.logical(.seg_cummin(as.numeric(valid), seg_id)) & target

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

  # Backward anchor: next non-NA value within group (vectorized NOCB)
  anchor_raw <- data.table::fifelse(is.na(vals), NA_real_, vals)
  nocb_raw <- data.table::nafill(anchor_raw, "nocb")
  nn <- length(anchor_raw)
  if (length(by_cols) > 0) {
    grp <- data.table::rleidv(
      data.table::as.data.table(data),
      cols = by_cols
    )
    valid_idx <- data.table::fifelse(
      !is.na(anchor_raw),
      seq_len(nn),
      NA_integer_
    )
    nocb_idx <- data.table::nafill(valid_idx, "nocb")
    nocb_ok <- !is.na(nocb_idx) & grp[nocb_idx] == grp
    anchor <- data.table::fifelse(nocb_ok, nocb_raw, NA_real_)
    # Shift growth forward within groups
    g_shifted <- c(growth[-1L], NA_real_)
    first_of_grp <- c(FALSE, diff(grp) != 0L)
    # Last element of each group has no successor
    last_of_grp <- c(diff(grp) != 0L, TRUE)
    g_shifted[last_of_grp] <- NA_real_
  } else {
    anchor <- nocb_raw
    g_shifted <- c(growth[-1L], NA_real_)
  }

  # Segments: consecutive target positions, respecting run boundaries
  seg_change <- c(TRUE, diff(target) != 0 | diff(data$.run_id) != 0)
  seg_id <- cumsum(seg_change)

  # Reverse cumprod within each segment
  g_factor <- data.table::fifelse(is.na(g_shifted), NA_real_, 1 + g_shifted)
  rev_cp <- .seg_rev_cumprod(g_factor, seg_id)
  filled <- anchor / rev_cp

  # Validity: result must be finite and > 0
  valid <- target & !is.na(filled) & is.finite(filled) & filled > 0
  valid <- as.logical(.seg_rev_cummin(as.numeric(valid), seg_id)) &
    target

  vals[valid] <- filled[valid]
  mets[valid & is_missing] <- m_name

  data[[val_col]] <- vals
  data[[met_col]] <- mets
  data
}

# Segmented cumprod using data.table by= (avoids ave() overhead).
.seg_cumprod <- function(x, seg_id) {
  dt <- data.table::data.table(x = x, seg = seg_id)
  dt[, cp := cumprod(data.table::fifelse(is.na(x), 1, x)), by = seg]
  dt[is.na(x), cp := NA_real_]
  dt$cp
}

# Reverse segmented cumprod.
.seg_rev_cumprod <- function(x, seg_id) {
  nn <- length(x)
  rev_idx <- nn:1L
  .seg_cumprod(x[rev_idx], seg_id[rev_idx])[rev_idx]
}

# Segmented cummin using cumsum-with-resets (fully vectorized).
# For binary 0/1 input: 1 until first 0 in segment, then 0.
.seg_cummin <- function(x, seg_id) {
  seg_start <- c(TRUE, diff(seg_id) != 0L)
  bad <- 1 - x
  global_cs <- cumsum(bad)
  base <- data.table::fifelse(seg_start, global_cs - bad, NA_real_)
  base <- data.table::nafill(base, "locf")
  as.numeric((global_cs - base) == 0)
}

# Reverse segmented cummin.
.seg_rev_cummin <- function(x, seg_id) {
  nn <- length(x)
  rev_idx <- nn:1L
  .seg_cummin(x[rev_idx], seg_id[rev_idx])[rev_idx]
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
    dt <- data.table::as.data.table(data)
    dt[,
      `:=`(
        .v_bef = data.table::shift(get(val_col), 1L, type = "lag"),
        .v_aft = data.table::shift(get(val_col), 1L, type = "lead"),
        .t_bef = data.table::shift(get(time_col), 1L, type = "lag"),
        .t_aft = data.table::shift(get(time_col), 1L, type = "lead")
      ),
      by = by_cols
    ]
    data <- as.data.frame(dt)
  } else {
    n <- length(vals)
    times <- data[[time_col]]
    data$.v_bef <- c(NA_real_, vals[-n])
    data$.v_aft <- c(vals[-1], NA_real_)
    data$.t_bef <- c(NA_real_, times[-n])
    data$.t_aft <- c(times[-1], NA_real_)
  }

  # Run-level anchor summary
  dt_elig <- data.table::as.data.table(data[eligible, , drop = FALSE])
  run_anchors <- dt_elig[,
    .(
      .v0 = .v_bef[1L],
      .v1 = .v_aft[.N],
      .t0 = .t_bef[1L],
      .t1 = .t_aft[.N]
    ),
    by = .run_id
  ]
  run_anchors <- run_anchors[
    is.finite(.v0) & .v0 > 0 & is.finite(.v1) & .v1 > 0
  ]

  if (nrow(run_anchors) > 0) {
    dt_all <- data.table::as.data.table(data)
    dt_all <- merge(dt_all, run_anchors, by = ".run_id", all.x = TRUE)
    data <- as.data.frame(dt_all)
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
    dt <- data.table::as.data.table(data)
    dt[,
      `:=`(
        .v_bef = data.table::shift(get(val_col), 1L, type = "lag"),
        .v_aft = data.table::shift(get(val_col), 1L, type = "lead"),
        .g_lead = data.table::shift(.comb_g, 1L, type = "lead")
      ),
      by = by_cols
    ]
    data <- as.data.frame(dt)
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
  dt_elig <- data.table::as.data.table(data[eligible, , drop = FALSE])
  run_meta <- dt_elig[,
    .(
      .v_start = .v_bef[1L],
      .v_end = .v_aft[.N],
      .prod_inner = prod(1 + .comb_g_tmp, na.rm = FALSE),
      .g_end = .g_lead_tmp[.N],
      .rlen = .N
    ),
    by = .run_id
  ]
  run_meta[, `:=`(
    .prod_total = .prod_inner * (1 + .g_end),
    .n_rates = .rlen + 1L
  )]
  run_meta[, .pred_end := .v_start * .prod_total]
  run_meta[,
    .lambda := data.table::fifelse(
      is.finite(.pred_end) & .pred_end > 0 & is.finite(.v_end) & .v_end > 0,
      (.v_end / .pred_end)^(1 / .n_rates),
      NA_real_
    )
  ]
  run_meta <- run_meta[!is.na(.lambda)]
  run_meta <- run_meta[, .(.run_id, .v_start, .lambda)]
  data$.comb_g_tmp <- NULL
  data$.g_lead_tmp <- NULL

  if (nrow(run_meta) > 0) {
    dt_all <- data.table::as.data.table(data)
    dt_all <- merge(
      dt_all,
      run_meta,
      by = ".run_id",
      all.x = TRUE
    )
    data <- as.data.frame(dt_all)
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
    drop_cols <- grep(ptn, names(data), value = TRUE)
    if (length(drop_cols) > 0) {
      data[drop_cols] <- NULL
    }
  } else {
    # Detailed: keep debug cols but clean raw numeric
    drop_cols <- intersect(
      c(cols$raw_numeric, cols$raw_missing),
      names(data)
    )
    if (length(drop_cols) > 0) {
      data[drop_cols] <- NULL
    }
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

.compute_ma_base_dt <- function(data, value_var, window, group_vars = NULL) {
  if (window <= 1) {
    return(data)
  }

  dt <- data.table::as.data.table(data)
  if (length(group_vars) > 0) {
    dt[, ma_base := .compute_ma_vec(get(value_var), window), by = group_vars]
  } else {
    dt[, ma_base := .compute_ma_vec(get(value_var), window)]
  }
  as.data.frame(dt)
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
