#' @title Calculate LMDI decomposition
#'
#' @description
#'   Performs LMDI (Log Mean Divisia Index) decomposition analysis with
#'   flexible identity parsing, automatic factor detection, and support for
#'   multiple periods and groupings. Supports sectoral decomposition using
#'   bracket notation for both summing and grouping operations.
#'
#' @details
#'   The LMDI method decomposes changes in a target variable into contributions
#'   from multiple factors using logarithmic mean weights. This implementation
#'   supports:
#'
#'   **Flexible identity specification:**
#'   - Automatic factor detection from identity string.
#'   - Support for ratio calculations (implicit division).
#'   - Sectoral aggregation with `[]` notation.
#'   - Sectoral grouping with `{}` notation.
#'
#'   **Period analysis:**
#'   The function can decompose changes over single or multiple periods.
#'   Periods are defined by consecutive pairs in the `periods` vector.
#'
#'   **Grouping capabilities:**
#'   Use `analysis_by` to perform separate decompositions for different
#'   groups (e.g., countries, regions) while maintaining consistent factor
#'   structure.
#'
#' @section Identity Syntax:
#'   The identity parameter uses a special syntax to define decomposition:
#'
#'   **Basic format:** `"target:factor1*factor2*factor3"`
#'
#'   **Simple decomposition (no sectors):**
#'   - Basic: `"emissions:gdp*(emissions/gdp)"`
#'   - Complete: `"emissions:(emissions/gdp)*(gdp/population)*population"`
#'
#'   **Understanding bracket notation:**
#'
#'   Square brackets `[]` specify variables to sum across categories, enabling
#'   structural decomposition. The bracket aggregates values BEFORE calculating
#'   ratios.
#'
#'   **Single-level structural decomposition:**
#'   - `"emissions:activity*(activity[sector]/activity)*(emissions[sector]/activity[sector])"`
#'   - Creates 3 factors: Activity level, Sectoral structure, Sectoral
#'     intensity.
#'
#'   **Multi-level structural decomposition:**
#'   - Two levels: `"emissions:activity*(activity[sector]/activity)*(activity[sector+fuel]/activity[sector])*(emissions[sector+fuel]/activity[sector+fuel])"`
#'   - Creates 4 factors: Activity level, Sector structure, Fuel structure,
#'     Sectoral-fuel intensity.
#'
#' @section Data Requirements:
#'   The input data frame must contain:
#'   - All variables mentioned in the identity.
#'   - The time variable (default: "year").
#'   - Grouping variables if using `analysis_by`.
#'   - No missing values in key variables for decomposition periods.
#'
#' @param data A data frame containing the variables for decomposition. Must
#'   include all variables specified in the identity, time variable, and any
#'   grouping variables.
#' @param identity Character. Decomposition identity in format
#'   `"target:factor1*factor2*..."`. The target appears before the colon,
#'   factors after, separated by asterisks. Supports explicit ratios with
#'   `/` and structural decomposition with `[]`.
#' @param identity_labels Named character vector. Custom labels for factors
#'   to use in output instead of variable names. Default: NULL uses variable
#'   names as-is.
#' @param time_var Character. Name of the time variable column in the data.
#'   Default: "year". Must be numeric or coercible to numeric.
#' @param periods Numeric vector. Years defining analysis periods. Each
#'   consecutive pair defines one period. Default: NULL uses all available
#'   years.
#' @param periods_2 Numeric vector. Additional period specification for
#'   complex multi-period analyses. Default: NULL.
#' @param analysis_by Character vector. Grouping variables for performing
#'   separate decompositions. Default: NULL (single decomposition for all
#'   data).
#' @param rolling_mean Numeric. Window size for rolling mean smoothing
#'   applied before decomposition. Default: NULL (no smoothing).
#' @param output_format Character. Format of output data frame. Options:
#'   `"clean"` (default) or `"detailed"`.
#' @param verbose Logical. If TRUE (default), prints progress messages during
#'   decomposition.
#'
#' @return
#'   A data frame with LMDI decomposition results containing:
#'   - Time variables and grouping variables (if specified).
#'   - `additive`: Additive contributions (sum equals total change in target).
#'   - `multiplicative`: Multiplicative indices (product equals target ratio).
#'   - `multiplicative_log`: Log of multiplicative indices.
#'   - Period identifiers and metadata.
#'
#' @export
#'
#' @examples
#' # Simple LMDI decomposition
#' data <- data.frame(
#'   year = rep(2010:2015, 2),
#'   country = rep(c("ESP", "FRA"), each = 6),
#'   emissions = c(100, 105, 110, 115, 120, 125, 200, 210, 220, 230, 240, 250),
#'   gdp = c(
#'     1000, 1050, 1100, 1150, 1200, 1250,
#'     2000, 2100, 2200, 2300, 2400, 2500
#'   ),
#'   population = c(46, 46.5, 47, 47.5, 48, 48.5, 65, 65.5, 66, 66.5, 67, 67.5)
#' )
#'
#' # Example 1: Simple decomposition by country
#' lmdi_basic <- calculate_lmdi(
#'   data,
#'   identity = "emissions:gdp*(emissions/gdp)",
#'   time_var = "year",
#'   analysis_by = "country"
#' )
#'
#' # Example 2: Complete form with multiple factors
#' lmdi_complete <- calculate_lmdi(
#'   data,
#'   identity = "emissions:(emissions/gdp)*(gdp/population)*population",
#'   time_var = "year",
#'   analysis_by = "country"
#' )
#'
#' @family calculations
calculate_lmdi <- function(
  data,
  identity,
  identity_labels = NULL,
  time_var = "year",
  periods = NULL,
  periods_2 = NULL,
  analysis_by = NULL,
  rolling_mean = NULL,
  output_format = "clean",
  verbose = TRUE
) {
  id <- .parse_identity(identity)
  target_var <- id$target
  factors <- id$factors

  data <- .lmdi_prepare_rolling_mean(data, identity, target_var, time_var, rolling_mean, verbose)

  labels <- .lmdi_handle_identity_labels(identity_labels, factors, target_var)
  target_label_final <- labels$target_label_final
  factor_labels <- labels$factor_labels

  selectors_detected <- .extract_selectors(identity)
  group_vars <- NULL
  if (length(selectors_detected) > 0) {
    group_vars <- selectors_detected
    if (verbose) {
      message(sprintf(
        "Auto-detected selectors: %s",
        paste(selectors_detected, collapse = ", ")
      ))
    }
  }

  periods <- .lmdi_setup_periods(data, time_var, periods, periods_2)

  if (!is.null(group_vars)) {
    if (!all(group_vars %in% names(data))) {
      cli::cli_abort("All group_vars must exist in the data")
    }
  }
  if (!is.null(analysis_by)) {
    analysis_by <- unique(analysis_by)
    if (!all(analysis_by %in% names(data))) {
      cli::cli_abort("All columns in analysis_by must exist in the data")
    }
  }

  output_format <- match.arg(tolower(output_format), c("clean", "total"))
  analysis_cols <- if (is.null(analysis_by)) character(0) else analysis_by
  analysis_groups <- if (length(analysis_cols) == 0) {
    tibble::tibble(.analysis_id = 1)
  } else {
    data |> dplyr::distinct(dplyr::across(dplyr::all_of(analysis_cols)))
  }

  tolerance_add <- 1e-6
  tolerance_mult <- 1e-6
  results_all <- list()
  period_targets_all <- list()

  for (idx in seq_len(nrow(analysis_groups))) {
    if (length(analysis_cols) == 0) {
      subset_data <- data
      analysis_values <- NULL
    } else {
      analysis_values <- as.list(analysis_groups[idx, , drop = FALSE])
      subset_data <- data
      for (nm in names(analysis_values)) {
        subset_data <- subset_data |>
          dplyr::filter(.data[[nm]] == analysis_values[[nm]])
      }
      if (nrow(subset_data) == 0) {
        next
      }
    }
    period_results <- list()
    period_targets <- list()
    for (i in seq_len(nrow(periods))) {
      period_out <- .lmdi_calc_period(
        subset_data, periods, i, time_var, group_vars, target_var, factors,
        factor_labels, target_label_final, identity, analysis_cols, analysis_values
      )
      if (is.null(period_out)) next
      period_results[[length(period_results) + 1]] <- period_out$result
      period_targets[[length(period_targets) + 1]] <- period_out$target
    }
    if (length(period_results) == 0) next
    analysis_result <- dplyr::bind_rows(period_results)
    analysis_period_targets <- dplyr::bind_rows(period_targets)
    .lmdi_closure_check(analysis_result, analysis_cols, tolerance_add, tolerance_mult)
    results_all[[length(results_all) + 1]] <- analysis_result
    period_targets_all[[length(period_targets_all) + 1]] <- analysis_period_targets
  }

  if (length(results_all) == 0) {
    if (verbose) message("No results produced.")
    return(tibble::tibble())
  }
  out <- dplyr::bind_rows(results_all)
  period_targets_df <- dplyr::bind_rows(period_targets_all)
  if (output_format == "clean") {
    select_cols <- c(
      analysis_cols,
      "period",
      "period_years",
      "factor_label",
      "component_type",
      "identity",
      "identity_var",
      "additive",
      "multiplicative",
      "multiplicative_log"
    )
    select_cols <- select_cols[select_cols %in% names(out)]
    out <- out |> dplyr::select(dplyr::all_of(select_cols))
  }
  attributes_list <- list(
    identity = identity,
    identity_labels = identity_labels,
    identity_var = target_var,
    period_targets = period_targets_df,
    periods = periods,
    analysis_by = analysis_cols,
    rolling_mean = rolling_mean
  )
  for (nm in names(attributes_list)) {
    attr(out, nm) <- attributes_list[[nm]]
  }
  if (verbose) message("LMDI calculation complete.")
  out
}

.lmdi_prepare_rolling_mean <- function(data, identity, target_var, time_var, rolling_mean, verbose) {
  if (is.null(rolling_mean)) return(data)
  if (!is.numeric(rolling_mean) || rolling_mean < 2) {
    cli::cli_abort(
      "rolling_mean must be a numeric value >= 2 (number of years for centered moving average)"
    )
  }
  all_vars <- unique(c(
    target_var,
    stringr::str_extract_all(identity, "[a-zA-Z0-9_]+")[[1]]
  ))
  numeric_vars <- all_vars[all_vars %in% names(data)]
  numeric_vars <- numeric_vars[sapply(data[numeric_vars], is.numeric)]
  group_cols <- setdiff(names(data), c(time_var, numeric_vars))
  if (verbose) {
    message("\n--- LMDI Data Preparation ---")
    message("Step 1: Panel balancing")
  }
  n_before <- nrow(data)
  if (length(group_cols) > 0) {
    data <- data |>
      tidyr::complete(
        !!!rlang::syms(c(time_var, group_cols)),
        fill = as.list(
          stats::setNames(rep(NA_real_, length(numeric_vars)), numeric_vars)
        )
      )
  }
  n_after <- nrow(data)
  n_added <- n_after - n_before
  if (verbose && n_added > 0) {
    message(sprintf("  - Original: %d rows", n_before))
    message(sprintf(
      "  - Balanced: %d rows (+%d rows added)",
      n_after,
      n_added
    ))
  }
  if (verbose) {
    message("\nStep 2: Zero/NA treatment (Ang, 2015 methodology)")
  }
  epsilon <- 1e-10
  data <- data |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(numeric_vars),
        ~ dplyr::if_else(is.na(.x) | .x == 0, epsilon, .x)
      )
    )
  if (verbose) {
    message(sprintf("  - Replacement value: %.2e (epsilon)", epsilon))
    message("-----------------------------\n")
  }
  years_per_group <- if (length(group_cols) > 0) {
    data |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
      dplyr::summarise(
        n_years = dplyr::n_distinct(.data[[time_var]]),
        .groups = "drop"
      )
  } else {
    tibble::tibble(n_years = dplyr::n_distinct(data[[time_var]]))
  }
  min_years <- min(years_per_group$n_years)
  k_orig <- as.integer(rolling_mean)
  k_eff <- k_orig
  if (min_years < k_orig) {
    k_eff <- max(
      3L,
      min(min_years, ifelse(k_orig %% 2 == 1, k_orig, k_orig - 1))
    )
    if (k_eff > min_years) {
      k_eff <- max(3L, min_years - (1L - (min_years %% 2)))
    }
    if (verbose) {
      message(sprintf(
        "rolling_mean=%d larger than available years (min=%d). Using k=%d.",
        k_orig,
        min_years,
        k_eff
      ))
    }
  }
  if (verbose) {
    message(sprintf(
      "Applying %d-year centered rolling mean to: %s",
      k_eff,
      paste(numeric_vars, collapse = ", ")
    ))
  }
  if (length(group_cols) > 0) {
    data <- data |>
      dplyr::arrange(!!!rlang::syms(group_cols), .data[[time_var]]) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
      dplyr::mutate(dplyr::across(
        dplyr::all_of(numeric_vars),
        ~ zoo::rollmean(.x, k = k_eff, fill = NA, align = "center")
      )) |>
      dplyr::ungroup() |>
      dplyr::filter(!is.na(.data[[target_var]]))
  } else {
    data <- data |>
      dplyr::arrange(.data[[time_var]]) |>
      dplyr::mutate(dplyr::across(
        dplyr::all_of(numeric_vars),
        ~ zoo::rollmean(.x, k = k_eff, fill = NA, align = "center")
      )) |>
      dplyr::filter(!is.na(.data[[target_var]]))
  }
  if (nrow(data) == 0) {
    cli::cli_warn(
      "No data remaining after applying rolling mean. Consider using a smaller rolling_mean value."
    )
  }
  if (verbose) {
    message(sprintf(
      "Data reduced to %d rows after rolling mean",
      nrow(data)
    ))
  }
  data
}

.lmdi_handle_identity_labels <- function(identity_labels, factors, target_var) {
  if (!is.null(identity_labels)) {
    if (length(identity_labels) != length(factors) + 1) {
      stop(sprintf(
        "identity_labels must have %d elements: 1 for target + %d for factors.",
        length(factors) + 1,
        length(factors)
      ))
    }
    target_label_final <- identity_labels[1]
    factor_labels <- identity_labels[-1]
  } else {
    target_label_final <- target_var
    factor_labels <- factors
  }
  list(target_label_final = target_label_final, factor_labels = factor_labels)
}

.lmdi_setup_periods <- function(data, time_var, periods, periods_2) {
  if (is.null(periods)) {
    years <- sort(unique(data[[time_var]]))
    if (length(years) < 2) {
      cli::cli_abort("Need at least two periods to perform the decomposition")
    }
    periods <- data.frame(t0 = years[-length(years)], tT = years[-1])
  } else {
    if (is.matrix(periods) || is.data.frame(periods)) {
      periods <- as.data.frame(periods)
      names(periods) <- c("t0", "tT")
    } else if (is.vector(periods)) {
      if (length(periods) == 2) {
        periods <- data.frame(t0 = periods[1], tT = periods[2])
      } else if (length(periods) > 2) {
        periods <- data.frame(
          t0 = periods[-length(periods)],
          tT = periods[-1]
        )
      } else {
        cli::cli_abort("periods must be a vector of at least 2 years")
      }
    } else {
      cli::cli_abort(
        "periods must be a 2-column matrix/data.frame or a vector of years"
      )
    }
  }
  if (!is.null(periods_2)) {
    if (is.matrix(periods_2) || is.data.frame(periods_2)) {
      periods_2 <- as.data.frame(periods_2)
      names(periods_2) <- c("t0", "tT")
    } else if (is.vector(periods_2) && length(periods_2) == 2) {
      periods_2 <- data.frame(t0 = periods_2[1], tT = periods_2[2])
    } else {
      cli::cli_abort(
        "periods_2 must be a 2-column matrix/data.frame or a vector of length 2"
      )
    }
    periods <- rbind(periods, periods_2)
  }
  periods
}

.lmdi_calc_period <- function(
  subset_data, periods, i, time_var, group_vars, target_var, factors,
  factor_labels, target_label_final, identity, analysis_cols, analysis_values
) {
  t0 <- periods$t0[i]
  t_final <- periods$tT[i]
  d0 <- subset_data |> dplyr::filter(.data[[time_var]] == t0)
  d_final <- subset_data |> dplyr::filter(.data[[time_var]] == t_final)
  if (nrow(d0) == 0 || nrow(d_final) == 0) {
    return(NULL)
  }
  if (!is.null(group_vars)) {
    groups <- d0 |>
      dplyr::select(dplyr::all_of(group_vars)) |>
      dplyr::distinct()
    group_index_str <- paste(group_vars, collapse = "+")
  } else {
    groups <- data.frame(dummy = 1)
    group_index_str <- ""
  }
  y0_total <- sum(d0[[target_var]], na.rm = TRUE)
  y_final_total <- sum(d_final[[target_var]], na.rm = TRUE)
  total_change <- y_final_total - y0_total
  period_years <- suppressWarnings(
    as.numeric(as.character(t_final)) - as.numeric(as.character(t0))
  )
  if (is.na(period_years)) period_years <- 0
  period_contribs_add <- rep(0, length(factors))
  period_contribs_mult_log <- rep(0, length(factors))
  l_total <- .log_mean(y_final_total, y0_total)
  for (g in seq_len(nrow(groups))) {
    if (!is.null(group_vars)) {
      group_vals <- as.list(groups[g, , drop = FALSE])
    } else {
      group_vals <- list()
    }
    d0g <- .filter_by_group(d0, group_index_str, group_vals)
    d_final_g <- .filter_by_group(d_final, group_index_str, group_vals)
    y0 <- sum(d0g[[target_var]], na.rm = TRUE)
    y_final <- sum(d_final_g[[target_var]], na.rm = TRUE)
    if (y0 == 0 || y_final == 0) next
    f0 <- sapply(
      factors,
      function(f) as.numeric(.eval_factor(d0, f, group_vals))
    )
    f_final <- sapply(
      factors,
      function(f) as.numeric(.eval_factor(d_final, f, group_vals))
    )
    if (any(is.na(f0)) || any(is.na(f_final))) next
    l_val <- .log_mean(y_final, y0)
    for (j in seq_along(factors)) {
      if (f0[j] > 0 && f_final[j] > 0) {
        period_contribs_add[j] <- period_contribs_add[j] +
          l_val * log(f_final[j] / f0[j])
      }
    }
    if (l_total != 0) {
      weight <- l_val / l_total
      for (j in seq_along(factors)) {
        if (f0[j] > 0 && f_final[j] > 0) {
          period_contribs_mult_log[j] <- period_contribs_mult_log[j] +
            weight * log(f_final[j] / f0[j])
        }
      }
    }
  }
  period_contribs_mult <- if (l_total == 0) {
    rep(1, length(factors))
  } else {
    exp(period_contribs_mult_log)
  }
  period_contribs_mult_log_vals <- ifelse(
    period_contribs_mult > 0,
    log(period_contribs_mult),
    NA_real_
  )
  additive_gap <- total_change - sum(period_contribs_add)
  target_ratio <- ifelse(
    y0_total > 0,
    y_final_total / y0_total,
    NA_real_
  )
  mult_product <- prod(period_contribs_mult, na.rm = TRUE)
  multiplicative_gap <- ifelse(
    is.na(target_ratio) || mult_product == 0,
    NA_real_,
    target_ratio / mult_product
  )
  period_id <- paste(t0, t_final, sep = "-")
  factor_rows <- data.frame(
    period = rep(period_id, length(factors)),
    period_years = period_years,
    factor_label = factor_labels,
    factor_formula = factors,
    component_type = "factor",
    identity = identity,
    identity_var = target_var,
    target_initial = NA_real_,
    target_final = NA_real_,
    total_change = NA_real_,
    percentage_change = NA_real_,
    additive = period_contribs_add,
    multiplicative = period_contribs_mult,
    multiplicative_log = period_contribs_mult_log_vals,
    closure_gap_additive = NA_real_,
    closure_gap_ratio = NA_real_,
    stringsAsFactors = FALSE
  )
  target_row <- data.frame(
    period = period_id,
    period_years = period_years,
    factor_label = target_label_final,
    factor_formula = target_var,
    component_type = "target",
    identity = identity,
    identity_var = target_var,
    target_initial = y0_total,
    target_final = y_final_total,
    total_change = total_change,
    percentage_change = if (y0_total != 0) {
      (total_change / y0_total) * 100
    } else {
      NA_real_
    },
    additive = total_change,
    multiplicative = target_ratio,
    multiplicative_log = ifelse(
      !is.na(target_ratio) && target_ratio > 0,
      log(target_ratio),
      NA_real_
    ),
    closure_gap_additive = additive_gap,
    closure_gap_ratio = multiplicative_gap,
    stringsAsFactors = FALSE
  )
  if (length(analysis_cols) > 0) {
    for (nm in analysis_cols) {
      factor_rows[[nm]] <- analysis_values[[nm]]
      target_row[[nm]] <- analysis_values[[nm]]
    }
  }
  result <- dplyr::bind_rows(factor_rows, target_row)
  period_target_df <- data.frame(
    period = period_id,
    t0 = t0,
    tT = t_final,
    target_initial = y0_total,
    target_final = y_final_total,
    total_change = total_change,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  if (length(analysis_cols) > 0) {
    for (nm in analysis_cols) {
      period_target_df[[nm]] <- analysis_values[[nm]]
    }
  }
  list(result = result, target = period_target_df)
}

.lmdi_closure_check <- function(analysis_result, analysis_cols, tolerance_add, tolerance_mult) {
  closure_group_cols <- c("period", analysis_cols)
  closure_summary <- analysis_result |>
    dplyr::group_by(dplyr::across(dplyr::all_of(closure_group_cols))) |>
    dplyr::summarise(
      target_add = dplyr::first(
        .data$additive[.data$component_type == "target"]
      ),
      sum_factors_add = sum(
        .data$additive[.data$component_type == "factor"],
        na.rm = TRUE
      ),
      target_mult = dplyr::first(
        .data$multiplicative[.data$component_type == "target"]
      ),
      prod_factors_mult = {
        vals <- .data$multiplicative[.data$component_type == "factor"]
        if (length(vals) == 0) 1 else prod(vals, na.rm = TRUE)
      },
      .groups = "drop"
    )
  for (row_idx in seq_len(nrow(closure_summary))) {
    add_diff <- closure_summary$target_add[row_idx] -
      closure_summary$sum_factors_add[row_idx]
    if (!is.na(add_diff) && abs(add_diff) > tolerance_add) {
      context <- closure_summary[row_idx, closure_group_cols, drop = TRUE]
      warning(sprintf(
        "[%s] Additive contributions differ from target change by %.4g",
        paste(context, collapse = ", "),
        add_diff
      ))
    }
    target_mult_val <- closure_summary$target_mult[row_idx]
    prod_mult_val <- closure_summary$prod_factors_mult[row_idx]
    if (
      !is.na(target_mult_val) && !is.na(prod_mult_val) && prod_mult_val != 0
    ) {
      mult_diff <- abs(target_mult_val / prod_mult_val - 1)
      if (mult_diff > tolerance_mult) {
        context <- closure_summary[row_idx, closure_group_cols, drop = TRUE]
        warning(sprintf(
          "[%s] Multiplicative contributions differ from target ratio by %.4g",
          paste(context, collapse = ", "),
          mult_diff
        ))
      }
    }
  }
}


.log_mean <- function(a, b) {
  ifelse(a == b, a, ifelse(a > 0 & b > 0, (a - b) / log(a / b), 0))
}

.parse_identity <- function(identity_expr) {
  identity_expr <- gsub(" ", "", identity_expr)
  parts <- strsplit(identity_expr, ":")[[1]]
  if (length(parts) != 2) {
    cli::cli_abort(
      "identity must follow the pattern 'target:factor1*factor2*...'"
    )
  }
  target <- parts[1]
  rhs <- parts[2]
  factors <- strsplit(rhs, "\\*")[[1]]
  factors <- gsub("^\\((.*)\\)$", "\1", factors)
  list(target = target, factors = factors)
}

.extract_selectors <- function(identity_expr) {
  matches <- stringr::str_extract_all(
    identity_expr,
    "\\[([a-zA-Z0-9_+]+)\\]"
  )[[1]]
  if (length(matches) == 0) {
    return(character(0))
  }
  selectors <- gsub("\\[|\\]", "", matches)
  unique(unlist(strsplit(selectors, "\\+")))
}

.filter_by_group <- function(df, index_str, group_vals) {
  if (is.null(index_str) || index_str == "" || is.na(index_str)) {
    return(df)
  }
  indices <- strsplit(index_str, "\\+")[[1]]
  out <- df
  for (idx in indices) {
    val <- group_vals[[idx]]
    if (!is.null(val) && !is.na(val)) {
      out <- out |> dplyr::filter(.data[[idx]] == val)
    }
  }
  out
}

.eval_factor <- function(df_period, factor, group_vals) {
  pat1 <- "^([a-zA-Z0-9_]+)\\[([a-zA-Z0-9_+]+)\\]$"
  pat2 <- "^([a-zA-Z0-9_]+)\\/([a-zA-Z0-9_]+)$"
  pat3 <- "^([a-zA-Z0-9_]+)\\[([a-zA-Z0-9_+]+)\\]\\/([a-zA-Z0-9_]+)$"
  pat4 <- "^([a-zA-Z0-9_]+)\\/([a-zA-Z0-9_]+)\\[([a-zA-Z0-9_+]+)\\]$"
  pat5 <- paste0(
    "^([a-zA-Z0-9_]+)\\[([a-zA-Z0-9_+]+)\\]\\/",
    "([a-zA-Z0-9_]+)\\[([a-zA-Z0-9_+]+)\\]$"
  )

  if (stringr::str_detect(factor, pat5)) {
    m <- stringr::str_match(factor, pat5)
    var1 <- m[2]
    group1 <- m[3]
    var2 <- m[4]
    group2 <- m[5]
    num_df <- .filter_by_group(df_period, group1, group_vals)
    den_df <- .filter_by_group(df_period, group2, group_vals)
    num <- sum(num_df[[var1]], na.rm = TRUE)
    den <- sum(den_df[[var2]], na.rm = TRUE)
    ifelse(den == 0, NA_real_, num / den)
  } else if (stringr::str_detect(factor, pat3)) {
    m <- stringr::str_match(factor, pat3)
    var1 <- m[2]
    group1 <- m[3]
    var2 <- m[4]
    num_df <- .filter_by_group(df_period, group1, group_vals)
    num <- sum(num_df[[var1]], na.rm = TRUE)
    den <- sum(df_period[[var2]], na.rm = TRUE)
    ifelse(den == 0, NA_real_, num / den)
  } else if (stringr::str_detect(factor, pat4)) {
    m <- stringr::str_match(factor, pat4)
    var1 <- m[2]
    var2 <- m[3]
    group2 <- m[4]
    num <- sum(df_period[[var1]], na.rm = TRUE)
    den_df <- .filter_by_group(df_period, group2, group_vals)
    den <- sum(den_df[[var2]], na.rm = TRUE)
    ifelse(den == 0, NA_real_, num / den)
  } else if (stringr::str_detect(factor, pat2)) {
    m <- stringr::str_match(factor, pat2)
    var1 <- m[2]
    var2 <- m[3]
    num <- sum(df_period[[var1]], na.rm = TRUE)
    den <- sum(df_period[[var2]], na.rm = TRUE)
    ifelse(den == 0, NA_real_, num / den)
  } else if (stringr::str_detect(factor, pat1)) {
    m <- stringr::str_match(factor, pat1)
    var1 <- m[2]
    group1 <- m[3]
    sub_df <- .filter_by_group(df_period, group1, group_vals)
    sum(sub_df[[var1]], na.rm = TRUE)
  } else {
    sum(df_period[[factor]], na.rm = TRUE)
  }
}
