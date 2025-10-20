# Title: lmdi_calculation
# Description: Flexible LMDI decomposition with identity parsing and auto-detection
# Date: 2025-01-19

lmdi_calculation <- function(
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
  # Load required packages
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package 'dplyr' is required")
  if (!requireNamespace("rlang", quietly = TRUE)) stop("Package 'rlang' is required")
  if (!requireNamespace("stringr", quietly = TRUE)) stop("Package 'stringr' is required")
  if (!requireNamespace("tidyr", quietly = TRUE)) stop("Package 'tidyr' is required")
  if (!requireNamespace("zoo", quietly = TRUE)) stop("Package 'zoo' is required")
  if (!requireNamespace("tibble", quietly = TRUE)) stop("Package 'tibble' is required")

  log_mean <- function(a, b) {
    ifelse(a == b, a, ifelse(a > 0 & b > 0, (a - b) / log(a / b), 0))
  }

  parse_identity <- function(identity_expr) {
    identity_expr <- gsub(" ", "", identity_expr)
    parts <- strsplit(identity_expr, ":")[[1]]
    if (length(parts) != 2) {
      stop("identity must follow the pattern 'target:factor1*factor2*...'")
    }
    target <- parts[1]
    rhs <- parts[2]
    factors <- strsplit(rhs, "\\*")[[1]]
    factors <- gsub("^\\((.*)\\)$", "\\1", factors)
    list(target = target, factors = factors)
  }

  # NEW: Extract selectors from identity (e.g., [sector], [sector+fuel])
  extract_selectors <- function(identity_expr) {
    matches <- stringr::str_extract_all(identity_expr, "\\[([a-zA-Z0-9_+]+)\\]")[[1]]
    if (length(matches) == 0) return(character(0))
    selectors <- gsub("\\[|\\]", "", matches)
    unique(unlist(strsplit(selectors, "\\+")))
  }

  filter_by_group <- function(df, index_str, group_vals) {
    if (is.null(index_str) || index_str == "" || is.na(index_str)) return(df)
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

  eval_factor <- function(df_period, factor, group_vals) {
    pat1 <- "^([a-zA-Z0-9_]+)\\[([a-zA-Z0-9_+]+)\\]$"
    pat2 <- "^([a-zA-Z0-9_]+)\\/([a-zA-Z0-9_]+)$"
    pat3 <- "^([a-zA-Z0-9_]+)\\[([a-zA-Z0-9_+]+)\\]\\/([a-zA-Z0-9_]+)$"
    pat4 <- "^([a-zA-Z0-9_]+)\\/([a-zA-Z0-9_]+)\\[([a-zA-Z0-9_+]+)\\]$"
    pat5 <- "^([a-zA-Z0-9_]+)\\[([a-zA-Z0-9_+]+)\\]\\/([a-zA-Z0-9_]+)\\[([a-zA-Z0-9_+]+)\\]$"

    if (stringr::str_detect(factor, pat5)) {
      m <- stringr::str_match(factor, pat5)
      var1 <- m[2]; group1 <- m[3]; var2 <- m[4]; group2 <- m[5]
      num_df <- filter_by_group(df_period, group1, group_vals)
      den_df <- filter_by_group(df_period, group2, group_vals)
      num <- sum(num_df[[var1]], na.rm = TRUE)
      den <- sum(den_df[[var2]], na.rm = TRUE)
      return(ifelse(den == 0, NA_real_, num / den))
    } else if (stringr::str_detect(factor, pat3)) {
      m <- stringr::str_match(factor, pat3)
      var1 <- m[2]; group1 <- m[3]; var2 <- m[4]
      num_df <- filter_by_group(df_period, group1, group_vals)
      num <- sum(num_df[[var1]], na.rm = TRUE)
      den <- sum(df_period[[var2]], na.rm = TRUE)
      return(ifelse(den == 0, NA_real_, num / den))
    } else if (stringr::str_detect(factor, pat4)) {
      m <- stringr::str_match(factor, pat4)
      var1 <- m[2]; var2 <- m[3]; group2 <- m[4]
      num <- sum(df_period[[var1]], na.rm = TRUE)
      den_df <- filter_by_group(df_period, group2, group_vals)
      den <- sum(den_df[[var2]], na.rm = TRUE)
      return(ifelse(den == 0, NA_real_, num / den))
    } else if (stringr::str_detect(factor, pat2)) {
      m <- stringr::str_match(factor, pat2)
      var1 <- m[2]; var2 <- m[3]
      num <- sum(df_period[[var1]], na.rm = TRUE)
      den <- sum(df_period[[var2]], na.rm = TRUE)
      return(ifelse(den == 0, NA_real_, num / den))
    } else if (stringr::str_detect(factor, pat1)) {
      m <- stringr::str_match(factor, pat1)
      var1 <- m[2]; group1 <- m[3]
      sub_df <- filter_by_group(df_period, group1, group_vals)
      return(sum(sub_df[[var1]], na.rm = TRUE))
    } else {
      return(sum(df_period[[factor]], na.rm = TRUE))
    }
  }

  id <- parse_identity(identity)
  target_var <- id$target
  factors <- id$factors

  # ============================================================================
  # ROLLING MEAN (MEDIA MÓVIL)
  # ============================================================================
  
  if (!is.null(rolling_mean)) {
    if (!is.numeric(rolling_mean) || rolling_mean < 2) {
      stop("rolling_mean must be a numeric value >= 2 (number of years for centered moving average)")
    }
    
    # Get all numeric variables used in the identity
    all_vars <- unique(c(target_var, 
                         stringr::str_extract_all(identity, "[a-zA-Z0-9_]+")[[1]]))
    numeric_vars <- all_vars[all_vars %in% names(data)]
    numeric_vars <- numeric_vars[sapply(data[numeric_vars], is.numeric)]
    
    # Identify grouping columns (everything except time_var and numeric variables)
    group_cols <- setdiff(names(data), c(time_var, numeric_vars))
    
    # ========================================================================
    # STEP 1: PANEL BALANCING (CRITICAL FOR LMDI)
    # ========================================================================
    # LMDI requires a BALANCED PANEL: all groups must exist in all time periods
    # Problem: If Country A has Wheat in 2020 but not in 2000, decomposition fails
    # Solution: Complete all combinations of (time × group_cols) and fill with epsilon
    
    if (verbose) {
      message("\n━━━ LMDI Data Preparation ━━━")
      message("Step 1: Panel balancing")
    }
    
    n_before <- nrow(data)
    n_groups_before <- if (length(group_cols) > 0) {
      nrow(dplyr::distinct(data, dplyr::across(dplyr::all_of(group_cols))))
    } else {
      1
    }
    n_years <- dplyr::n_distinct(data[[time_var]])
    
    if (length(group_cols) > 0) {
      # Complete all combinations: time × group_cols using data.table for efficiency
      if (!requireNamespace("data.table", quietly = TRUE)) {
        # Fallback to tidyr if data.table not available
        data <- data |>
          tidyr::complete(
            !!!rlang::syms(c(time_var, group_cols)),
            fill = as.list(stats::setNames(rep(NA_real_, length(numeric_vars)), numeric_vars))
          )
      } else {
        # Use data.table for better performance on large datasets
        dt <- data.table::as.data.table(data)
        key_vars <- c(time_var, group_cols)
        
        # Create complete grid using CJ (cross join)
        unique_vals <- lapply(key_vars, function(v) unique(dt[[v]]))
        names(unique_vals) <- key_vars
        complete_grid <- do.call(data.table::CJ, c(unique_vals, list(sorted = FALSE)))
        
        # Efficient merge
        data.table::setkeyv(dt, key_vars)
        data.table::setkeyv(complete_grid, key_vars)
        data <- tibble::as_tibble(complete_grid[dt])
      }
    }
    
    n_after <- nrow(data)
    n_added <- n_after - n_before
    
    if (verbose && n_added > 0) {
      message(sprintf("  - Original: %d rows", n_before))
      message(sprintf("  - Balanced: %d rows (+%d rows added)", n_after, n_added))
      message(sprintf("  - Structure: %d groups × %d years = %d expected rows", 
                     n_groups_before, n_years, n_groups_before * n_years))
    }
    
    # ========================================================================
    # STEP 2: ZERO AND NA REPLACEMENT (REQUIRED FOR LMDI)
    # ========================================================================
    # LMDI uses log() and division → requires strictly positive values
    # Following Ang (2015): Replace ALL zeros and NA with small epsilon (1e-10)
    # This is NOT optional - it's a mathematical requirement of the method
    
    if (verbose) {
      message("\nStep 2: Zero/NA treatment (Ang, 2015 methodology)")
    }
    
    epsilon <- 1e-10  # Standard value from literature
    
    n_zeros <- sum(sapply(data[numeric_vars], function(x) sum(x == 0, na.rm = TRUE)))
    n_nas <- sum(sapply(data[numeric_vars], function(x) sum(is.na(x))))
    
    if (verbose) {
      message(sprintf("  - Zeros detected: %d", n_zeros))
      message(sprintf("  - NA detected: %d", n_nas))
      message(sprintf("  - Replacement value: %.2e (epsilon)", epsilon))
    }
    
    # Replace ALL zeros and NA with epsilon
    data <- data |>
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(numeric_vars),
          ~dplyr::if_else(is.na(.x) | .x == 0, epsilon, .x)
        )
      )
    
    if (verbose) {
      message(sprintf("  - Result: All numeric values now > 0"))
      message(sprintf("  - Final dataset: %d rows", nrow(data)))
    }
    
    # Verify no zeros or NA remain
    remaining_zeros <- sum(sapply(data[numeric_vars], function(x) sum(x == 0, na.rm = TRUE)))
    remaining_nas <- sum(sapply(data[numeric_vars], function(x) sum(is.na(x))))
    
    if (remaining_zeros > 0 || remaining_nas > 0) {
      stop("Internal error: Zeros or NA still present after replacement")
    }
    
    if (verbose) {
      message("━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n")
    }
    # ========================================================================

    # If window is larger than the available years in any group, auto-shrink and warn
    years_per_group <- NULL
    if (length(group_cols) > 0) {
      years_per_group <- data |>
        dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
        dplyr::summarise(n_years = dplyr::n_distinct(.data[[time_var]]), .groups = "drop")
    } else {
      years_per_group <- tibble::tibble(n_years = dplyr::n_distinct(data[[time_var]]))
    }
    min_years <- min(years_per_group$n_years)
    k_orig <- as.integer(rolling_mean)
    k_eff <- k_orig
    if (min_years < k_orig) {
      # Prefer the largest odd window <= min_years, with floor at 3
      k_eff <- max(3L, min(min_years, ifelse(k_orig %% 2 == 1, k_orig, k_orig - 1)))
      if (k_eff > min_years) k_eff <- max(3L, min_years - (1L - (min_years %% 2)))
      if (verbose) {
        message(sprintf("rolling_mean=%d is larger than available years in some groups (min=%d). Using k=%d.", k_orig, min_years, k_eff))
      }
    }
    
    if (verbose) {
  message(sprintf("Applying %d-year centered rolling mean to variables: %s", 
          k_eff, paste(numeric_vars, collapse = ", ")))
    }
    
    # Apply rolling mean by group
    if (length(group_cols) > 0) {
      data <- data |>
        dplyr::arrange(!!!rlang::syms(group_cols), .data[[time_var]]) |>
        dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
        dplyr::mutate(dplyr::across(dplyr::all_of(numeric_vars), 
                      ~zoo::rollmean(.x, k = k_eff, fill = NA, align = "center"))) |>
        dplyr::ungroup() |>
        dplyr::filter(!is.na(.data[[target_var]]))  # Remove rows with NA in target after smoothing
    } else {
      # No grouping - global rolling mean
      data <- data |>
        dplyr::arrange(.data[[time_var]]) |>
        dplyr::mutate(dplyr::across(dplyr::all_of(numeric_vars), 
                      ~zoo::rollmean(.x, k = k_eff, fill = NA, align = "center"))) |>
        dplyr::filter(!is.na(.data[[target_var]]))
    }
    
    if (nrow(data) == 0) {
      warning("No data remaining after applying rolling mean. Proceeding without smoothing. Consider using a smaller rolling_mean value.")
      # Fallback: do not smooth
      data <- data |> dplyr::ungroup()
      # no-op: keep original input (unsmoothed)
      # Note: We intentionally skip further smoothing here.
    }
    
    if (verbose) {
      message(sprintf("Data reduced to %d rows after rolling mean (removed edge years with NA)", nrow(data)))
    }
  }

  # ============================================================================
  # IDENTITY LABELS & AUTO-DETECTION
  # ============================================================================
  
  # Handle identity_labels
  if (!is.null(identity_labels)) {
    if (length(identity_labels) != length(factors) + 1) {
      stop(sprintf("identity_labels must have %d elements: 1 for target + %d for factors. Got %d.", 
                   length(factors) + 1, length(factors), length(identity_labels)))
    }
    target_label_final <- identity_labels[1]
    factor_labels <- identity_labels[-1]
  } else {
    # Use variable names as defaults
    target_label_final <- target_var
    factor_labels <- factors
  }
  
  # Auto-detect selectors from identity (e.g., [sector], [fuel])
  selectors_detected <- extract_selectors(identity)
  
  # Build group_vars internally from analysis_by + detected selectors
  group_vars <- NULL
  if (!is.null(analysis_by)) {
    # Combine analysis_by + detected selectors
    group_vars <- unique(c(analysis_by, selectors_detected))
    if (verbose && length(selectors_detected) > 0) {
      message(sprintf("Auto-detected selectors from identity: %s", paste(selectors_detected, collapse = ", ")))
      message(sprintf("group_vars set to: %s", paste(group_vars, collapse = ", ")))
    }
  } else if (length(selectors_detected) > 0) {
    group_vars <- selectors_detected
    if (verbose) {
      message(sprintf("Auto-detected group_vars from selectors: %s", paste(group_vars, collapse = ", ")))
    }
  }
  # else: group_vars remains NULL (global analysis)
  
  # ============================================================================
  # END IDENTITY LABELS & AUTO-DETECTION
  # ============================================================================

  if (is.null(periods)) {
    years <- sort(unique(data[[time_var]]))
    if (length(years) < 2) stop("Need at least two periods to perform the decomposition")
    periods <- data.frame(t0 = years[-length(years)], tT = years[-1])
  } else {
    if (is.matrix(periods) || is.data.frame(periods)) {
      periods <- as.data.frame(periods)
      names(periods) <- c("t0", "tT")
    } else if (is.vector(periods)) {
      if (length(periods) == 2) {
        # Single period: vector of length 2
        periods <- data.frame(t0 = periods[1], tT = periods[2])
      } else if (length(periods) > 2) {
        # Multiple years: create consecutive periods automatically
        # e.g., c(1990, 2000, 2010) -> (1990-2000, 2000-2010)
        periods <- data.frame(t0 = periods[-length(periods)], 
                              tT = periods[-1])
      } else {
        stop("periods must be a vector of at least 2 years")
      }
    } else {
      stop("periods must be a 2-column matrix/data.frame or a vector of years")
    }
  }
  
  # Combine periods_2 if provided
  if (!is.null(periods_2)) {
    if (is.matrix(periods_2) || is.data.frame(periods_2)) {
      periods_2 <- as.data.frame(periods_2)
      names(periods_2) <- c("t0", "tT")
    } else if (is.vector(periods_2) && length(periods_2) == 2) {
      periods_2 <- data.frame(t0 = periods_2[1], tT = periods_2[2])
    } else {
      stop("periods_2 must be a 2-column matrix/data.frame or a vector of length 2")
    }
    periods <- rbind(periods, periods_2)
  }

  if (!is.null(group_vars)) {
    if (!all(group_vars %in% names(data))) {
      stop("All group_vars must exist in the data")
    }
  }

  if (!is.null(analysis_by)) {
    analysis_by <- unique(analysis_by)
    if (!all(analysis_by %in% names(data))) {
      stop("All columns in analysis_by must exist in the data")
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
        subset_data <- subset_data |> dplyr::filter(.data[[nm]] == analysis_values[[nm]])
      }
      if (nrow(subset_data) == 0) {
        next
      }
    }

    period_results <- list()
    period_targets <- list()

    for (i in seq_len(nrow(periods))) {
      t0 <- periods$t0[i]
      tT <- periods$tT[i]
      d0 <- subset_data |> dplyr::filter(.data[[time_var]] == t0)
      dT <- subset_data |> dplyr::filter(.data[[time_var]] == tT)

      if (nrow(d0) == 0 || nrow(dT) == 0) {
        if (verbose) {
          group_tag <- if (is.null(analysis_values)) "Global" else 
            paste(sprintf("%s=%s", names(analysis_values), analysis_values), collapse = ", ")
          message(sprintf("⚠ Skipping period %s-%s for %s: No data found (t0: %d rows, tT: %d rows)", 
                        t0, tT, group_tag, nrow(d0), nrow(dT)))
        }
        next
      }

      if (!is.null(group_vars)) {
        groups <- d0 |> dplyr::select(dplyr::all_of(group_vars)) |> dplyr::distinct()
        group_index_str <- paste(group_vars, collapse = "+")
      } else {
        groups <- data.frame(dummy = 1)
        group_index_str <- ""
      }

      Y0_total <- sum(d0[[target_var]], na.rm = TRUE)
      YT_total <- sum(dT[[target_var]], na.rm = TRUE)
      total_change <- YT_total - Y0_total
      period_years <- suppressWarnings(as.numeric(as.character(tT)) - as.numeric(as.character(t0)))
      if (is.na(period_years)) period_years <- 0

      period_contribs_add <- rep(0, length(factors))
      period_contribs_mult_log <- rep(0, length(factors))
      L_total <- log_mean(YT_total, Y0_total)

      for (g in seq_len(nrow(groups))) {
        if (!is.null(group_vars)) {
          group_vals <- as.list(groups[g, , drop = FALSE])
        } else {
          group_vals <- list()
        }

        d0g <- filter_by_group(d0, group_index_str, group_vals)
        dTg <- filter_by_group(dT, group_index_str, group_vals)

        Y0 <- sum(d0g[[target_var]], na.rm = TRUE)
        YT <- sum(dTg[[target_var]], na.rm = TRUE)

        if (Y0 == 0 || YT == 0) next

        # Evaluate factors using the FULL period data (d0/dT) not just the group subset
        # This allows eval_factor to apply its own filtering based on selectors
        F0 <- sapply(factors, function(f) as.numeric(eval_factor(d0, f, group_vals)))
        FT <- sapply(factors, function(f) as.numeric(eval_factor(dT, f, group_vals)))

        if (any(is.na(F0)) || any(is.na(FT))) {
          if (verbose) {
            # Identificar qué factores son problemáticos
            invalid_f0 <- which(is.na(F0))
            invalid_fT <- which(is.na(FT))
            
            message(sprintf("\n⚠ Invalid factors for group %s in period %s-%s:", g, t0, tT))
            if (!is.null(group_vals) && length(group_vals) > 0) {
              message(sprintf("  Group values: %s", 
                            paste(sprintf("%s=%s", names(group_vals), group_vals), collapse = ", ")))
            }
            
            if (length(invalid_f0) > 0) {
              message(sprintf("  Period %s - Invalid factors:", t0))
              for (idx in invalid_f0) {
                message(sprintf("    Factor %d: %s → NA (likely denominator = 0)", idx, factors[idx]))
              }
            }
            
            if (length(invalid_fT) > 0) {
              message(sprintf("  Period %s - Invalid factors:", tT))
              for (idx in invalid_fT) {
                message(sprintf("    Factor %d: %s → NA (likely denominator = 0)", idx, factors[idx]))
              }
            }
            
            message("  → Skipping this group/period combination\n")
          }
          next
        }

        L <- log_mean(YT, Y0)
        for (j in seq_along(factors)) {
          if (F0[j] > 0 && FT[j] > 0) {
            period_contribs_add[j] <- period_contribs_add[j] + L * log(FT[j] / F0[j])
          }
        }

        if (L_total != 0) {
          weight <- L / L_total
          for (j in seq_along(factors)) {
            if (F0[j] > 0 && FT[j] > 0) {
              period_contribs_mult_log[j] <- period_contribs_mult_log[j] + weight * log(FT[j] / F0[j])
            }
          }
        }
      }

  period_contribs_mult <- if (L_total == 0) rep(1, length(factors)) else exp(period_contribs_mult_log)
  period_contribs_mult_log_vals <- ifelse(period_contribs_mult > 0, log(period_contribs_mult), NA_real_)
      additive_gap <- total_change - sum(period_contribs_add)
      target_ratio <- ifelse(Y0_total > 0, YT_total / Y0_total, NA_real_)
      mult_product <- prod(period_contribs_mult, na.rm = TRUE)
      multiplicative_gap <- ifelse(is.na(target_ratio) || mult_product == 0, NA_real_, target_ratio / mult_product)

      period_id <- paste(t0, tT, sep = "-")
      factor_rows <- data.frame(
        period = rep(period_id, length(factors)),
        period_years = period_years,
        factor_label = factor_labels,
        factor_formula = factors,
        component_type = "factor",
        identity = identity,
        identity_label = NA_character_,   # Removed in simplified API
        identity_acronym = NA_character_, # Removed in simplified API
        identity_var = target_var,
        target_initial = NA_real_,
        target_final = NA_real_,
        total_change = NA_real_,
        percentage_change = NA_real_,
        annual_growth_rate = NA_real_,
        additive = period_contribs_add,
        additive_annual = if (period_years > 0) period_contribs_add / period_years else period_contribs_add,
        multiplicative = period_contribs_mult,
        multiplicative_annual = if (period_years > 0) period_contribs_mult^(1/period_years) else period_contribs_mult,
        multiplicative_log = period_contribs_mult_log_vals,
        multiplicative_log_annual = if (period_years > 0) period_contribs_mult_log_vals / period_years else period_contribs_mult_log_vals,
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
        identity_label = NA_character_,   # Removed in simplified API
        identity_acronym = NA_character_, # Removed in simplified API
        identity_var = target_var,
        target_initial = Y0_total,
        target_final = YT_total,
        total_change = total_change,
        percentage_change = if (Y0_total != 0) (total_change / Y0_total) * 100 else NA_real_,
        annual_growth_rate = if (Y0_total > 0 && period_years > 0) ((YT_total / Y0_total)^(1/period_years) - 1) * 100 else NA_real_,
        additive = total_change,
        additive_annual = if (period_years > 0) total_change / period_years else total_change,
        multiplicative = target_ratio,
        multiplicative_annual = if (period_years > 0 && !is.na(target_ratio) && target_ratio > 0) target_ratio^(1/period_years) else target_ratio,
        multiplicative_log = ifelse(!is.na(target_ratio) && target_ratio > 0, log(target_ratio), NA_real_),
        multiplicative_log_annual = if (period_years > 0 && !is.na(target_ratio) && target_ratio > 0) log(target_ratio) / period_years else ifelse(!is.na(target_ratio) && target_ratio > 0, log(target_ratio), NA_real_),
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

      period_results[[length(period_results) + 1]] <- dplyr::bind_rows(factor_rows, target_row)
      period_target_df <- data.frame(
        period = period_id,
        t0 = t0,
        tT = tT,
        target_initial = Y0_total,
        target_final = YT_total,
        total_change = total_change,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
      if (length(analysis_cols) > 0) {
        for (nm in analysis_cols) {
          period_target_df[[nm]] <- analysis_values[[nm]]
        }
      }
      period_targets[[length(period_targets) + 1]] <- period_target_df
    }

    if (length(period_results) == 0) next

    analysis_result <- dplyr::bind_rows(period_results)
    analysis_period_targets <- dplyr::bind_rows(period_targets)

    closure_group_cols <- c("period", analysis_cols)
    closure_summary <- analysis_result |>
      dplyr::group_by(dplyr::across(dplyr::all_of(closure_group_cols))) |>
      dplyr::summarise(
          target_add = dplyr::first(.data$additive[.data$component_type == "target"]),
          sum_factors_add = sum(.data$additive[.data$component_type == "factor"], na.rm = TRUE),
          target_mult = dplyr::first(.data$multiplicative[.data$component_type == "target"]),
          prod_factors_mult = { vals <- .data$multiplicative[.data$component_type == "factor"]; if (length(vals) == 0) 1 else prod(vals, na.rm = TRUE) },
        .groups = "drop"
      )

    for (row_idx in seq_len(nrow(closure_summary))) {
      add_diff <- closure_summary$target_add[row_idx] - closure_summary$sum_factors_add[row_idx]
      if (!is.na(add_diff) && abs(add_diff) > tolerance_add) {
        context <- closure_summary[row_idx, closure_group_cols, drop = TRUE]
        warning(sprintf("[%s] Additive contributions differ from target change by %.4g", paste(context, collapse = ", "), add_diff))
      }
      target_mult_val <- closure_summary$target_mult[row_idx]
      prod_mult_val <- closure_summary$prod_factors_mult[row_idx]
      if (!is.na(target_mult_val) && !is.na(prod_mult_val) && prod_mult_val != 0) {
        mult_diff <- abs(target_mult_val / prod_mult_val - 1)
        if (mult_diff > tolerance_mult) {
          context <- closure_summary[row_idx, closure_group_cols, drop = TRUE]
          warning(sprintf("[%s] Multiplicative contributions differ from target ratio by %.4g", paste(context, collapse = ", "), mult_diff))
        }
      }
    }

    results_all[[length(results_all) + 1]] <- analysis_result
    period_targets_all[[length(period_targets_all) + 1]] <- analysis_period_targets
  }

  if (length(results_all) == 0) {
    if (verbose) cat("No results produced.\n")
    return(tibble::tibble())
  }

  out <- dplyr::bind_rows(results_all)
  period_targets_df <- dplyr::bind_rows(period_targets_all)

  if (output_format == "clean") {
    select_cols <- c(analysis_cols, "period", "period_years", "factor_label", "component_type", "identity",
                     "identity_var", "additive", "additive_annual", "multiplicative",
                     "multiplicative_annual", "annual_growth_rate")
    select_cols <- select_cols[select_cols %in% names(out)]
    out <- out |> dplyr::select(dplyr::all_of(select_cols))
  }

  attributes_list <- list(
    identity = identity,
    identity_var = target_var,
    period_targets = period_targets_df,
    analysis_by = analysis_cols
  )

  for (nm in names(attributes_list)) {
    attr(out, nm) <- attributes_list[[nm]]
  }

  if (verbose) cat("LMDI calculation complete.\n")
  out
}