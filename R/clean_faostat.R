# Data-quality checking and cleaning for FAOSTAT-derived outputs.
#
# These helpers are applied *after* the v1 pipeline has run, adding a
# `qc_flag` column that records detected anomalies and optionally
# smoothing carry-forward tails.

# -- Constants -----------------------------------------------------------------

# Items that are known fodder/forage, affected by the 1984-85 EU reclassification
.fodder_items <- c(
  "Forage and silage, sorghum",
  "Forage and silage, rye grass",
  "Forage products",
  "Turnips for fodder",
  "Vegetables and roots fodder",
  "Mixed Grasses and Legumes",
  "Fodder vegetables and roots"
)

# -- Public QC helpers ---------------------------------------------------------

#' Flag constant-value streaks at the end of a time series
#'
#' Detects series where the value is identical for `min_run` or more
#' consecutive years ending at `max(year)`.
#' These are almost certainly FAOSTAT carry-forwards (imputed data that
#' just repeats the last observed value).
#'
#' @param df A tibble in long format with at least `year`, `value`, and
#'   the id columns listed in `by`.
#' @param by Character vector of grouping columns (everything except
#'   `year` and `value`).
#' @param min_run Minimum number of identical consecutive final years to
#'   flag (default 3).
#' @param value_col Name of the value column (default `"value"`).
#' @param time_col Name of the time column (default `"year"`).
#'
#' @returns The input tibble with a logical column `qc_carry_forward`
#'   (`TRUE` for flagged rows, `FALSE` otherwise).
#' @keywords internal
#' @noRd
.flag_carry_forward <- function(
  df,
  by,
  min_run = 3L,
  value_col = "value",
  time_col = "year"
) {
  force(df)
  cli::cli_progress_step("Flagging carry-forwards")
  if (!data.table::is.data.table(df)) {
    data.table::setDT(df)
  } else {
    data.table::setalloccol(df)
  }
  data.table::setkeyv(df, c(by, time_col))
  df[, qc_carry_forward := {
    val <- get(value_col)
    last_val <- val[.N]
    from_end <- rev(cumsum(rev(val != last_val)))
    run_length <- sum(from_end == 0)
    from_end == 0 & run_length >= min_run & val != 0 & !is.na(val)
  }, by = by]
  df
}

#' Flag year-on-year spikes
#'
#' Detects year-on-year changes exceeding `spike_ratio` (e.g. 10 means
#' a 10x jump or a 10x drop). Both the current and the previous value
#' must exceed `min_value` to qualify.
#'
#' @param df A tibble with at least `year`, `value`, and id columns.
#' @param by Character vector of grouping columns.
#' @param spike_ratio Threshold for |log(current/previous)| (default 10).
#' @param min_value Both values must exceed this to flag (default 1000).
#' @param value_col Name of the value column.
#' @param time_col Name of the time column.
#'
#' @returns The input tibble with a logical column `qc_spike`.
#' @keywords internal
#' @noRd
.flag_spikes <- function(
  df,
  by,
  spike_ratio = 10,
  min_value = 1000,
  value_col = "value",
  time_col = "year"
) {
  force(df)
  cli::cli_progress_step("Flagging spikes")
  if (!data.table::is.data.table(df)) data.table::setDT(df)
  sort_cols <- c(by, time_col)
  if (!identical(data.table::key(df), sort_cols)) {
    data.table::setorderv(df, sort_cols)
  }
  df[, qc_spike := {
    val <- get(value_col)
    prev <- data.table::shift(val, 1L, type = "lag")
    ratio <- val / prev
    !is.na(ratio) &
      is.finite(ratio) &
      (abs(ratio) > spike_ratio | abs(ratio) < 1 / spike_ratio) &
      val > min_value &
      prev > min_value
  }, by = by]
  df
}

#' Flag carry-forwards and spikes in a single grouped pass
#'
#' Combines the logic of `.flag_carry_forward` and `.flag_spikes`
#' into one `by=` expression, halving the grouped-dispatch overhead
#' (one sort + one pass instead of two).
#'
#' @inheritParams .flag_carry_forward
#' @inheritParams .flag_spikes
#' @noRd
.flag_cf_and_spikes <- function(
  df,
  by,
  min_run = 3L,
  spike_ratio = 10,
  spike_min = 1000,
  value_col = "value",
  time_col = "year"
) {
  force(df)
  cli::cli_progress_step("Flagging carry-forwards and spikes")
  if (!data.table::is.data.table(df)) {
    data.table::setDT(df)
  } else {
    data.table::setalloccol(df)
  }
  data.table::setkeyv(df, c(by, time_col))
  df[, c("qc_carry_forward", "qc_spike") := {
    val <- get(value_col)
    # carry-forward: constant tail of length >= min_run
    last_val <- val[.N]
    from_end <- rev(cumsum(rev(val != last_val)))
    run_len <- sum(from_end == 0)
    cf <- from_end == 0 &
      run_len >= min_run &
      val != 0 &
      !is.na(val)
    # spike: year-on-year ratio beyond threshold
    prev <- data.table::shift(val, 1L, type = "lag")
    ratio <- val / prev
    sp <- !is.na(ratio) &
      is.finite(ratio) &
      (abs(ratio) > spike_ratio |
         abs(ratio) < 1 / spike_ratio) &
      val > spike_min &
      prev > spike_min
    list(cf, sp)
  }, by = by]
  df
}

#' Flag the 1984-85 fodder reporting discontinuity
#'
#' Several European countries changed fodder/forage reporting around 1985,
#' causing impossible jumps.  This flags the break-point rows for known
#' fodder items.
#'
#' @param df A tibble with columns `year`, `item_prod` (or `item_cbs`),
#'   `value`, and id columns.
#' @param item_col Name of the item column to match against `.fodder_items`.
#' @param value_col Name of the value column.
#'
#' @returns The input tibble with a logical column `qc_fodder_break`.
#' @keywords internal
#' @noRd
.flag_fodder_break <- function(
  df,
  item_col = "item_prod",
  value_col = "value"
) {
  force(df)
  cli::cli_progress_step("Flagging fodder break")
  if (!data.table::is.data.table(df)) data.table::setDT(df)
  df[, qc_fodder_break := df[[item_col]] %in% .fodder_items &
    year %in% c(1984L, 1985L)]
  df
}

#' Combine individual QC flags into a single semi-colon-separated string
#'
#' @param df A tibble that may contain `qc_carry_forward`, `qc_spike`,
#'   `qc_fodder_break` columns.
#'
#' @returns The input tibble with a character column `qc_flag` and the
#'   individual boolean columns removed.
#' @keywords internal
#' @noRd
.collapse_qc_flags <- function(df) {
  force(df)
  cli::cli_progress_step("Collapsing QC flags")
  flag_cols <- intersect(
    c("qc_carry_forward", "qc_spike", "qc_fodder_break"),
    names(df)
  )

  if (!data.table::is.data.table(df)) data.table::setDT(df)
  dt <- df

  if (length(flag_cols) == 0) {
    dt[, qc_flag := NA_character_]
    return(dt)
  }

  # Build the label map
  labels <- c(
    qc_carry_forward = "carry_forward",
    qc_spike = "spike",
    qc_fodder_break = "fodder_break"
  )

  flag_matrix <- vapply(
    flag_cols,
    \(col) {
      x <- dt[[col]]
      data.table::fifelse(is.na(x), FALSE, x)
    },
    logical(nrow(dt))
  )
  if (length(flag_cols) == 1) {
    dim(flag_matrix) <- c(nrow(dt), 1)
  }

  any_flagged <- rowSums(flag_matrix) > 0L
  qc_flag <- rep(NA_character_, nrow(dt))
  if (any(any_flagged)) {
    qc_flag[any_flagged] <- apply(
      flag_matrix[any_flagged, , drop = FALSE],
      1,
      \(row) {
        paste(labels[flag_cols[row]], collapse = ";")
      }
    )
  }

  dt[, qc_flag := qc_flag]
  data.table::set(dt, j = flag_cols, value = NULL)
  dt
}

#' Smooth carry-forward tails with a simple linear trend
#'
#' For each flagged carry-forward run, replaces the constant tail with a
#' linear extrapolation from the last `anchor_years` of non-flagged data.
#' This is only applied when `smooth = TRUE` in the v2 pipeline.
#'
#' @param df A tibble with `year`, `value`, and boolean
#'   `qc_carry_forward`.
#' @param by Character vector of grouping columns.
#' @param anchor_years Number of non-flagged years to use for the linear
#'   trend (default 5).
#' @param value_col Name of the value column (default `"value"`).
#' @param time_col Name of the time column (default `"year"`).
#'
#' @returns The input tibble with carry-forward values replaced.
#' @keywords internal
#' @noRd
.smooth_carry_forward <- function(
  df,
  by,
  anchor_years = 5L,
  value_col = "value",
  time_col = "year"
) {
  if (!data.table::is.data.table(df)) data.table::setDT(df)
  dt <- df
  data.table::setorderv(dt, c(by, time_col))

  # Step 1: compute anchor flags
  dt[, c(".val", ".time", ".is_cf", ".anchor", ".n_anchor",
         ".anchor_start", ".use_anchor") := {
    val <- get(value_col)
    time <- get(time_col)
    is_cf <- !is.na(qc_carry_forward) & qc_carry_forward
    anchor <- !is_cf & !is.na(val)
    n_anchor <- cumsum(anchor)
    anchor_start <- max(0L, max(n_anchor[anchor]) - anchor_years + 1L)
    use_anchor <- anchor & n_anchor > anchor_start
    list(val, time, is_cf, anchor, n_anchor, anchor_start, use_anchor)
  }, by = by]

  # Step 2: fit linear model on anchor points and compute smoothed values
  dt[, c(".slope", ".intercept", ".smoothed") := {
    ax <- .time[.use_anchor]
    ay <- .val[.use_anchor]
    if (length(ax) >= 2) {
      fit <- stats::coef(stats::lm(ay ~ ax))
      slope <- fit[2]
      intercept <- fit[1]
    } else {
      slope <- 0
      intercept <- mean(ay, na.rm = TRUE)
    }
    smoothed <- pmax(intercept + slope * .time, 0)
    list(slope, intercept, smoothed)
  }, by = by]

  # Step 3: replace carry-forward values with smoothed values
  dt[, (value_col) := data.table::fifelse(.is_cf, .smoothed, .val)]

  # Remove temporary columns
  tmp_cols <- grep("^\\.", names(dt), value = TRUE)
  data.table::set(dt, j = tmp_cols, value = NULL)

  dt
}

# -- Print summary -------------------------------------------------------------

#' Print a short QC summary to the console
#'
#' @param df A tibble with a `qc_flag` column.
#' @param label A label for the dataset (e.g. "Primary Production").
#' @keywords internal
#' @noRd
.qc_summary <- function(df, label = "dataset") {
  if (!"qc_flag" %in% names(df)) {
    cli::cli_alert_info("{label}: no qc_flag column found")
    return(invisible(NULL))
  }
  n_total <- nrow(df)
  n_flagged <- sum(!is.na(df$qc_flag))
  pct <- round(100 * n_flagged / n_total, 2)

  cli::cli_h2("QC summary: {label}")
  cli::cli_text("Total rows: {n_total}")
  cli::cli_text("Flagged:    {n_flagged} ({pct}%)")

  if (n_flagged > 0) {
    # Break down by flag type
    flags <- df$qc_flag[!is.na(df$qc_flag)]
    all_tags <- unlist(strsplit(flags, ";"))
    tag_counts <- sort(table(all_tags), decreasing = TRUE)
    for (nm in names(tag_counts)) {
      cli::cli_text("  {nm}: {tag_counts[[nm]]}")
    }
  }
  invisible(NULL)
}
