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
  cli::cli_progress_step("Flagging carry-forwards")
  df |>
    dplyr::arrange(dplyr::across(dplyr::all_of(c(by, time_col)))) |>
    dplyr::mutate(
      .val = .data[[value_col]],
      .time = .data[[time_col]],
      .max_time = max(.time, na.rm = TRUE),
      # Run-length from the end: count backwards while value == last value
      .last_val = dplyr::last(.val),
      .from_end = rev(cumsum(rev(.val != .last_val))),
      .run_length = sum(.from_end == 0),
      qc_carry_forward = .from_end == 0 &
        .run_length >= min_run &
        .val != 0 &
        !is.na(.val),
      .by = dplyr::all_of(by)
    ) |>
    dplyr::select(
      -".val",
      -".time",
      -".max_time",
      -".last_val",
      -".from_end",
      -".run_length"
    )
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
  cli::cli_progress_step("Flagging spikes")
  df |>
    dplyr::arrange(dplyr::across(dplyr::all_of(c(by, time_col)))) |>
    dplyr::mutate(
      .val = .data[[value_col]],
      .prev = dplyr::lag(.val),
      .ratio = .val / .prev,
      qc_spike = !is.na(.ratio) &
        is.finite(.ratio) &
        (abs(.ratio) > spike_ratio | abs(.ratio) < 1 / spike_ratio) &
        .val > min_value &
        .prev > min_value,
      .by = dplyr::all_of(by)
    ) |>
    dplyr::select(-".val", -".prev", -".ratio")
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
  cli::cli_progress_step("Flagging fodder break")
  df |>
    dplyr::mutate(
      qc_fodder_break = .data[[item_col]] %in%
        .fodder_items &
        year %in% c(1984L, 1985L)
    )
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
  cli::cli_progress_step("Collapsing QC flags")
  flag_cols <- intersect(
    c("qc_carry_forward", "qc_spike", "qc_fodder_break"),
    names(df)
  )
  if (length(flag_cols) == 0) {
    return(dplyr::mutate(df, qc_flag = NA_character_))
  }

  # Build the label map
  labels <- c(
    qc_carry_forward = "carry_forward",
    qc_spike = "spike",
    qc_fodder_break = "fodder_break"
  )

  df |>
    dplyr::rowwise() |>
    dplyr::mutate(
      qc_flag = {
        active <- flag_cols[
          vapply(flag_cols, \(fc) isTRUE(.data[[fc]]), logical(1))
        ]
        if (length(active) == 0) {
          NA_character_
        } else {
          paste(labels[active], collapse = ";")
        }
      }
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-dplyr::all_of(flag_cols))
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
  df |>
    dplyr::mutate(
      .val = .data[[value_col]],
      .time = .data[[time_col]],
      .is_cf = !is.na(qc_carry_forward) & qc_carry_forward,
      # anchor = last anchor_years worth of non-flagged data
      .anchor = !.is_cf & !is.na(.val),
      .n_anchor = cumsum(.anchor),
      .anchor_start = max(0L, max(.n_anchor[.anchor]) - anchor_years + 1L),
      .use_anchor = .anchor & .n_anchor > .anchor_start,
      .by = dplyr::all_of(by)
    ) |>
    dplyr::mutate(
      # Fit simple linear model on anchor points
      .slope = {
        ax <- .time[.use_anchor]
        ay <- .val[.use_anchor]
        if (length(ax) >= 2) {
          stats::coef(stats::lm(ay ~ ax))[2]
        } else {
          0
        }
      },
      .intercept = {
        ax <- .time[.use_anchor]
        ay <- .val[.use_anchor]
        if (length(ax) >= 2) {
          stats::coef(stats::lm(ay ~ ax))[1]
        } else {
          mean(ay, na.rm = TRUE)
        }
      },
      .smoothed = .intercept + .slope * .time,
      # Floor at zero — production can't be negative
      .smoothed = pmax(.smoothed, 0),
      .by = dplyr::all_of(by)
    ) |>
    dplyr::mutate(
      !!value_col := dplyr::if_else(.is_cf, .smoothed, .val)
    ) |>
    dplyr::select(-dplyr::starts_with("."))
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
