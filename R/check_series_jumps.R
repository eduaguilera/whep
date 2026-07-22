# Level-2 within-series jump detector of the AFE data-validation framework.

#' Flag implausible year-on-year jumps in a time series.
#'
#' @description
#' Scan each series for consecutive observations whose ratio falls outside
#' a plausible band, the level-2 (within-series) detector of the AFE data
#' and code validation framework. It catches unjustified single-year steps,
#' spikes and the level shifts that appear where two sources are spliced,
#' while a break-year allowlist keeps documented regime changes from firing
#' as false positives.
#'
#' @details
#' This is the first landed detector of the reusable `check_*` library
#' described in the AFE data-validation framework decision
#' (`afse-wiki/wiki/decisions/afse-data-validation-framework.md`, level 2,
#' within-series). It generalises the energy-hist consecutive-year jump scan
#' (a world-total ratio scan against a break-year allowlist) into a grouped,
#' band-parameterised check usable both as an inline pipeline guard and as a
#' test backend.
#'
#' Two pieces of the framework's metadata model are exposed as arguments.
#' `bands` supplies per-variable plausible-jump bands (land area is tight,
#' yield is wide, so a single global band is wrong for a mixed panel), and
#' `allowlist` supplies the historically justified break years that are
#' reported but not treated as defects. Undocumented jumps stay flagged.
#'
#' A robust variant (flagging via the median absolute deviation of log
#' ratios, per the Hampel/MAD anchors in the framework) is a documented
#' future extension, not implemented here.
#'
#' @param data A data frame with one observation per row.
#' @param value_col The column holding the series values to scan.
#' @param time_col The column holding time values. Default: `year`.
#' @param .by A character vector of grouping columns identifying each
#'   series (optional). When `NULL`, the whole table is one series.
#' @param ratio_bounds Length-2 numeric `c(low, high)`: the default
#'   plausible band for the ratio of consecutive values. A ratio below
#'   `low` or above `high` is flagged.
#' @param bands Optional data frame of per-group band overrides: the
#'   grouping columns (a subset of `.by`) plus `lo` and `hi`. Where a group
#'   matches, its `lo`/`hi` replace `ratio_bounds` for that group.
#' @param min_value Minimum value both members of a pair must exceed to be
#'   flagged. Keeps genuine near-zero technology onsets from firing.
#'   Default: `0`.
#' @param consecutive_only Logical. If `TRUE` (default), only pairs one
#'   time step apart are scanned; larger gaps are skipped.
#' @param allowlist Optional data frame of documented break years, matched
#'   on the grouping columns plus `time_col`. Matching flags are returned
#'   with `allowlisted = TRUE` rather than dropped.
#' @param verbose Logical. If `TRUE` (default), report flag counts with
#'   `cli`.
#'
#' @return A tibble with one row per flagged jump: the grouping columns,
#'   the `time_col` of the later observation, `prev_value`, `value`,
#'   `ratio` (`value / prev_value`) and `allowlisted`. When nothing is
#'   flagged, a zero-row tibble of the same shape and column types.
#'
#' @export
#'
#' @examples
#' series <- tibble::tibble(
#'   category = rep(c("area", "yield"), each = 5),
#'   year = rep(2000:2004, times = 2),
#'   value = c(100, 101, 102, 180, 181, 3.0, 3.1, 5.2, 3.0, 3.1)
#' )
#' # area steps 102 -> 180 (1.76x); yield steps 3.1 -> 5.2 (1.68x)
#' check_series_jumps(series, value, .by = "category")
#'
#' # Widen the band for yield only, leaving area on the default:
#' bands <- tibble::tibble(category = "yield", lo = 0.4, hi = 2.5)
#' check_series_jumps(series, value, .by = "category", bands = bands)
check_series_jumps <- function(
  data,
  value_col,
  time_col = year,
  .by = NULL,
  ratio_bounds = c(0.55, 1.6),
  bands = NULL,
  min_value = 0,
  consecutive_only = TRUE,
  allowlist = NULL,
  verbose = TRUE
) {
  value_col_name <- rlang::as_name(rlang::enquo(value_col))
  time_col_name <- rlang::as_name(rlang::enquo(time_col))
  by_cols <- if (is.null(.by)) character(0) else .by

  .validate_jumps_inputs(
    data,
    value_col_name,
    time_col_name,
    by_cols,
    ratio_bounds,
    bands,
    min_value,
    allowlist
  )

  flags <- .scan_series_jumps(
    data,
    value_col_name,
    time_col_name,
    by_cols,
    ratio_bounds,
    bands,
    min_value,
    consecutive_only,
    allowlist
  )

  if (verbose) {
    .report_series_jumps(flags, ratio_bounds)
  }
  flags
}

# --- Helpers ---

.scan_series_jumps <- function(
  data,
  value_col,
  time_col,
  by_cols,
  ratio_bounds,
  bands,
  min_value,
  consecutive_only,
  allowlist
) {
  dt <- .series_consecutive_pairs(
    data,
    value_col,
    time_col,
    by_cols,
    consecutive_only
  )
  dt <- .apply_jump_bands(dt, bands, ratio_bounds, by_cols)
  flagged <- dt[
    (ratio < .lo | ratio > .hi) &
      .value_prev > min_value &
      .value_now > min_value
  ]
  flagged <- .mark_allowlisted(flagged, allowlist, by_cols, time_col)
  .shape_jump_flags(flagged, by_cols, time_col)
}

# Build the table of consecutive-observation pairs per series, carrying the
# previous value and the ratio. Optionally restrict to single-step pairs.
.series_consecutive_pairs <- function(
  data,
  value_col,
  time_col,
  by_cols,
  consecutive_only
) {
  keep <- unique(c(by_cols, time_col, value_col))
  dt <- data.table::as.data.table(data)[, ..keep]
  dt[, .value_now := as.double(get(value_col))]
  dt[, .time_num := as.double(get(time_col))]
  data.table::setorderv(dt, c(by_cols, ".time_num"))

  lag_by <- if (length(by_cols) > 0) by_cols else NULL
  dt[, .value_prev := data.table::shift(.value_now), by = lag_by]
  dt[, .time_prev := data.table::shift(.time_num), by = lag_by]

  dt <- dt[!is.na(.value_prev) & !is.na(.time_prev)]
  if (consecutive_only) {
    dt <- dt[(.time_num - .time_prev) == 1]
  }
  dt[, ratio := .value_now / .value_prev]
  dt[]
}

# Set the per-row plausible band, overriding the global default with any
# per-group band supplied via `bands`.
.apply_jump_bands <- function(dt, bands, ratio_bounds, by_cols) {
  dt[, `:=`(.lo = ratio_bounds[1], .hi = ratio_bounds[2])]
  if (is.null(bands)) {
    return(dt[])
  }
  band_dt <- data.table::as.data.table(bands)
  band_keys <- setdiff(names(band_dt), c("lo", "hi"))
  data.table::setnames(band_dt, c("lo", "hi"), c("band_lo", "band_hi"))
  if (length(band_keys) == 0) {
    dt[, `:=`(.lo = band_dt$band_lo[1], .hi = band_dt$band_hi[1])]
  } else {
    dt[band_dt, on = band_keys, `:=`(.lo = i.band_lo, .hi = i.band_hi)]
  }
  dt[]
}

# Add the `allowlisted` column, TRUE for flags matching a documented break
# year (grouping columns plus time), FALSE otherwise. Flags are kept either
# way.
.mark_allowlisted <- function(flagged, allowlist, by_cols, time_col) {
  flagged[, allowlisted := FALSE]
  if (is.null(allowlist)) {
    return(flagged[])
  }
  allow_keys <- c(by_cols, time_col)
  allow_dt <- unique(data.table::as.data.table(allowlist)[, ..allow_keys])
  matched <- flagged[allow_dt, on = allow_keys, which = TRUE, nomatch = NULL]
  if (length(matched) > 0) {
    flagged[matched, allowlisted := TRUE]
  }
  flagged[]
}

# Select and rename the output columns into the reporting shape, ordered by
# series then time. Preserves column types on a zero-row result.
.shape_jump_flags <- function(flagged, by_cols, time_col) {
  data.table::setnames(
    flagged,
    c(".value_prev", ".value_now"),
    c("prev_value", "value")
  )
  out_cols <- c(
    by_cols,
    time_col,
    "prev_value",
    "value",
    "ratio",
    "allowlisted"
  )
  result <- flagged[, ..out_cols]
  data.table::setorderv(result, c(by_cols, time_col))
  tibble::as_tibble(result)
}

# Report the flag counts (undocumented vs allowlisted) in the check_* idiom.
.report_series_jumps <- function(flags, ratio_bounds) {
  n <- nrow(flags)
  if (n == 0) {
    cli::cli_alert_success(
      "check_series_jumps: no jumps outside
      [{ratio_bounds[1]}, {ratio_bounds[2]}]."
    )
    return(invisible(NULL))
  }
  n_allow <- sum(flags$allowlisted)
  n_new <- n - n_allow
  cli::cli_alert_info(
    "check_series_jumps: {n} jump{?s} flagged
    ({n_new} undocumented, {n_allow} allowlisted)."
  )
  invisible(NULL)
}

.validate_jumps_inputs <- function(
  data,
  value_col,
  time_col,
  by_cols,
  ratio_bounds,
  bands,
  min_value,
  allowlist
) {
  .require_cols(data, c(by_cols, time_col, value_col), "data")
  if (
    !is.numeric(ratio_bounds) ||
      length(ratio_bounds) != 2 ||
      anyNA(ratio_bounds) ||
      any(ratio_bounds <= 0) ||
      ratio_bounds[1] >= ratio_bounds[2]
  ) {
    cli::cli_abort(
      "{.arg ratio_bounds} must be two positive numbers, low then high."
    )
  }
  .validate_tol(min_value, "min_value")
  if (!is.null(bands)) {
    .validate_bands(bands, by_cols)
  }
  if (!is.null(allowlist)) {
    .require_cols(allowlist, c(by_cols, time_col), "allowlist")
  }
}

.validate_bands <- function(bands, by_cols) {
  .require_cols(bands, c("lo", "hi"), "bands")
  band_keys <- setdiff(names(bands), c("lo", "hi"))
  extra <- band_keys[!band_keys %in% by_cols]
  if (length(extra) > 0) {
    cli::cli_abort(
      "{.arg bands} group column{?s} {.field {extra}} not in {.arg .by}."
    )
  }
}
