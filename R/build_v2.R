#' Build primary production dataset (v2 — with QC flags)
#'
#' @description
#' Runs the v1 [build_primary_production()] pipeline, then applies a
#' data-quality layer that detects:
#'
#' * **carry_forward** — constant-value tails at the end of a series
#'   (typically FAOSTAT carry-forward imputations).
#' * **spike** — year-on-year changes exceeding a threshold (e.g. 10×).
#' * **fodder_break** — the known 1984 / 1985 discontinuity in several
#'   European countries' fodder / forage reporting.
#'
#' The original v1 output is returned unchanged in all value columns;
#' the only additions are the `qc_flag` column and, when
#' `smooth_carry_forward = TRUE`, a trend-based replacement of the
#' flagged carry-forward tail.
#'
#' @inheritParams build_primary_production
#' @param smooth_carry_forward Logical.
#'   If `TRUE`, carry-forward tails are replaced with a linear trend
#'   extrapolated from the preceding `anchor_years` of non-flagged data.
#'   Default `FALSE` (flag only, no value change).
#' @param anchor_years Integer. Number of preceding years used for the
#'   linear-trend extrapolation when `smooth_carry_forward = TRUE`.
#'   Default `5`.
#' @param spike_ratio Numeric. Year-on-year ratio threshold: rows where
#'   `current / previous > spike_ratio` (or `< 1 / spike_ratio`) and
#'   both values exceed `spike_min` are flagged.  Default `10`.
#' @param spike_min Numeric. Minimum absolute value on both sides of a
#'   year-on-year comparison for a spike to be flagged.  Default `1000`.
#' @param carry_forward_run Integer. Minimum number of identical
#'   consecutive final-year values to qualify as a carry-forward flag.
#'   Default `3`.
#'
#' @returns A tibble with the same columns as [build_primary_production()]
#'   **plus** a character column `qc_flag` that is `NA` for clean rows
#'   or contains semicolon-separated labels (`"carry_forward"`,
#'   `"spike"`, `"fodder_break"`).
#'
#'   When `smooth_carry_forward = TRUE`, the `value` column is modified
#'   for flagged carry-forward rows; the original value is preserved in
#'   an attribute `"original_carry_forward"` (a tibble of the replaced
#'   rows with their original value).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' primary_v2 <- build_primary_production_v2()
#' primary_v2 |> dplyr::filter(!is.na(qc_flag))
#' }
build_primary_production_v2 <- function(
    max_year = 2021,
    version = NULL,
    smooth_carry_forward = FALSE,
    anchor_years = 5L,
    spike_ratio = 10,
    spike_min = 1000,
    carry_forward_run = 3L
) {
  # ---- Run v1 pipeline (untouched) ----
  primary <- build_primary_production(
    max_year = max_year,
    version = version
  )

  # ---- QC flags ----
  by_cols <- c(
    "area", "area_code",
    "item_prod", "item_code_prod", "unit"
  )

  primary <- primary |>
    .flag_carry_forward(
      by = by_cols,
      min_run = carry_forward_run,
      value_col = "value",
      time_col = "year"
    ) |>
    .flag_spikes(
      by = by_cols,
      spike_ratio = spike_ratio,
      min_value = spike_min,
      value_col = "value",
      time_col = "year"
    ) |>
    .flag_fodder_break(
      item_col = "item_prod",
      value_col = "value"
    )

  # ---- Optionally smooth carry-forward tails ----
  original_cf <- NULL
  if (smooth_carry_forward) {
    original_cf <- primary |>
      dplyr::filter(
        !is.na(qc_carry_forward) & qc_carry_forward
      ) |>
      dplyr::select(
        dplyr::all_of(c(by_cols, "year", "value"))
      )

    primary <- .smooth_carry_forward(
      primary,
      by = by_cols,
      anchor_years = anchor_years,
      value_col = "value",
      time_col = "year"
    )
  }

  # ---- Collapse flags into a single column ----
  primary <- .collapse_qc_flags(primary)

  # ---- Summary ----
  .qc_summary(primary, "Primary Production v2")

  if (!is.null(original_cf) && nrow(original_cf) > 0) {
    attr(primary, "original_carry_forward") <- original_cf
  }

  primary
}


#' Build commodity balance sheets (v2 — with QC flags)
#'
#' @description
#' Runs the v1 [build_commodity_balances()] pipeline, then applies a
#' data-quality layer that detects:
#'
#' * **carry_forward** — constant-value tails (for Production element).
#' * **spike** — extreme year-on-year Production changes.
#'
#' Fodder-break flags are not applied to CBS because the CBS pipeline
#' already aggregates items differently.
#'
#' @inheritParams build_commodity_balances
#' @param smooth_carry_forward Logical. Default `FALSE`.
#' @param anchor_years Integer. Default `5`.
#' @param spike_ratio Numeric. Default `10`.
#' @param spike_min Numeric. Default `1000`.
#' @param carry_forward_run Integer. Default `3`.
#'
#' @returns A tibble with the same columns as [build_commodity_balances()]
#'   **plus** `qc_flag`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' primary <- build_primary_production()
#' cbs_v2 <- build_commodity_balances_v2(primary)
#' cbs_v2 |> dplyr::filter(!is.na(qc_flag))
#' }
build_commodity_balances_v2 <- function(
    primary_all,
    max_year = 2021,
    version = NULL,
    smooth_carry_forward = FALSE,
    anchor_years = 5L,
    spike_ratio = 10,
    spike_min = 1000,
    carry_forward_run = 3L
) {
  # ---- Run v1 pipeline (untouched) ----
  cbs <- build_commodity_balances(
    primary_all = primary_all,
    max_year = max_year,
    version = version
  )

  # ---- QC flags (applied only to the Production element) ----
  # Carry-forward and spikes are most meaningful for Production;
  # derived elements like Food/Feed follow different logic.
  by_cols <- c(
    "area", "area_code",
    "item_cbs", "item_code_cbs", "element"
  )

  cbs <- cbs |>
    .flag_carry_forward(
      by = by_cols,
      min_run = carry_forward_run,
      value_col = "value",
      time_col = "year"
    ) |>
    .flag_spikes(
      by = by_cols,
      spike_ratio = spike_ratio,
      min_value = spike_min,
      value_col = "value",
      time_col = "year"
    )

  # ---- Optionally smooth carry-forward tails ----
  original_cf <- NULL
  if (smooth_carry_forward) {
    original_cf <- cbs |>
      dplyr::filter(
        !is.na(qc_carry_forward) & qc_carry_forward
      ) |>
      dplyr::select(
        dplyr::all_of(c(by_cols, "year", "value"))
      )

    cbs <- .smooth_carry_forward(
      cbs,
      by = by_cols,
      anchor_years = anchor_years,
      value_col = "value",
      time_col = "year"
    )
  }

  # ---- Collapse flags ----
  cbs <- .collapse_qc_flags(cbs)

  # ---- Summary ----
  .qc_summary(cbs, "Commodity Balance Sheets v2")

  if (!is.null(original_cf) && nrow(original_cf) > 0) {
    attr(cbs, "original_carry_forward") <- original_cf
  }

  cbs
}
