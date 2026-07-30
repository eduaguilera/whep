#' Check footprint conservation against direct extensions.
#'
#' @description
#' Verify the master input-output accounting identity for a
#' footprint: the environmental pressure embodied across all final
#' demand and traced back to an origin sector should equal the
#' direct extension (the source pressure) of that sector.
#'
#' The footprint engine zeroes negative coefficients, caps column
#' sums, and drops near-zero-output sectors (the FABIO
#' conventions), so the identity holds only approximately. This
#' check quantifies the discrepancy per origin sector instead of
#' asserting exact equality. Crucially it detects *under-tracing*
#' (pressure that silently disappears and never reaches final
#' demand), which [compute_footprint()]'s `conserve_extensions`
#' bounding never reports because it only rescales results
#' downward.
#'
#' Only the positive side of the extensions is traced, matching the
#' engine, which traces `pmax(extensions, 0)` and ignores sectors
#' with output `<= output_tol`.
#'
#' @param footprint Footprint tibble from [compute_footprint()],
#'   with `origin_area`, `origin_item` and `value` columns.
#' @param extensions Numeric vector of environmental extensions per
#'   sector, as passed to [compute_footprint()].
#' @param labels Tibble with `area_code` and `item_cbs_code`
#'   mapping each sector to its meaning, as passed to
#'   [compute_footprint()].
#' @param x_vec Numeric vector of total output per sector.
#' @param output_tol Minimum output for a sector to be traceable.
#'   Sectors with `x_vec <= output_tol` contribute zero direct
#'   pressure, matching [compute_footprint()].
#' @param tol Relative tolerance for the conservation status.
#'   Discrepancies within `tol` of the direct pressure are `"ok"`.
#'
#' @return A tibble with one row per origin sector:
#'   - `origin_area`: Country where the pressure occurs.
#'   - `origin_item`: Item causing the pressure.
#'   - `direct`: Direct (source) extension for the sector.
#'   - `embodied`: Footprint traced back to the sector.
#'   - `discrepancy`: `embodied - direct`.
#'   - `rel_discrepancy`: `discrepancy / direct` (`NA` when
#'     `direct` is zero).
#'   - `status`: One of `"ok"`, `"under_traced"`, `"dropped"`
#'     (embodied is zero while direct is positive) or
#'     `"over_traced"`.
#'   Rows are ordered by descending absolute relative discrepancy.
#'
#' @export
#'
#' @examples
#' z_mat <- matrix(c(0, 5, 10, 0), nrow = 2)
#' x_vec <- c(100, 200)
#' y_mat <- matrix(c(85, 195), ncol = 1)
#' extensions <- c(50, 30)
#' labels <- tibble::tibble(
#'   area_code = c(1L, 1L),
#'   item_cbs_code = c(1L, 2L)
#' )
#' fp <- compute_footprint(
#'   x_vec = x_vec, y_mat = y_mat, extensions = extensions,
#'   labels = labels, z_mat = z_mat
#' )
#' check_footprint_conservation(fp, extensions, labels, x_vec)
check_footprint_conservation <- function(
  footprint,
  extensions,
  labels,
  x_vec,
  output_tol = 1e-8,
  tol = 0.01
) {
  .validate_conservation_inputs(
    footprint,
    extensions,
    labels,
    x_vec,
    output_tol,
    tol
  )

  source_tbl <- .footprint_source_totals(
    extensions,
    labels,
    x_vec,
    output_tol
  )
  embodied_tbl <- footprint |>
    dplyr::summarise(
      embodied = sum(value, na.rm = TRUE),
      .by = c(origin_area, origin_item)
    )

  source_tbl |>
    dplyr::full_join(
      embodied_tbl,
      by = c("origin_area", "origin_item")
    ) |>
    dplyr::mutate(
      direct = tidyr::replace_na(direct, 0),
      embodied = tidyr::replace_na(embodied, 0),
      discrepancy = embodied - direct,
      rel_discrepancy = .safe_rel(discrepancy, direct),
      status = .conservation_status(direct, embodied, tol)
    ) |>
    dplyr::arrange(dplyr::desc(abs(rel_discrepancy)))
}

#' Summarise a footprint conservation report.
#'
#' @description
#' Roll up the per-origin report from
#' [check_footprint_conservation()] into a single global verdict.
#'
#' @param report Tibble returned by
#'   [check_footprint_conservation()].
#'
#' @return A one-row tibble with `n_origin`, `n_ok`, `n_flagged`,
#'   `n_dropped`, `n_under_traced`, `n_over_traced`,
#'   `total_direct`, `total_embodied` and
#'   `global_rel_discrepancy`.
#'
#' @export
#'
#' @examples
#' z_mat <- matrix(c(0, 5, 10, 0), nrow = 2)
#' x_vec <- c(100, 200)
#' y_mat <- matrix(c(85, 195), ncol = 1)
#' extensions <- c(50, 30)
#' labels <- tibble::tibble(
#'   area_code = c(1L, 1L),
#'   item_cbs_code = c(1L, 2L)
#' )
#' compute_footprint(
#'   x_vec = x_vec, y_mat = y_mat, extensions = extensions,
#'   labels = labels, z_mat = z_mat
#' ) |>
#'   check_footprint_conservation(extensions, labels, x_vec) |>
#'   summarise_conservation()
summarise_conservation <- function(report) {
  .require_cols(
    report,
    c("direct", "embodied", "status"),
    "report"
  )
  report |>
    dplyr::summarise(
      n_origin = dplyr::n(),
      n_ok = sum(status == "ok"),
      n_flagged = sum(status != "ok"),
      n_dropped = sum(status == "dropped"),
      n_under_traced = sum(status == "under_traced"),
      n_over_traced = sum(status == "over_traced"),
      total_direct = sum(direct),
      total_embodied = sum(embodied),
      global_rel_discrepancy = .safe_rel(
        sum(embodied) - sum(direct),
        sum(direct)
      )
    )
}

#' Check the commodity balance sheet supply-use identity.
#'
#' @description
#' Verify that total supply equals total use for every row of a
#' wide commodity balance sheet, the fundamental accounting
#' identity behind the input-output model. Supply is
#' `production + import + stock_withdrawal`; use is
#' `export + food + feed + seed + processing + other_uses +
#' stock_addition`.
#'
#' @param cbs Wide commodity balance sheet, e.g. from
#'   [get_wide_cbs()].
#' @param tol Absolute tolerance (in the data's mass units) for a
#'   row to count as balanced.
#'
#' @return A tibble with the key columns present in `cbs` plus:
#'   - `supply`: Total supply.
#'   - `use`: Total use.
#'   - `abs_diff`: `abs(supply - use)`.
#'   - `rel_diff`: `abs_diff / supply` (`NA` when supply is zero).
#'   - `balanced`: `TRUE` when `abs_diff <= tol`.
#'   Rows are ordered by descending absolute difference.
#'
#' @export
#'
#' @examples
#' get_wide_cbs(example = TRUE) |>
#'   check_supply_use_balance()
check_supply_use_balance <- function(cbs, tol = 1e-6) {
  supply_cols <- c("production", "import", "stock_withdrawal")
  use_cols <- c(
    "export",
    "food",
    "feed",
    "seed",
    "processing",
    "other_uses",
    "stock_addition"
  )
  .require_cols(cbs, c(supply_cols, use_cols), "cbs")
  .validate_tol(tol, "tol")

  cbs |>
    dplyr::mutate(
      supply = production + import + stock_withdrawal,
      use = export +
        food +
        feed +
        seed +
        processing +
        other_uses +
        stock_addition,
      abs_diff = abs(supply - use),
      rel_diff = .safe_rel(abs_diff, supply),
      balanced = abs_diff <= tol
    ) |>
    dplyr::select(
      dplyr::any_of(c("year", "area_code", "item_cbs_code")),
      supply,
      use,
      abs_diff,
      rel_diff,
      balanced
    ) |>
    dplyr::arrange(dplyr::desc(abs_diff))
}

#' Assert that footprint conservation invariants hold.
#'
#' @description
#' Build-time gate over a footprint result, intended for use in a
#' pipeline or regression test. Aborts when the footprint
#' over-traces any origin sector (embodied pressure exceeds the
#' source, which should never happen) or when the global share of
#' untraced pressure exceeds `max_rel_loss`. A regression that
#' silently loses or inflates pressure then fails loudly instead of
#' shipping.
#'
#' @inheritParams check_footprint_conservation
#' @param max_rel_loss Maximum tolerated global relative
#'   under-tracing (pressure that never reaches final demand). The
#'   engine's negative-zeroing and column capping cause a small
#'   amount, so this is non-zero by default.
#'
#' @return Invisibly, the one-row summary from
#'   [summarise_conservation()]. Called for its side effect of
#'   aborting on violation.
#'
#' @export
#'
#' @examples
#' z_mat <- matrix(c(0, 5, 10, 0), nrow = 2)
#' x_vec <- c(100, 200)
#' y_mat <- matrix(c(85, 195), ncol = 1)
#' extensions <- c(50, 30)
#' labels <- tibble::tibble(
#'   area_code = c(1L, 1L),
#'   item_cbs_code = c(1L, 2L)
#' )
#' fp <- compute_footprint(
#'   x_vec = x_vec, y_mat = y_mat, extensions = extensions,
#'   labels = labels, z_mat = z_mat
#' )
#' assert_footprint_invariants(fp, extensions, labels, x_vec)
assert_footprint_invariants <- function(
  footprint,
  extensions,
  labels,
  x_vec,
  max_rel_loss = 0.05
) {
  .validate_tol(max_rel_loss, "max_rel_loss")
  summary <- check_footprint_conservation(
    footprint,
    extensions,
    labels,
    x_vec
  ) |>
    summarise_conservation()
  rel_loss <- -min(summary$global_rel_discrepancy, 0, na.rm = TRUE)
  if (summary$n_over_traced > 0) {
    cli::cli_abort(c(
      "Footprint over-traces {summary$n_over_traced} origin sector{?s}.",
      "i" = "Embodied pressure exceeds the source extension; this is a bug."
    ))
  }
  if (rel_loss > max_rel_loss) {
    cli::cli_abort(c(
      "Footprint loses {round(100 * rel_loss, 1)}% of source pressure.",
      "i" = "Exceeds {.arg max_rel_loss} = {round(100 * max_rel_loss, 1)}%.",
      "i" = "{summary$n_dropped} dropped, {summary$n_under_traced} under-traced."
    ))
  }
  cli::cli_alert_success(
    "Footprint invariants hold ({summary$n_ok}/{summary$n_origin} sectors ok)."
  )
  invisible(summary)
}

# --- Helpers ---

.footprint_source_totals <- function(
  extensions,
  labels,
  x_vec,
  output_tol
) {
  direct <- ifelse(x_vec <= output_tol, 0, pmax(extensions, 0))
  direct[is.na(direct)] <- 0
  tibble::tibble(
    origin_area = as.integer(labels$area_code),
    origin_item = as.integer(labels$item_cbs_code),
    direct = direct
  ) |>
    dplyr::summarise(
      direct = sum(direct),
      .by = c(origin_area, origin_item)
    )
}

.conservation_status <- function(direct, embodied, tol) {
  dplyr::case_when(
    embodied > direct * (1 + tol) ~ "over_traced",
    direct <= 0 ~ "ok",
    embodied <= 0 ~ "dropped",
    embodied < direct * (1 - tol) ~ "under_traced",
    .default = "ok"
  )
}

.safe_rel <- function(num, den) {
  ifelse(den > 0, num / den, NA_real_)
}

.validate_conservation_inputs <- function(
  footprint,
  extensions,
  labels,
  x_vec,
  output_tol,
  tol
) {
  .require_cols(
    footprint,
    c("origin_area", "origin_item", "value"),
    "footprint"
  )
  .require_cols(labels, c("area_code", "item_cbs_code"), "labels")
  n <- length(x_vec)
  if (length(extensions) != n) {
    cli::cli_abort(
      "{.arg extensions} length ({length(extensions)}) must match
      {.arg x_vec} length ({n})."
    )
  }
  if (nrow(labels) != n) {
    cli::cli_abort(
      "{.arg labels} must have {n} rows to match {.arg x_vec}."
    )
  }
  .validate_tol(output_tol, "output_tol")
  .validate_tol(tol, "tol")
}

.validate_tol <- function(value, arg) {
  if (
    !is.numeric(value) ||
      length(value) != 1 ||
      is.na(value) ||
      !is.finite(value) ||
      value < 0
  ) {
    cli::cli_abort("{.arg {arg}} must be one non-negative number.")
  }
}

.require_cols <- function(df, cols, arg) {
  missing <- cols[!rlang::has_name(df, cols)]
  if (length(missing) > 0) {
    cli::cli_abort(
      "{.arg {arg}} is missing column{?s}: {.field {missing}}."
    )
  }
}

# Warn at build time about CBS rows whose supply and use differ by more
# than `threshold` (relative), surfacing material data anomalies such as
# corrupted trade values instead of letting them flow silently.
.qc_supply_use_balance <- function(cbs, threshold = 0.5) {
  flagged <- check_supply_use_balance(cbs) |>
    dplyr::filter(!is.na(rel_diff), rel_diff > threshold)
  if (nrow(flagged) == 0) {
    return(invisible(NULL))
  }
  top <- flagged |>
    dplyr::count(item_cbs_code, sort = TRUE) |>
    dplyr::slice_head(n = 5)
  cli::cli_warn(c(
    "!" = "{nrow(flagged)} CBS row{?s} have supply-use imbalance above
      {round(100 * threshold)}%.",
    "i" = "Most-affected item code{cli::qty(nrow(top))}{?s}:
      {.val {top$item_cbs_code}}."
  ))
  invisible(flagged)
}
