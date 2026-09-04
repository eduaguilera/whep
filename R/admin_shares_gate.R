# NSE globals for admin_shares_gate.R (#1000):
# c(
#   "area_code", "level", "item_prod_code", "level_polity_code", "year",
#   "share", "treatment", "seam_year", "seam_kind", "seam_kinds", "t0",
#   "n_units", "n_observed", "share_sum", "max_rel_diff", "basis",
#   "seam_start_year", "matches_seam_start", "pass", "reason",
#   "prev_year", "prev_share", "prev_treatment", "log_ratio",
#   "q_reference", "n_reference", "beyond_quantile", "status", "n_gated",
#   "n_beyond", "frac_beyond", "threshold", "gate_status",
#   "reported_value", "value_share", "value_total",
#   "rel_diff", "crop_name", "harvested_ha", "cell_share", "regime",
#   "n_regime", "n_years", "n_regime_mismatch", "regime_checked", "role",
#   "y1", "y2", "n_series", "n_flag", "flag_rate", "max_ha", "n",
#   "offset", "flag_rate_prev", "flag_rate_seam", "flag_rate_next",
#   "neighbour_rate", "excess"
# )

#' Tolerances of the admin-shares seam gate
#'
#' @description
#' The user-settable numbers [seam_gate()] judges with, in one place so a
#' run records what it was gated against. Each is a **proposal** of the
#' subnational-spatialization plan's T29 and may be replaced by its T31
#' allocation-policy decision; none is a measured quantity and none is
#' derived from data, so each is stated here with the reason it has the
#' value it has rather than left at a call site.
#'
#' - `identity_rel` (1e-8): tier A's relative tolerance on a unit-share
#'   total, the plan's figure. The back-cast leaves the anchor year's
#'   observed rows untouched, so the only thing between the two sides is
#'   floating-point summation of at most a few hundred shares, which
#'   1e-8 is orders of magnitude above.
#' - `identity_floor` (1e-12): denominator guard for that relative
#'   comparison, so a unit whose observed share rounds to zero does not
#'   turn a difference of 1e-18 into a ratio of 1e6. A numerical guard,
#'   not a scientific threshold.
#' - `reference_quantile` (0.95): tier B's reference point, the plan's
#'   Q95 of the observed consecutive log-ratio distribution. The null
#'   exceedance rate `null_rate` follows from it as `1 - quantile` and is
#'   returned rather than set, so the two cannot disagree.
#' - `binomial_sigma` (2): how many standard errors of slack the tier-B
#'   gate allows above `null_rate`, giving the plan's
#'   `0.05 + 2 * sqrt(0.05 * 0.95 / n)`. Two standard errors is a ~95%
#'   one-sided band under the null that a seam pair is an ordinary pair.
#' - `min_reference` (20): the smallest reference sample a quantile is
#'   taken from. Below it the Q95 is essentially the largest one or two
#'   observed ratios, so the series is reported unevaluable instead of
#'   being judged against a threshold that is itself noise. Assumed,
#'   unverified: no measurement fixes it, and T31 may.
#' - `cell_ratio_bounds` (`c(0.55, 1.6)`): tier C's plausible band,
#'   [check_series_jumps()]'s own default, kept so the cell smoke test
#'   speaks the same language as the rest of the `check_*` library.
#' - `cell_min_ha` (100): tier C's hectare floor. A cell holding under a
#'   square kilometre of a crop in both years of a pair is dropped before
#'   the scan, its share being numerically tiny and its ratio dominated
#'   by rounding. The same 100 ha [backcast_admin_shares()] inherits from
#'   `.fix_luh2_crop_collapse()`'s `min_neighbor_mha = 0.001` Mha.
#'   [check_series_jumps()]'s `min_value` cannot express it: that applies
#'   to the scanned column, which here is a share, not an area.
#' - `cell_excess` (0.01): the plan's one percentage point. Tier C passes
#'   when the seam pair's flag rate exceeds the mean of its two
#'   neighbouring pairs' rates by no more than this.
#'
#' Two further numbers are **derived** from those, and returned rather
#' than set, so they cannot disagree with them:
#'
#' - `null_rate`, the tier-B null exceedance rate `1 - quantile`.
#' - `min_gated`, the smallest number of gated pairs a container gate is
#'   pronounced on. Below it the gate is arithmetically incapable of
#'   passing a container with a single exceeding pair: one pair out of
#'   `n` is a rate of `1/n`, and `1/n` exceeds the band whenever
#'   `1/n > null_rate + binomial_sigma * sqrt(...)`. At the defaults that
#'   is every `n` up to 3, so `min_gated` is 4 and a container with three
#'   gated pairs is reported `"too_few_pairs"` rather than failed. This
#'   is a property of the band, not a preference.
#'
#' @param identity_rel Relative tolerance for tier A.
#' @param identity_floor Denominator guard for tier A.
#' @param reference_quantile Reference quantile for tier B, in (0, 1).
#' @param binomial_sigma Standard errors of slack in tier B's gate.
#' @param min_reference Smallest reference sample tier B will judge
#'   against.
#' @param cell_ratio_bounds Length-2 plausible band for tier C, passed to
#'   [check_series_jumps()].
#' @param cell_min_ha Hectare floor applied to tier C's cells before the
#'   scan.
#' @param cell_excess Largest flag-rate excess tier C accepts, as a
#'   fraction.
#'
#' @return A named list of the arguments plus `null_rate` and
#'   `min_gated`.
#'
#' @export
#'
#' @examples
#' seam_gate_tolerances()$null_rate
#' seam_gate_tolerances(reference_quantile = 0.99)$null_rate
seam_gate_tolerances <- function(
  identity_rel = 1e-8,
  identity_floor = 1e-12,
  reference_quantile = 0.95,
  binomial_sigma = 2,
  min_reference = 20L,
  cell_ratio_bounds = c(0.55, 1.6),
  cell_min_ha = 100,
  cell_excess = 0.01
) {
  .validate_tol(identity_rel, "identity_rel")
  .validate_tol(identity_floor, "identity_floor")
  .validate_tol(binomial_sigma, "binomial_sigma")
  .validate_tol(min_reference, "min_reference")
  .validate_tol(cell_min_ha, "cell_min_ha")
  .validate_tol(cell_excess, "cell_excess")
  .sg_check_quantile(reference_quantile)
  .sg_check_bounds(cell_ratio_bounds)
  list(
    identity_rel = identity_rel,
    identity_floor = identity_floor,
    reference_quantile = reference_quantile,
    null_rate = 1 - reference_quantile,
    min_gated = .sg_min_gated(1 - reference_quantile, binomial_sigma),
    binomial_sigma = binomial_sigma,
    min_reference = min_reference,
    cell_ratio_bounds = cell_ratio_bounds,
    cell_min_ha = cell_min_ha,
    cell_excess = cell_excess
  )
}

#' Gate an admin-shares back-cast at its seams
#'
#' @description
#' Judge a back-cast share table at every seam the resolver found, in the
#' three tiers the subnational-spatialization plan's seam section asks
#' for. Nothing is aborted and nothing is repaired: each tier returns its
#' own table with a `pass` column and the numbers behind it, and the
#' caller decides what a failure means. That is deliberate. The gate is
#' evidence for a release decision, not a guard inside a pipeline.
#'
#' Seam years are read from `seams`, never assumed. This function
#' contains no year: a run whose statistics start in 1850 and one whose
#' statistics start in 1974 are gated by the same code at different
#' years.
#'
#' @section Tier A -- identity at the anchor:
#' At each series' `t0` -- the first year carrying an observed row in
#' `shares_backcast`, which is the anchor the back-cast actually used --
#' the table must still hold the observation:
#'
#' - every unit's row at `t0` is `treatment == "observed"`;
#' - the unit shares at `t0` sum to 1 within `identity_rel`;
#' - where the anchor rows carry `value`, each share equals that value's
#'   share of the anchor-year total within `identity_rel` (`basis =
#'   "value"`; `"share_only"` where any anchor value is missing, as it is
#'   for a residual pseudo-unit, and then only the checks above bite);
#' - `t0` is the `"start"` seam `seams` names for that series.
#'
#' What tier A cannot see is the modulation itself: the extent table is
#' not an argument, so `E_u(t0) / E_u(t0) = 1` is verified by
#' [backcast_admin_shares()]'s own tests, not here. The last check is the
#' one that catches a `shares_backcast` and a `seams` built from
#' different runs -- and a deliberate `seam =` override, which moves `t0`
#' on purpose, so a temporal hold-out is expected to fail it and says so
#' in `reason`.
#'
#' @section Tier B -- the governed quantity:
#' The seam log-ratio `|log(s_u(t0) / s_u(t0 - 1))|` of every unit at
#' every seam year, judged against the empirical distribution of the
#' **observed** consecutive log-ratios of the same `(container, level,
#' item)`, pooled over its units. Gated pairs are held out of their own
#' reference distribution.
#'
#' The statistic is the fraction of gated pairs beyond that
#' distribution's `reference_quantile`, aggregated per container across
#' its items, and the gate is
#' `null_rate + binomial_sigma * sqrt(null_rate * (1 - null_rate) / n)`:
#' under the null that a seam pair is an ordinary pair the fraction is
#' binomial, and this is its upper band. A container whose seams are
#' invisible against its own year-to-year variation passes; one whose
#' seams step further than its ordinary years do, does not.
#'
#' Every seam kind in `seams` is gated, coverage changes and indicator
#' switches included: the seam list is taken as given rather than
#' filtered, so a kind added upstream is gated without a change here. A
#' pair that cannot be judged is reported with its reason in `status`
#' (`"na_share"`, `"zero_share"`, `"no_previous_row"`, `"no_reference"`)
#' and left out of the statistic rather than counted as a pass.
#'
#' A unit that *stops* reporting at a seam year has no `s_u(t0)` and so
#' no pair at all: it is not a row of `tier_b`. Its mass does not vanish
#' from the gate, because it moves into its siblings' shares, whose
#' ratios at that year are gated.
#'
#' @section Tier C -- cell smoke:
#' Only when `cells` is supplied. [check_series_jumps()] on each cell's
#' share of the national total, across three pairs per gated seam year:
#' `(t0-2, t0-1)`, `(t0-1, t0)` and `(t0, t0+1)`. Dividing by the
#' national total is what takes the national series' own splice out of
#' the comparison; that splice is not this feature's to answer for. The
#' gate is that the seam pair's flag rate exceeds the mean of its
#' neighbours' by at most `cell_excess`.
#'
#' The scan runs one pair at a time, on a table pre-subset to that pair's
#' two years, so the flags returned are exactly that pair's and the
#' memory bound is two years of cells rather than the whole run. Cells
#' under `cell_min_ha` in **both** years are dropped first.
#'
#' Tier C is keyed on `(container, seam year)`, not on the item: the
#' crop-level engine output carries `crop_name`, not `item_prod_code`, so
#' there is nothing to join a per-item seam to. Pass the crop-level
#' output, never the CFT aggregation, whose rows pool several items.
#'
#' Where `cells` carries a `regime` column -- the within-unit weight
#' regime the allocation records per (unit, item, year) -- it must be
#' identical on both sides of every gated pair. A regime flip at a seam
#' fails tier C on its own, whatever the flag rates do, because a cell
#' series that changes weight regime at the seam is discontinuous by
#' construction rather than by measurement.
#'
#' @param shares_backcast A back-cast share table, the `shares` element
#'   of [backcast_admin_shares()]. Columns: `area_code`, `level`,
#'   `item_prod_code`, `level_polity_code`, `year`, `share`, `treatment`,
#'   optionally `value`. One row per unit and year.
#' @param seams A seam list, the `seams` element of
#'   [resolve_admin_shares()]: `area_code`, `level`, `item_prod_code`,
#'   `seam_year`, `seam_kind`.
#' @param cells Optional crop-level engine output for tier C: `lon`,
#'   `lat`, `year`, `area_code`, `crop_name`, and either `harvested_ha`
#'   or `rainfed_ha` plus `irrigated_ha`. `polycell_id`, `cell_id`,
#'   `level_polity_code` and `regime` are used when present. `NULL`
#'   (default) leaves tier C unevaluated.
#' @param tolerances The numbers to judge with, from
#'   [seam_gate_tolerances()].
#'
#' @return A list:
#'   - `tier_a`: one row per series, with `t0`, `n_units`, `n_observed`,
#'     `share_sum`, `max_rel_diff`, `basis`, `seam_start_year`,
#'     `matches_seam_start`, `pass` and `reason`.
#'   - `tier_b`: one row per gated unit-seam pair, with `log_ratio`,
#'     `q_reference`, `n_reference`, `beyond_quantile` and `status`, plus
#'     the container gate (`n_gated`, `n_beyond`, `frac_beyond`,
#'     `threshold`, `gate_status`, `pass`) broadcast onto every row of
#'     that container.
#'   - `tier_c`: one row per gated `(area_code, seam_year)`, with the
#'     three pairs' scanned series counts and flag rates,
#'     `neighbour_rate`, `excess`, `n_regime_mismatch`, `pass` and
#'     `reason`. Zero rows when `cells` is `NULL`.
#'   - `verdict`: a named logical, `tier_a` / `tier_b` / `tier_c` /
#'     `overall`. A tier is `TRUE` when every gate it evaluated passed,
#'     `FALSE` when any failed and `NA` when it evaluated none.
#'     `overall` is `FALSE` if any tier failed, `NA` if none was
#'     evaluable, else `TRUE`.
#'
#' @export
#'
#' @examples
#' # Two units back-cast to 1898 from an anchor at 1900, with the seam
#' # list the resolver would have emitted for that series.
#' shares <- tibble::tibble(
#'   area_code = 900L,
#'   level = 1L,
#'   item_prod_code = 15L,
#'   level_polity_code = rep(c("A1", "A2"), each = 3),
#'   year = rep(1898:1900, times = 2),
#'   share = c(0.36, 0.38, 0.40, 0.64, 0.62, 0.60),
#'   treatment = rep(
#'     c("backcast_t0_geometry", "backcast_t0_geometry", "observed"),
#'     times = 2
#'   )
#' )
#' seams <- tibble::tibble(
#'   area_code = 900L,
#'   level = 1L,
#'   item_prod_code = 15L,
#'   seam_year = 1900L,
#'   seam_kind = "start"
#' )
#' seam_gate(shares, seams)$tier_a
seam_gate <- function(
  shares_backcast,
  seams,
  cells = NULL,
  tolerances = seam_gate_tolerances()
) {
  tolerances <- .sg_validate_tolerances(tolerances)
  .sg_validate_shares(shares_backcast)
  .sg_validate_seams(seams)
  tier_a <- .sg_tier_a(shares_backcast, seams, tolerances)
  tier_b <- .sg_tier_b(shares_backcast, seams, tolerances)
  tier_c <- .sg_tier_c(cells, seams, tolerances)
  verdict <- .sg_verdict(tier_a, tier_b, tier_c)
  .sg_report(tier_a, tier_b, tier_c, verdict)
  list(tier_a = tier_a, tier_b = tier_b, tier_c = tier_c, verdict = verdict)
}

# --- Keys and input validation ------------------------------------------------

.sg_series_key <- function() {
  c("area_code", "level", "item_prod_code")
}

.sg_unit_key <- function() {
  c(.sg_series_key(), "level_polity_code")
}

# The smallest gated-pair count at which a single exceeding pair does not
# on its own put the container over the band. Searched rather than
# solved: the closed form is a quadratic in `1 / sqrt(n)` and reads far
# worse than the inequality it comes from.
.sg_min_gated <- function(null_rate, sigma) {
  n <- seq_len(1000L)
  band <- null_rate + sigma * sqrt(null_rate * (1 - null_rate) / n)
  passable <- n[1 / n <= band]
  if (length(passable) == 0L) {
    return(NA_integer_)
  }
  min(passable)
}

.sg_check_quantile <- function(value) {
  ok <- is.numeric(value) &&
    length(value) == 1 &&
    !is.na(value) &&
    value > 0 &&
    value < 1
  if (!ok) {
    cli::cli_abort(
      "{.arg reference_quantile} must be one number strictly in (0, 1)."
    )
  }
}

.sg_check_bounds <- function(bounds) {
  ok <- is.numeric(bounds) &&
    length(bounds) == 2 &&
    !anyNA(bounds) &&
    all(bounds > 0) &&
    bounds[1] < bounds[2]
  if (!ok) {
    cli::cli_abort(
      "{.arg cell_ratio_bounds} must be two positive numbers, low then high."
    )
  }
}

# Re-run the constructor on the supplied list, so a hand-built list is
# validated by exactly the code that documents the defaults and cannot
# reach the gate missing `null_rate` or carrying an inconsistent one.
.sg_validate_tolerances <- function(tolerances) {
  if (!is.list(tolerances)) {
    cli::cli_abort("{.arg tolerances} must be a list.")
  }
  settable <- names(formals(seam_gate_tolerances))
  derived <- c("null_rate", "min_gated")
  extra <- setdiff(names(tolerances), c(settable, derived))
  if (length(extra) > 0) {
    cli::cli_abort(
      "{.arg tolerances} has unknown element{?s} {.field {extra}}."
    )
  }
  do.call(
    seam_gate_tolerances,
    tolerances[intersect(names(tolerances), settable)]
  )
}

.sg_validate_shares <- function(shares) {
  .require_cols(
    shares,
    c(.sg_unit_key(), "year", "share", "treatment"),
    "shares_backcast"
  )
  if (!is.numeric(shares$year)) {
    cli::cli_abort("{.arg shares_backcast} needs a numeric {.field year}.")
  }
  dup <- shares |>
    dplyr::summarise(
      n = dplyr::n(),
      .by = dplyr::all_of(c(.sg_unit_key(), "year"))
    ) |>
    dplyr::filter(n > 1)
  if (nrow(dup) > 0) {
    cli::cli_abort(c(
      "{.arg shares_backcast} repeats {nrow(dup)} unit-year row{?s}.",
      i = "One resolved indicator binds per year; a union of indicators is
           not a gateable series."
    ))
  }
  invisible(NULL)
}

.sg_validate_seams <- function(seams) {
  .require_cols(seams, c(.sg_series_key(), "seam_year", "seam_kind"), "seams")
  if (!is.numeric(seams$seam_year)) {
    cli::cli_abort("{.arg seams} needs a numeric {.field seam_year}.")
  }
  invisible(NULL)
}

.sg_validate_cells <- function(cells) {
  .require_cols(
    cells,
    c("lon", "lat", "year", "area_code", "crop_name"),
    "cells"
  )
  has_area <- rlang::has_name(cells, "harvested_ha") ||
    all(rlang::has_name(cells, c("rainfed_ha", "irrigated_ha")))
  if (!has_area) {
    cli::cli_abort(c(
      "{.arg cells} has no harvested area.",
      i = "Supply {.field harvested_ha}, or {.field rainfed_ha} and
           {.field irrigated_ha} as the crop-level engine output does."
    ))
  }
  invisible(NULL)
}

# --- Tier A: identity at the anchor -------------------------------------------

.sg_tier_a <- function(shares, seams, tol) {
  anchors <- .sg_anchor_years(shares)
  if (nrow(anchors) == 0L) {
    return(.sg_tier_a_prototype())
  }
  shares |>
    dplyr::inner_join(anchors, by = .sg_series_key()) |>
    dplyr::filter(year == t0) |>
    .sg_anchor_value_share(tol) |>
    .sg_anchor_summarise() |>
    dplyr::left_join(.sg_start_seams(seams), by = .sg_series_key()) |>
    .sg_anchor_verdict(tol)
}

# `t0` is the anchor the back-cast used, which is the first year it left
# observed. Reading it off the table rather than off `seams` is what lets
# the two be compared instead of assumed equal.
.sg_anchor_years <- function(shares) {
  observed <- dplyr::filter(shares, treatment == "observed")
  if (nrow(observed) == 0L) {
    return(dplyr::mutate(
      .sg_distinct_keys(observed, .sg_series_key()),
      t0 = integer()
    ))
  }
  dplyr::summarise(
    observed,
    t0 = min(year),
    .by = dplyr::all_of(.sg_series_key())
  )
}

.sg_distinct_keys <- function(data, keys) {
  dplyr::distinct(dplyr::select(data, dplyr::all_of(keys)))
}

.sg_start_seams <- function(seams) {
  starts <- dplyr::filter(seams, seam_kind == "start")
  if (nrow(starts) == 0L) {
    return(dplyr::mutate(
      .sg_distinct_keys(starts, .sg_series_key()),
      seam_start_year = integer()
    ))
  }
  dplyr::summarise(
    starts,
    seam_start_year = min(seam_year),
    .by = dplyr::all_of(.sg_series_key())
  )
}

# The share the anchor row's own reported value implies, where every unit
# of the anchor year has one. A residual pseudo-unit has none, and then
# the whole series falls back to the share column alone.
.sg_anchor_value_share <- function(rows, tol) {
  reported <- if (rlang::has_name(rows, "value")) {
    as.numeric(rows$value)
  } else {
    rep(NA_real_, nrow(rows))
  }
  rows |>
    dplyr::mutate(reported_value = reported) |>
    dplyr::mutate(
      value_total = sum(reported_value),
      .by = dplyr::all_of(.sg_series_key())
    ) |>
    dplyr::mutate(
      value_share = dplyr::if_else(
        !is.na(value_total) & value_total > 0,
        reported_value / value_total,
        NA_real_
      ),
      rel_diff = abs(share - value_share) /
        pmax(abs(value_share), tol$identity_floor)
    )
}

.sg_anchor_summarise <- function(rows) {
  rows |>
    dplyr::summarise(
      t0 = min(t0),
      n_units = dplyr::n(),
      n_observed = sum(treatment == "observed", na.rm = TRUE),
      share_sum = sum(share),
      max_rel_diff = .sg_full_max(rel_diff),
      basis = if (all(!is.na(rel_diff))) "value" else "share_only",
      .by = dplyr::all_of(.sg_series_key())
    )
}

# `max()` only where every element is present: a partial maximum would
# claim a value basis the series does not have.
.sg_full_max <- function(x) {
  if (length(x) == 0L || anyNA(x)) {
    return(NA_real_)
  }
  max(x)
}

.sg_anchor_verdict <- function(anchors, tol) {
  anchors |>
    dplyr::mutate(
      matches_seam_start = dplyr::if_else(
        is.na(seam_start_year),
        NA,
        t0 == seam_start_year
      ),
      reason = dplyr::case_when(
        n_observed < n_units ~ "anchor_row_not_observed",
        is.na(share_sum) ~ "anchor_share_missing",
        abs(share_sum - 1) > tol$identity_rel ~ "anchor_shares_not_unit_sum",
        !dplyr::coalesce(max_rel_diff <= tol$identity_rel, TRUE) ~
          "anchor_share_disagrees_with_value",
        !dplyr::coalesce(matches_seam_start, TRUE) ~ "anchor_not_seam_start",
        .default = ""
      ),
      pass = reason == ""
    ) |>
    dplyr::select(dplyr::all_of(.sg_tier_a_cols()))
}

.sg_tier_a_cols <- function() {
  c(
    .sg_series_key(),
    "t0",
    "n_units",
    "n_observed",
    "share_sum",
    "max_rel_diff",
    "basis",
    "seam_start_year",
    "matches_seam_start",
    "pass",
    "reason"
  )
}

.sg_tier_a_prototype <- function() {
  tibble::tibble(
    area_code = integer(),
    level = integer(),
    item_prod_code = integer(),
    t0 = integer(),
    n_units = integer(),
    n_observed = integer(),
    share_sum = numeric(),
    max_rel_diff = numeric(),
    basis = character(),
    seam_start_year = integer(),
    matches_seam_start = logical(),
    pass = logical(),
    reason = character()
  )
}

# --- Tier B: the seam log-ratio against the ordinary ones ---------------------

.sg_tier_b <- function(shares, seams, tol) {
  gates <- .sg_seam_years(seams)
  if (nrow(gates) == 0L) {
    return(.sg_tier_b_prototype())
  }
  pairs <- .sg_consecutive_pairs(shares)
  reference <- .sg_reference_quantiles(pairs, gates, tol)
  .sg_gated_pairs(shares, pairs, gates) |>
    dplyr::left_join(reference, by = .sg_series_key()) |>
    .sg_score_pairs(tol) |>
    .sg_container_gate(tol)
}

# One row per series and seam year, with every kind that names it. A year
# can be a seam for two reasons at once (a source switch that is also a
# coverage change); it is gated once and both kinds are reported.
.sg_seam_years <- function(seams) {
  seams |>
    dplyr::summarise(
      seam_kinds = paste(
        sort(unique(as.character(seam_kind)), method = "radix"),
        collapse = "|"
      ),
      .by = dplyr::all_of(c(.sg_series_key(), "seam_year"))
    ) |>
    dplyr::mutate(year = seam_year)
}

.sg_consecutive_pairs <- function(shares) {
  shares |>
    dplyr::arrange(dplyr::pick(dplyr::all_of(c(.sg_unit_key(), "year")))) |>
    dplyr::mutate(
      prev_year = dplyr::lag(year),
      prev_share = dplyr::lag(share),
      prev_treatment = dplyr::lag(treatment),
      .by = dplyr::all_of(.sg_unit_key())
    ) |>
    dplyr::filter(!is.na(prev_year), year - prev_year == 1) |>
    dplyr::mutate(log_ratio = .sg_log_ratio(share, prev_share)) |>
    dplyr::select(
      dplyr::all_of(c(.sg_unit_key(), "year")),
      prev_year,
      prev_share,
      share,
      treatment,
      prev_treatment,
      log_ratio
    )
}

# `NA` wherever the ratio is undefined, computed on a substituted vector
# so a zero or missing denominator never reaches `log()`.
.sg_log_ratio <- function(now, prev) {
  ok <- !is.na(now) & !is.na(prev) & now > 0 & prev > 0
  safe <- dplyr::if_else(ok, now, 1) / dplyr::if_else(ok, prev, 1)
  dplyr::if_else(ok, abs(log(safe)), NA_real_)
}

# The reference distribution: ordinary observed-to-observed pairs of the
# same series, with the gated pairs held out of their own reference.
.sg_reference_quantiles <- function(pairs, gates, tol) {
  pairs |>
    dplyr::anti_join(gates, by = c(.sg_series_key(), "year")) |>
    dplyr::filter(
      treatment == "observed",
      prev_treatment == "observed",
      !is.na(log_ratio)
    ) |>
    dplyr::summarise(
      n_reference = dplyr::n(),
      q_reference = stats::quantile(
        log_ratio,
        probs = tol$reference_quantile,
        names = FALSE
      ),
      .by = dplyr::all_of(.sg_series_key())
    )
}

# Every unit present at a seam year, whether or not the year before it
# holds a row: a unit that simply appears at the seam is a gap in the
# gate, not a pass, and is reported as one.
.sg_gated_pairs <- function(shares, pairs, gates) {
  shares |>
    dplyr::select(dplyr::all_of(c(.sg_unit_key(), "year"))) |>
    dplyr::inner_join(gates, by = c(.sg_series_key(), "year")) |>
    dplyr::left_join(pairs, by = c(.sg_unit_key(), "year"))
}

.sg_score_pairs <- function(scored, tol) {
  scored |>
    dplyr::mutate(
      n_reference = dplyr::coalesce(n_reference, 0L),
      status = dplyr::case_when(
        is.na(prev_year) ~ "no_previous_row",
        is.na(share) | is.na(prev_share) ~ "na_share",
        is.na(log_ratio) ~ "zero_share",
        n_reference < tol$min_reference ~ "no_reference",
        .default = "gated"
      ),
      beyond_quantile = dplyr::if_else(
        status == "gated",
        log_ratio > q_reference,
        NA
      )
    )
}

# The gate is per container, but it is written onto every row of that
# container by a grouped `mutate()` rather than summarised and rejoined:
# a rejoin on a bare territory key is what the package's join audit asks
# code not to add, and there is nothing here it would buy.
.sg_container_gate <- function(scored, tol) {
  scored |>
    dplyr::mutate(
      n_gated = sum(status == "gated"),
      n_beyond = sum(beyond_quantile, na.rm = TRUE),
      .by = dplyr::all_of(c("area_code", "level"))
    ) |>
    dplyr::mutate(
      frac_beyond = dplyr::if_else(n_gated > 0L, n_beyond / n_gated, NA_real_),
      threshold = .sg_binomial_band(n_gated, tol),
      gate_status = dplyr::case_when(
        n_gated == 0L ~ "no_pairs",
        n_gated < tol$min_gated ~ "too_few_pairs",
        .default = "gated"
      ),
      pass = dplyr::if_else(
        gate_status == "gated",
        frac_beyond <= threshold,
        NA
      )
    ) |>
    dplyr::select(dplyr::all_of(.sg_tier_b_cols()))
}

# The upper band of a binomial proportion under the null that a seam pair
# is an ordinary pair: `p + sigma * sqrt(p (1 - p) / n)`.
.sg_binomial_band <- function(n, tol) {
  p <- tol$null_rate
  dplyr::if_else(
    n > 0L,
    p + tol$binomial_sigma * sqrt(p * (1 - p) / pmax(n, 1L)),
    NA_real_
  )
}

.sg_tier_b_cols <- function() {
  c(
    .sg_unit_key(),
    "seam_year",
    "seam_kinds",
    "prev_year",
    "prev_share",
    "share",
    "log_ratio",
    "q_reference",
    "n_reference",
    "beyond_quantile",
    "status",
    "n_gated",
    "n_beyond",
    "frac_beyond",
    "threshold",
    "gate_status",
    "pass"
  )
}

.sg_tier_b_prototype <- function() {
  tibble::tibble(
    area_code = integer(),
    level = integer(),
    item_prod_code = integer(),
    level_polity_code = character(),
    seam_year = integer(),
    seam_kinds = character(),
    prev_year = integer(),
    prev_share = numeric(),
    share = numeric(),
    log_ratio = numeric(),
    q_reference = numeric(),
    n_reference = integer(),
    beyond_quantile = logical(),
    status = character(),
    n_gated = integer(),
    n_beyond = integer(),
    frac_beyond = numeric(),
    threshold = numeric(),
    gate_status = character(),
    pass = logical()
  )
}

# --- Tier C: cell smoke -------------------------------------------------------

.sg_tier_c <- function(cells, seams, tol) {
  gates <- dplyr::distinct(seams, area_code, seam_year)
  if (is.null(cells) || nrow(gates) == 0L) {
    return(.sg_tier_c_prototype())
  }
  .sg_validate_cells(cells)
  keys <- .sg_cell_keys(cells)
  scored <- .sg_cell_shares(cells)
  plan <- .sg_cell_pair_plan(gates)
  rates <- .sg_scan_cell_pairs(scored, plan, keys, tol)
  regime <- .sg_regime_mismatch(scored, gates, keys)
  .sg_tier_c_gate(plan, rates, regime, tol)
}

# The engine's own compartment key plus the crop. `.compartment_cell_cols()`
# resolves to `(area_code, lon, lat)` on a level-0 output and adds
# `polycell_id` / `cell_id` / `level_polity_code` where the run carried
# them, which is what keeps one physical cell's compartments apart.
.sg_cell_keys <- function(cells) {
  unique(c(.compartment_cell_cols(cells), "crop_name"))
}

.sg_cell_shares <- function(cells) {
  harvested <- if (rlang::has_name(cells, "harvested_ha")) {
    as.numeric(cells$harvested_ha)
  } else {
    as.numeric(cells$rainfed_ha) + as.numeric(cells$irrigated_ha)
  }
  n_dropped <- sum(!is.finite(harvested))
  if (n_dropped > 0L) {
    cli::cli_warn(
      "Tier C dropped {n_dropped} of {nrow(cells)} {.arg cells} row{?s} whose
       harvested area is missing or infinite; they enter neither the national
       total nor the scan."
    )
  }
  cells |>
    dplyr::mutate(harvested_ha = harvested) |>
    dplyr::filter(is.finite(harvested_ha)) |>
    dplyr::mutate(
      cell_share = harvested_ha / sum(harvested_ha),
      .by = dplyr::all_of(c("area_code", "crop_name", "year"))
    )
}

# The three pairs the plan names, as the offset of each pair's FIRST year
# from the seam: the seam pair is (t0 - 1, t0), so its offset is -1.
.sg_pair_offsets <- function() {
  c(prev = -2L, seam = -1L, `next` = 0L)
}

.sg_cell_pair_plan <- function(gates) {
  offsets <- .sg_pair_offsets()
  tidyr::expand_grid(gates, role = names(offsets)) |>
    dplyr::mutate(
      offset = unname(offsets[role]),
      y1 = as.integer(seam_year) + offset,
      y2 = y1 + 1L
    ) |>
    dplyr::select(area_code, seam_year, role, y1, y2)
}

.sg_scan_cell_pairs <- function(scored, plan, keys, tol) {
  windows <- dplyr::distinct(plan, y1, y2)
  purrr::map2(
    windows$y1,
    windows$y2,
    function(from, to) {
      codes <- unique(plan$area_code[plan$y1 == from & plan$y2 == to])
      .sg_scan_one_pair(scored, from, to, codes, keys, tol)
    }
  ) |>
    purrr::list_rbind()
}

# One `check_series_jumps()` call, on this pair's two years only.
.sg_scan_one_pair <- function(scored, from, to, codes, keys, tol) {
  pair <- scored |>
    dplyr::filter(year %in% c(from, to), area_code %in% codes) |>
    .sg_apply_ha_floor(keys, tol$cell_min_ha)
  both <- pair |>
    dplyr::summarise(n = dplyr::n_distinct(year), .by = dplyr::all_of(keys)) |>
    dplyr::filter(n == 2L)
  flags <- .sg_scan_jumps(dplyr::semi_join(pair, both, by = keys), keys, tol)
  tibble::tibble(area_code = codes, y1 = from, y2 = to) |>
    dplyr::left_join(
      dplyr::count(both, area_code, name = "n_series"),
      by = "area_code"
    ) |>
    dplyr::left_join(
      dplyr::count(flags, area_code, name = "n_flag"),
      by = "area_code"
    ) |>
    dplyr::mutate(
      n_series = dplyr::coalesce(n_series, 0L),
      n_flag = dplyr::coalesce(n_flag, 0L),
      flag_rate = dplyr::if_else(n_series > 0L, n_flag / n_series, NA_real_)
    )
}

# The hectare floor `check_series_jumps()` cannot express: `min_value`
# applies to the scanned column, and the scanned column is a share.
.sg_apply_ha_floor <- function(pair, keys, min_ha) {
  keep <- pair |>
    dplyr::summarise(max_ha = max(harvested_ha), .by = dplyr::all_of(keys)) |>
    dplyr::filter(max_ha >= min_ha)
  dplyr::semi_join(pair, keep, by = keys)
}

.sg_scan_jumps <- function(pair, keys, tol) {
  if (nrow(pair) == 0L) {
    return(dplyr::select(pair, dplyr::all_of("area_code")))
  }
  check_series_jumps(
    pair,
    value_col = cell_share,
    time_col = year,
    .by = keys,
    ratio_bounds = tol$cell_ratio_bounds,
    min_value = 0,
    verbose = FALSE
  )
}

# The regime-identity assertion: a within-unit weight regime that flips
# at the seam makes the cell series discontinuous by construction, which
# no flag-rate comparison can separate from a step in the data.
.sg_regime_mismatch <- function(scored, gates, keys) {
  if (!rlang::has_name(scored, "regime")) {
    return(dplyr::mutate(
      gates,
      regime_checked = FALSE,
      n_regime_mismatch = NA_integer_
    ))
  }
  wanted <- gates |>
    tidyr::expand_grid(offset = c(-1L, 0L)) |>
    dplyr::mutate(year = as.integer(seam_year) + offset) |>
    dplyr::select(area_code, seam_year, year)
  mismatched <- scored |>
    dplyr::select(dplyr::all_of(c(keys, "year")), regime) |>
    dplyr::inner_join(
      wanted,
      by = c("area_code", "year"),
      relationship = "many-to-many"
    ) |>
    dplyr::summarise(
      n_regime = dplyr::n_distinct(regime),
      n_years = dplyr::n_distinct(year),
      .by = dplyr::all_of(c(keys, "seam_year"))
    ) |>
    dplyr::filter(n_years == 2L, n_regime > 1L) |>
    dplyr::count(area_code, seam_year, name = "n_regime_mismatch")
  gates |>
    dplyr::left_join(mismatched, by = c("area_code", "seam_year")) |>
    dplyr::mutate(
      regime_checked = TRUE,
      n_regime_mismatch = dplyr::coalesce(n_regime_mismatch, 0L)
    )
}

.sg_tier_c_gate <- function(plan, rates, regime, tol) {
  plan |>
    dplyr::left_join(rates, by = c("area_code", "y1", "y2")) |>
    dplyr::select(area_code, seam_year, role, n_series, n_flag, flag_rate) |>
    tidyr::pivot_wider(
      names_from = role,
      values_from = c(n_series, n_flag, flag_rate)
    ) |>
    dplyr::left_join(regime, by = c("area_code", "seam_year")) |>
    dplyr::mutate(
      neighbour_rate = .sg_neighbour_mean(flag_rate_prev, flag_rate_next),
      excess = flag_rate_seam - neighbour_rate,
      reason = dplyr::case_when(
        dplyr::coalesce(n_regime_mismatch > 0L, FALSE) ~ "regime_flip",
        is.na(flag_rate_seam) ~ "seam_pair_not_scanned",
        is.na(neighbour_rate) ~ "no_neighbour_pair",
        excess > tol$cell_excess ~ "seam_flag_rate_excess",
        .default = ""
      ),
      pass = dplyr::case_when(
        reason == "" ~ TRUE,
        reason %in% c("seam_pair_not_scanned", "no_neighbour_pair") ~ NA,
        .default = FALSE
      )
    )
}

# The mean of whichever neighbouring pairs were scannable; `NA` when
# neither was, so the gate reports itself unevaluable instead of
# comparing the seam pair with nothing.
.sg_neighbour_mean <- function(before, after) {
  n <- (!is.na(before)) + (!is.na(after))
  total <- dplyr::coalesce(before, 0) + dplyr::coalesce(after, 0)
  dplyr::if_else(n > 0L, total / n, NA_real_)
}

.sg_tier_c_prototype <- function() {
  tibble::tibble(
    area_code = integer(),
    seam_year = integer(),
    n_series_prev = integer(),
    n_series_seam = integer(),
    n_series_next = integer(),
    n_flag_prev = integer(),
    n_flag_seam = integer(),
    n_flag_next = integer(),
    flag_rate_prev = numeric(),
    flag_rate_seam = numeric(),
    flag_rate_next = numeric(),
    regime_checked = logical(),
    n_regime_mismatch = integer(),
    neighbour_rate = numeric(),
    excess = numeric(),
    reason = character(),
    pass = logical()
  )
}

# --- Verdict and report -------------------------------------------------------

.sg_verdict <- function(tier_a, tier_b, tier_c) {
  tiers <- c(
    tier_a = .sg_tier_verdict(tier_a$pass),
    tier_b = .sg_tier_verdict(tier_b$pass),
    tier_c = .sg_tier_verdict(tier_c$pass)
  )
  c(tiers, overall = .sg_overall_verdict(tiers))
}

.sg_tier_verdict <- function(pass) {
  pass <- pass[!is.na(pass)]
  if (length(pass) == 0L) {
    return(NA)
  }
  all(pass)
}

.sg_overall_verdict <- function(tiers) {
  if (any(!tiers, na.rm = TRUE)) {
    return(FALSE)
  }
  if (all(is.na(tiers))) {
    return(NA)
  }
  TRUE
}

.sg_report <- function(tier_a, tier_b, tier_c, verdict) {
  n_scored <- sum(tier_b$status == "gated")
  judged <- dplyr::n_distinct(tier_b$area_code[!is.na(tier_b$pass)])
  cli::cli_inform(c(
    "{.fn seam_gate}: overall {.val {verdict[['overall']]}}.",
    "*" = "Tier A {.val {verdict[['tier_a']]}}: {nrow(tier_a)} series,
           {sum(!tier_a$pass)} failing.",
    "*" = "Tier B {.val {verdict[['tier_b']]}}: {n_scored} of
           {nrow(tier_b)} unit-seam pair{?s} scored,
           {judged} of {dplyr::n_distinct(tier_b$area_code)}
           container{?s} judged.",
    "*" = "Tier C {.val {verdict[['tier_c']]}}: {nrow(tier_c)}
           container-seam gate{?s},
           {sum(tier_c$pass, na.rm = TRUE)} passing."
  ))
  invisible(NULL)
}
