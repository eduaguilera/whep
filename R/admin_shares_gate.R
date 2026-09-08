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
#
# Added by the tier-B hold-out leg (T29b), for the same call. Nine are
# new; `first_year` is already declared for another file and is listed
# only because this one now uses it too:
# c(
#   "anchor_year", "holdout_k", "horizon", "n_backcast_years",
#   "n_observed_years", "n_units_gated", "reconstructed",
#   "share_backcast", "share_observed"
# )
#
# No `.example_*` fixture is added: `seam_gate()`'s example is unchanged.

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
#' - `holdout_k` (10): how many of a series' earliest observed years the
#'   tier-B hold-out withholds. The leg's power grows with the horizon,
#'   because a wrong per-unit trend in the extent proxy accumulates
#'   linearly with the years back-cast while the yardstick stays a single
#'   year's move: on this package's own two-history fixture
#'   (`test_admin_shares_gate.R`), two extent tables whose reconstructions
#'   differ by a factor of 0.40 to 1.63 at ten years are **not** separated
#'   at `holdout_k = 3` (both pass) and are separated decisively at 10
#'   (0.008 against 0.383 of pairs beyond the reference quantile). Each
#'   withheld year is also an observation removed from both the anchor and
#'   the reference, so `holdout_k` cannot approach the length of the
#'   record; ten leaves two decades of reference pairs on a thirty-year
#'   series. Assumed, unverified: no measurement fixes it, and it is a
#'   choice about how deep an extrapolation the gate certifies, not a
#'   property of the data.
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
#' @param holdout_k How many of a series' earliest observed years the
#'   tier-B hold-out withholds.
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
  cell_excess = 0.01,
  holdout_k = 10L
) {
  .validate_tol(identity_rel, "identity_rel")
  .validate_tol(identity_floor, "identity_floor")
  .validate_tol(binomial_sigma, "binomial_sigma")
  .validate_tol(min_reference, "min_reference")
  .validate_tol(cell_min_ha, "cell_min_ha")
  .validate_tol(cell_excess, "cell_excess")
  .sg_check_quantile(reference_quantile)
  .sg_check_bounds(cell_ratio_bounds)
  .sg_check_k(holdout_k)
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
    cell_excess = cell_excess,
    holdout_k = as.integer(holdout_k)
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
#' Two states are not a pass, and each says so on its own row rather
#' than in a verdict. Both keep their measured numbers, because what is
#' withheld is the verdict and not the evidence:
#'
#' - **no `"start"` seam names the series**, so the last check above did
#'   not run: `pass` is `NA`, `reason = "no_start_seam"`. Gated here
#'   means what the check means -- a seam year to compare `t0` with --
#'   so a series named only by, say, a `source_switch` seam is ungated
#'   in tier A and gated in tier B, where that seam actually lands. An
#'   empty or filtered seam list would otherwise certify a run by
#'   handing tier A nothing to check.
#' - **no observed row at all**, so there is no anchor to measure:
#'   `pass` is `NA`, `reason = "no_observed_anchor"` and every anchor
#'   measurement on the row is `NA`. Such a series has a row here rather
#'   than none, because a series that never reached the tier is the one
#'   state a table of tier-A rows cannot otherwise show.
#'
#' Both reasons come after the identity ones, so a series whose identity
#' is wrong still fails on the identity: a missing seam withholds a
#' verdict, it does not excuse one.
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
#' **That reasoning holds only where both sides of the pair are
#' observations**, which `basis` records per pair and the container gate
#' is grouped by:
#'
#' - `basis = "observed_both_sides"`: the seam year's row and the year
#'   before it are both `treatment == "observed"`. The tier keeps its
#'   meaning, and its `pass` enters the verdict. This is the
#'   `source_switch`, `grain_switch`, `nuts_version_switch`,
#'   `coverage_change` and `indicator_switch` case.
#' - `basis = "vacuous_by_construction"`: the year before the seam is a
#'   row the back-cast produced, or is absent. It is read off the two
#'   rows' `treatment`, not off the seam kind, because that is the
#'   operative fact. At a `"start"` seam of a table whose anchor matches
#'   it -- tier A's last check -- it always holds: `t0` is the series'
#'   first observed year, so
#'   `s_u(t0 - 1)` is `s_u(t0) * E_u(t0 - 1) / E_u(t0)` renormalised, one
#'   year of smooth extent change. Scored against a reference built from
#'   the noisier observed year-to-year moves it cannot fail: two extent
#'   proxies with opposite per-unit trends, whose reconstructions differ
#'   by a factor of 0.40 to 1.63 ten years down, both return
#'   `n_beyond = 0`. Such rows report `gate_status =
#'   "vacuous_by_construction"` and `pass = NA`, and do **not** enter the
#'   verdict; `frac_beyond` and `threshold` are still filled in, because
#'   the numbers are worth reading even though they decide nothing.
#'
#' @section Tier B hold-out -- what stands in at a start seam:
#' Only when `holdout` is supplied. For every series with a `"start"`
#' seam, the first `holdout_k` observed years are withheld, the series is
#' back-cast again from the next observed year (the `seam =` override of
#' [backcast_admin_shares()], which is the same lever the verification
#' protocol's temporal hold-out uses), and the reconstruction is scored
#' against what was actually observed in those years:
#' `|log(s_hat_u(t) / s_u(t))|` per unit and withheld year, against the
#' same reference quantile of that series' observed consecutive
#' log-ratios. The two are commensurable under iid year-to-year noise: a
#' reconstruction error is a difference of two years' noise (the anchor's
#' and the withheld year's), like a one-year move, and neither widens
#' with the horizon. On this package's fixture the error distribution
#' comes out slightly narrower than the reference, so the realised null
#' exceedance rate sits under `null_rate` rather than over it; under
#' persistent noise a deep hold-out over-rejects instead. Both departures
#' are the safe direction for a gate, and neither is assumed: the
#' measured fractions are in the tests.
#'
#' The reference excludes every pair at or below the hold-out anchor as
#' well as every seam year, for the reason tier B holds its gated pairs
#' out of their own reference: a series whose earliest years are unusual
#' would otherwise both widen the yardstick and be measured by it. The
#' consequence is worth stating plainly -- where the earliest years
#' genuinely move differently from the rest of the record, this leg
#' fails a proxy that is right about the rest. It fails loudly rather
#' than passing quietly.
#'
#' The band divides by the number of **units** with a scored pair, not by
#' the number of pairs. A unit's `holdout_k` errors all carry the anchor
#' year's own noise, so they are one cluster and not `holdout_k`
#' independent draws; dividing by pairs made this leg flag a correct
#' proxy on the package's own fixture. `min_gated` therefore reads as a
#' minimum unit count here (`gate_status = "too_few_units"`).
#'
#' A series with `holdout_k` or fewer observed years is reported
#' `not_applicable`, never passed, and so is every series when `holdout`
#' is not supplied: a start seam then has no evidence at all behind it,
#' which the returned table says on its own row.
#'
#' What a pass certifies is a `holdout_k`-year extrapolation. Where the
#' published back-cast runs deeper -- `n_backcast_years` on every row
#' says how much deeper -- it is evidence about the first `holdout_k`
#' years of it and no more.
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
#' Nothing in the package writes that column today: the allocation's
#' regime label is `method_crop_alloc`, which lives on the targets
#' table and not on the cell grid, so on the shipped crop-level output
#' this axis is **not evaluated**. Such a gate reports
#' `regime_checked = FALSE` and `n_regime_mismatch = NA` -- `NA` rather
#' than zero, because an axis nobody looked at has no count of
#' mismatches -- and the report line says how many of the gates are in
#' that state, so a pass is not read as evidence that no regime flipped.
#'
#' @param shares_backcast A back-cast share table, the `shares` element
#'   of [backcast_admin_shares()]. Columns: `area_code`, `level`,
#'   `item_prod_code`, `level_polity_code`, `year`, `share`, `treatment`,
#'   optionally `value`. One row per unit and year.
#' @param seams A seam list, the `seams` element of
#'   [resolve_admin_shares()]: `area_code`, `level`, `item_prod_code`,
#'   `seam_year`, `seam_kind`. Every seam must name a series
#'   `shares_backcast` carries, or the gate aborts
#'   (`whep_seam_gate_seams_unmatched`): such a seam is gated by nothing
#'   and leaves no row in any tier, so it can be reported nowhere else.
#'   The resolver emits one `"start"` seam per series, so a matched pair
#'   satisfies this by construction and a breach means the two tables
#'   come from different runs, or one of them was filtered. A zero-row
#'   `shares_backcast` -- the tier-C-only call -- is exempt.
#' @param cells Optional crop-level engine output for tier C: `lon`,
#'   `lat`, `year`, `area_code`, `crop_name`, and either `harvested_ha`
#'   or `rainfed_ha` plus `irrigated_ha`. `polycell_id`, `cell_id`,
#'   `level_polity_code` and `regime` are used when present. `NULL`
#'   (default) leaves tier C unevaluated.
#' @param holdout What the tier-B hold-out needs, or `NULL` (default),
#'   which leaves it unevaluated. A list of `extent`, the per-unit extent
#'   table from [aggregate_unit_extent()] that the back-cast was run
#'   with, and optionally `args`, a named list of further arguments for
#'   [backcast_admin_shares()] -- normally
#'   `list(settings = <the knobs the original back-cast ran with>)`, so
#'   the hold-out is run the way the run was.
#'   `shares`, `extent` and `seam` are set by the leg and are refused in
#'   `args`. With `holdout` supplied, `shares_backcast` must also carry
#'   `indicator_used` and `treatment_year`, which the back-cast reads.
#'   The re-run's own input errors -- an `extent` with two rows for a
#'   unit-year, say -- surface as errors from it. That is not the gate
#'   failing: a gate outcome is always a returned row.
#' @param tolerances The numbers to judge with, from
#'   [seam_gate_tolerances()].
#'
#' @return A list:
#'   - `tier_a`: one row per series `shares_backcast` carries, with
#'     `t0`, `n_units`, `n_observed`, `share_sum`, `max_rel_diff`,
#'     `basis`, `seam_start_year`, `matches_seam_start`, `pass` and
#'     `reason`. `pass` is `NA` where the tier judged nothing, and
#'     `reason` says which of the two ways.
#'   - `tier_b`: one row per gated unit-seam pair, with `basis`,
#'     `log_ratio`, `q_reference`, `n_reference`, `beyond_quantile` and
#'     `status`, plus the container gate (`n_gated`, `n_beyond`,
#'     `frac_beyond`, `threshold`, `gate_status`, `pass`) broadcast onto
#'     every row of that container and `basis`.
#'   - `tier_b_holdout`: one row per unit and withheld year, with
#'     `anchor_year`, `horizon`, `share_observed`, `share_backcast`,
#'     `log_ratio`, `status` and the per-series gate; plus one row,
#'     keyed on the series alone, for every start-seam series the leg
#'     could not run (`status` beginning `"not_applicable"`). Zero rows
#'     when no series has a `"start"` seam.
#'   - `tier_c`: one row per gated `(area_code, seam_year)`, with the
#'     three pairs' scanned series counts and flag rates,
#'     `neighbour_rate`, `excess`, `n_regime_mismatch`, `pass` and
#'     `reason`. Zero rows when `cells` is `NULL`.
#'   - `verdict`: a named logical, `tier_a` / `tier_b` /
#'     `tier_b_holdout` / `tier_c` / `overall`. A tier is `TRUE` when
#'     every gate it evaluated passed, `FALSE` when any failed and `NA`
#'     when it evaluated none -- which is what a container of nothing but
#'     `"vacuous_by_construction"` rows gives, and what an empty seam
#'     list gives in every tier at once. `overall` is `FALSE` if any
#'     tier failed; `NA` if any series of `shares_backcast` left tier A,
#'     tier B and the hold-out with nothing but `NA` for a `pass`, or if
#'     no tier evaluated anything; else `TRUE`. A pass therefore covers
#'     every series the gate was handed. Tier C does not count towards
#'     that coverage: it is keyed on `(container, seam year)` and
#'     carries no item, so it cannot stand in for a series.
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
  holdout = NULL,
  tolerances = seam_gate_tolerances()
) {
  tolerances <- .sg_validate_tolerances(tolerances)
  .sg_validate_shares(shares_backcast)
  .sg_validate_seams(seams)
  .sg_validate_series_match(shares_backcast, seams)
  holdout <- .sg_validate_holdout(holdout, shares_backcast)
  tiers <- list(
    tier_a = .sg_tier_a(shares_backcast, seams, tolerances),
    tier_b = .sg_tier_b(shares_backcast, seams, tolerances),
    tier_b_holdout = .sg_tier_b_holdout(
      shares_backcast,
      seams,
      holdout,
      tolerances
    ),
    tier_c = .sg_tier_c(cells, seams, tolerances)
  )
  coverage <- .sg_series_coverage(shares_backcast, tiers)
  verdict <- .sg_verdict(tiers, coverage)
  .sg_report(tiers, verdict, coverage)
  c(tiers, list(verdict = verdict))
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

.sg_check_k <- function(value) {
  ok <- is.numeric(value) &&
    length(value) == 1 &&
    !is.na(value) &&
    value >= 1 &&
    value == trunc(value)
  if (!ok) {
    cli::cli_abort("{.arg holdout_k} must be one whole number of years >= 1.")
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

# The hold-out re-runs the back-cast, so it needs the extent table and
# the two provenance columns that function keys on, neither of which the
# gate's own contract asks for. Refused here rather than deep inside a
# `do.call()`, and only when the leg is actually requested.
.sg_validate_holdout <- function(holdout, shares) {
  if (is.null(holdout)) {
    return(NULL)
  }
  if (!is.list(holdout) || !rlang::has_name(holdout, "extent")) {
    cli::cli_abort("{.arg holdout} must be a list with an {.field extent}.")
  }
  extra <- setdiff(names(holdout), c("extent", "args"))
  if (length(extra) > 0) {
    cli::cli_abort("{.arg holdout} has unknown element{?s} {.field {extra}}.")
  }
  .require_cols(
    holdout$extent,
    c("area_code", "level_polity_code", "level", "year", "extent_ha"),
    "holdout$extent"
  )
  .require_cols(
    shares,
    c("indicator_used", "treatment_year"),
    "shares_backcast"
  )
  list(extent = holdout$extent, args = .sg_holdout_args(holdout$args))
}

.sg_holdout_args <- function(args) {
  args <- args %||% list()
  if (!is.list(args) || (length(args) > 0 && !rlang::is_named(args))) {
    cli::cli_abort("{.arg holdout$args} must be a named list.")
  }
  reserved <- intersect(names(args), c("shares", "extent", "seam"))
  if (length(reserved) > 0) {
    cli::cli_abort(c(
      "{.arg holdout$args} sets {.field {reserved}}, which the leg sets.",
      i = "The hold-out chooses the anchor; that is what it is."
    ))
  }
  args
}

# A seam whose series `shares_backcast` does not carry is gated by
# nothing and leaves no row in any tier to report itself on, so the gate
# would pronounce on a run while measuring none of it. There is nowhere
# but here it can be seen, which is why this one is refused rather than
# returned. A zero-row `shares_backcast` is exempt: that is the
# documented tier-C-only call, where tiers A and B report themselves
# unevaluated on their own.
.sg_validate_series_match <- function(shares, seams) {
  if (nrow(shares) == 0L || nrow(seams) == 0L) {
    return(invisible(NULL))
  }
  unmatched <- dplyr::anti_join(
    .sg_distinct_keys(seams, .sg_series_key()),
    .sg_distinct_keys(shares, .sg_series_key()),
    by = .sg_series_key()
  )
  if (nrow(unmatched) == 0L) {
    return(invisible(NULL))
  }
  shown <- .sg_series_labels(unmatched)
  cli::cli_abort(
    c(
      "{nrow(unmatched)} seam series {?is/are} absent from
       {.arg shares_backcast}.",
      x = "{.val {shown}}",
      i = "Gate a run against its own seam list, or filter both tables to
           the same series."
    ),
    class = "whep_seam_gate_seams_unmatched"
  )
}

.sg_series_labels <- function(keys) {
  sprintf(
    "area_code %s, level %s, item %s",
    keys$area_code,
    keys$level,
    keys$item_prod_code
  )
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

# One row per series `shares` carries, including the series that have no
# anchor to measure: a series that never reached the tier is the one
# state a table of tier-A rows cannot otherwise show, and it is what the
# gate is handed when a `treatment` column arrives mislabelled.
.sg_tier_a <- function(shares, seams, tol) {
  series <- .sg_distinct_keys(shares, .sg_series_key())
  if (nrow(series) == 0L) {
    return(.sg_tier_a_prototype())
  }
  measured <- .sg_anchor_measures(shares, tol)
  dplyr::bind_rows(measured, .sg_unanchored_rows(series, measured)) |>
    dplyr::left_join(.sg_start_seams(seams), by = .sg_series_key()) |>
    .sg_anchor_verdict(tol)
}

# The anchor identity of the series that have an anchor.
.sg_anchor_measures <- function(shares, tol) {
  anchors <- .sg_anchor_years(shares)
  if (nrow(anchors) == 0L) {
    return(anchors)
  }
  shares |>
    dplyr::inner_join(anchors, by = .sg_series_key()) |>
    dplyr::filter(year == t0) |>
    .sg_anchor_value_share(tol) |>
    .sg_anchor_summarise()
}

# Every measurement is at `t0`, so a series with no `t0` carries `NA`
# for all of them rather than a zero: a count of nothing is not a count
# of none. Written out rather than left to `bind_rows()`, which would
# leave the columns absent altogether when no series has an anchor.
.sg_unanchored_rows <- function(series, measured) {
  series |>
    dplyr::anti_join(measured, by = .sg_series_key()) |>
    dplyr::mutate(
      t0 = NA_integer_,
      n_units = NA_integer_,
      n_observed = NA_integer_,
      share_sum = NA_real_,
      max_rel_diff = NA_real_,
      basis = NA_character_
    )
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

# The two withholding reasons come last, so a series nothing gates still
# fails on its own identity where its identity is wrong: a missing seam
# withholds a verdict, it does not excuse one.
#
# What withholds the seam verdict is `matches_seam_start` being `NA` --
# the check's own result, not a second membership test keyed on
# something else. A vacuity test that asked "does any seam name this
# series?" while the check asked "does a start seam name it?" passed a
# series on a comparison that never ran; and asking it in formatted
# strings while the join asks it in values marked a joined series
# ungated. One key, one kind of comparison, and it is the check's.
.sg_anchor_verdict <- function(anchors, tol) {
  anchors |>
    dplyr::mutate(
      matches_seam_start = dplyr::if_else(
        is.na(seam_start_year),
        NA,
        t0 == seam_start_year
      ),
      reason = dplyr::case_when(
        is.na(t0) ~ "no_observed_anchor",
        n_observed < n_units ~ "anchor_row_not_observed",
        is.na(share_sum) ~ "anchor_share_missing",
        abs(share_sum - 1) > tol$identity_rel ~ "anchor_shares_not_unit_sum",
        !dplyr::coalesce(max_rel_diff <= tol$identity_rel, TRUE) ~
          "anchor_share_disagrees_with_value",
        !dplyr::coalesce(matches_seam_start, TRUE) ~ "anchor_not_seam_start",
        is.na(matches_seam_start) ~ "no_start_seam",
        .default = ""
      ),
      pass = dplyr::case_when(
        reason == "" ~ TRUE,
        reason %in% .sg_tier_a_ungated() ~ NA,
        .default = FALSE
      )
    ) |>
    dplyr::select(dplyr::all_of(.sg_tier_a_cols()))
}

# The tier-A reasons that withhold a verdict instead of failing one.
.sg_tier_a_ungated <- function() {
  c("no_start_seam", "no_observed_anchor")
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

# A seam log-ratio is evidence only where both its rows are
# observations. Read off the two treatments rather than off the seam
# kind, because that is the operative fact: at a `"start"` seam the year
# before `t0` is a row the back-cast produced from `t0` (or is absent),
# so the comparison is with a construction and cannot fail.
.sg_pair_basis <- function(treatment, prev_treatment) {
  observed <- !is.na(treatment) &
    !is.na(prev_treatment) &
    treatment == "observed" &
    prev_treatment == "observed"
  dplyr::if_else(observed, "observed_both_sides", "vacuous_by_construction")
}

.sg_score_pairs <- function(scored, tol) {
  scored |>
    dplyr::mutate(
      basis = .sg_pair_basis(treatment, prev_treatment),
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
#
# `basis` joins the grouping so that pairs whose comparison is with a
# back-cast row are not pooled with pairs between two observations: one
# rate over both would let the vacuous half dilute the half that means
# something.
.sg_container_gate <- function(scored, tol) {
  scored |>
    dplyr::mutate(
      n_gated = sum(status == "gated"),
      n_beyond = sum(beyond_quantile, na.rm = TRUE),
      .by = dplyr::all_of(c("area_code", "level", "basis"))
    ) |>
    dplyr::mutate(
      frac_beyond = dplyr::if_else(n_gated > 0L, n_beyond / n_gated, NA_real_),
      threshold = .sg_binomial_band(n_gated, tol),
      gate_status = dplyr::case_when(
        basis == "vacuous_by_construction" ~ "vacuous_by_construction",
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
    "basis",
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
    basis = character(),
    status = character(),
    n_gated = integer(),
    n_beyond = integer(),
    frac_beyond = numeric(),
    threshold = numeric(),
    gate_status = character(),
    pass = logical()
  )
}

# --- Tier B hold-out: what stands in where tier B cannot bite -----------------

.sg_tier_b_holdout <- function(shares, seams, holdout, tol) {
  series <- .sg_holdout_series(shares, seams, tol)
  if (nrow(series) == 0L) {
    return(.sg_holdout_prototype())
  }
  rows <- if (is.null(holdout)) {
    .sg_holdout_refusal(series, "not_applicable_no_extent")
  } else {
    dplyr::bind_rows(
      .sg_holdout_scored(
        shares,
        seams,
        dplyr::filter(series, !is.na(anchor_year)),
        holdout,
        tol
      ),
      .sg_holdout_refusal(
        dplyr::filter(series, is.na(anchor_year)),
        "not_applicable_short_series"
      )
    )
  }
  .sg_holdout_gate(rows, tol)
}

# The series the leg is about: those the seam list calls `"start"`, with
# the anchor the hold-out would move to. `anchor_year` is `NA` where the
# record is too short to withhold `holdout_k` years and still have one to
# anchor on -- reported, never shortened to fit.
.sg_holdout_series <- function(shares, seams, tol) {
  starts <- .sg_start_seams(seams)
  observed <- dplyr::filter(shares, treatment == "observed")
  if (nrow(starts) == 0L || nrow(observed) == 0L) {
    return(.sg_holdout_series_prototype())
  }
  observed |>
    dplyr::distinct(dplyr::pick(dplyr::all_of(c(.sg_series_key(), "year")))) |>
    dplyr::summarise(
      n_observed_years = dplyr::n(),
      anchor_year = .sg_nth_year(year, tol$holdout_k + 1L),
      .by = dplyr::all_of(.sg_series_key())
    ) |>
    dplyr::inner_join(starts, by = .sg_series_key()) |>
    dplyr::inner_join(.sg_anchor_years(shares), by = .sg_series_key()) |>
    dplyr::left_join(.sg_series_first_year(shares), by = .sg_series_key()) |>
    dplyr::mutate(
      holdout_k = as.integer(tol$holdout_k),
      n_backcast_years = as.integer(t0 - first_year)
    ) |>
    dplyr::select(dplyr::all_of(.sg_holdout_series_cols()))
}

.sg_series_first_year <- function(shares) {
  dplyr::summarise(
    shares,
    first_year = min(year),
    .by = dplyr::all_of(.sg_series_key())
  )
}

.sg_nth_year <- function(years, n) {
  years <- sort(unique(years))
  if (length(years) < n) {
    return(NA_integer_)
  }
  as.integer(years[[n]])
}

.sg_holdout_series_cols <- function() {
  c(
    .sg_series_key(),
    "t0",
    "anchor_year",
    "holdout_k",
    "n_observed_years",
    "n_backcast_years"
  )
}

.sg_holdout_series_prototype <- function() {
  tibble::tibble(
    area_code = integer(),
    level = integer(),
    item_prod_code = integer(),
    t0 = integer(),
    anchor_year = integer(),
    holdout_k = integer(),
    n_observed_years = integer(),
    n_backcast_years = integer()
  )
}

# A series the leg could not run at all: one row, keyed on the series,
# carrying its reason. It reaches the gate like any other row and counts
# towards nothing, which is what keeps `not_applicable` out of `pass`.
# The argument is `why`, not `status`, so that the assignment below reads
# the argument rather than a column of `series` should one ever be added.
.sg_holdout_refusal <- function(series, why) {
  if (nrow(series) == 0L) {
    return(.sg_holdout_prototype())
  }
  series |>
    dplyr::mutate(
      level_polity_code = NA_character_,
      year = NA_integer_,
      horizon = NA_integer_,
      share_observed = NA_real_,
      share_backcast = NA_real_,
      log_ratio = NA_real_,
      q_reference = NA_real_,
      n_reference = NA_integer_,
      beyond_quantile = NA,
      status = why
    )
}

.sg_holdout_scored <- function(shares, seams, series, holdout, tol) {
  if (nrow(series) == 0L) {
    return(.sg_holdout_prototype())
  }
  withheld <- .sg_holdout_withheld(shares, series)
  produced <- .sg_holdout_backcast(shares, series, holdout)
  shares |>
    dplyr::filter(treatment == "observed") |>
    dplyr::select(
      dplyr::all_of(c(.sg_unit_key(), "year")),
      share_observed = share
    ) |>
    dplyr::inner_join(withheld, by = c(.sg_series_key(), "year")) |>
    dplyr::left_join(
      produced,
      by = c(.sg_unit_key(), "year"),
      relationship = "one-to-one"
    ) |>
    dplyr::left_join(series, by = .sg_series_key()) |>
    dplyr::left_join(
      .sg_holdout_reference(shares, seams, series, tol),
      by = .sg_series_key()
    ) |>
    .sg_holdout_status(tol)
}

.sg_holdout_status <- function(scored, tol) {
  scored |>
    dplyr::mutate(
      horizon = as.integer(anchor_year - year),
      log_ratio = .sg_log_ratio(share_backcast, share_observed),
      n_reference = dplyr::coalesce(n_reference, 0L),
      status = dplyr::case_when(
        !dplyr::coalesce(reconstructed, FALSE) ~ "not_reconstructed",
        is.na(share_backcast) | is.na(share_observed) ~ "na_share",
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

# The observed years below the hold-out anchor: the first `holdout_k` of
# them, counted on the years that are actually there, so a gap in the
# record shortens no window.
.sg_holdout_withheld <- function(shares, series) {
  shares |>
    dplyr::filter(treatment == "observed") |>
    dplyr::distinct(dplyr::pick(dplyr::all_of(c(.sg_series_key(), "year")))) |>
    dplyr::inner_join(
      dplyr::select(series, dplyr::all_of(c(.sg_series_key(), "anchor_year"))),
      by = .sg_series_key()
    ) |>
    dplyr::filter(year < anchor_year) |>
    dplyr::select(dplyr::all_of(c(.sg_series_key(), "year")))
}

# Re-run the back-cast from the withheld anchor. Only the rows it
# produced are kept: a series whose override did not take (an anchor year
# that is not observed, a refused extent) would otherwise hand back the
# observations themselves and score a perfect reconstruction of them.
.sg_holdout_backcast <- function(shares, series, holdout) {
  input <- shares |>
    dplyr::semi_join(series, by = .sg_series_key()) |>
    dplyr::filter(treatment == "observed") |>
    dplyr::select(
      -dplyr::any_of(c("treatment", "gap_rule", "extent_ha", "extent_basis"))
    )
  seam <- dplyr::transmute(
    series,
    area_code,
    level,
    item_prod_code,
    t0 = anchor_year
  )
  args <- c(
    list(shares = input, extent = holdout$extent, seam = seam),
    holdout$args
  )
  do.call(backcast_admin_shares, args)$shares |>
    dplyr::filter(!dplyr::coalesce(treatment == "observed", FALSE)) |>
    dplyr::select(
      dplyr::all_of(c(.sg_unit_key(), "year")),
      share_backcast = share
    ) |>
    dplyr::mutate(reconstructed = TRUE)
}

# The same reference distribution tier B judges against, with the
# hold-out window held out of it as tier B holds its gated pairs out of
# theirs: every pair at or below the anchor goes, and so does every seam
# year.
.sg_holdout_reference <- function(shares, seams, series, tol) {
  window <- shares |>
    dplyr::filter(treatment == "observed") |>
    dplyr::distinct(dplyr::pick(dplyr::all_of(c(.sg_series_key(), "year")))) |>
    dplyr::inner_join(
      dplyr::select(series, dplyr::all_of(c(.sg_series_key(), "anchor_year"))),
      by = .sg_series_key()
    ) |>
    dplyr::filter(year <= anchor_year) |>
    dplyr::select(dplyr::all_of(c(.sg_series_key(), "year")))
  excluded <- dplyr::distinct(dplyr::bind_rows(
    dplyr::select(
      .sg_seam_years(seams),
      dplyr::all_of(c(.sg_series_key(), "year"))
    ),
    window
  ))
  .sg_reference_quantiles(.sg_consecutive_pairs(shares), excluded, tol)
}

# The band divides by the number of units, not the number of pairs: a
# unit's `holdout_k` errors all carry its anchor year's own noise, so
# they are one cluster. `min_gated` is the same arithmetic on that count
# -- one whole unit beyond the quantile is `1 / n_units` of the pairs.
.sg_holdout_gate <- function(rows, tol) {
  rows |>
    dplyr::mutate(
      n_gated = sum(status == "gated"),
      n_units_gated = dplyr::n_distinct(level_polity_code[status == "gated"]),
      n_beyond = sum(beyond_quantile, na.rm = TRUE),
      .by = dplyr::all_of(.sg_series_key())
    ) |>
    dplyr::mutate(
      frac_beyond = dplyr::if_else(n_gated > 0L, n_beyond / n_gated, NA_real_),
      threshold = .sg_binomial_band(n_units_gated, tol),
      gate_status = dplyr::case_when(
        n_gated == 0L & startsWith(status, "not_applicable") ~
          "not_applicable",
        n_gated == 0L ~ "no_pairs",
        n_units_gated < tol$min_gated ~ "too_few_units",
        .default = "gated"
      ),
      pass = dplyr::if_else(
        gate_status == "gated",
        frac_beyond <= threshold,
        NA
      )
    ) |>
    dplyr::select(dplyr::all_of(.sg_holdout_cols()))
}

.sg_holdout_cols <- function() {
  c(
    .sg_unit_key(),
    "t0",
    "anchor_year",
    "year",
    "horizon",
    "holdout_k",
    "n_observed_years",
    "n_backcast_years",
    "share_observed",
    "share_backcast",
    "log_ratio",
    "q_reference",
    "n_reference",
    "beyond_quantile",
    "status",
    "n_gated",
    "n_units_gated",
    "n_beyond",
    "frac_beyond",
    "threshold",
    "gate_status",
    "pass"
  )
}

.sg_holdout_prototype <- function() {
  tibble::tibble(
    area_code = integer(),
    level = integer(),
    item_prod_code = integer(),
    level_polity_code = character(),
    t0 = integer(),
    anchor_year = integer(),
    year = integer(),
    horizon = integer(),
    holdout_k = integer(),
    n_observed_years = integer(),
    n_backcast_years = integer(),
    share_observed = numeric(),
    share_backcast = numeric(),
    log_ratio = numeric(),
    q_reference = numeric(),
    n_reference = integer(),
    beyond_quantile = logical(),
    status = character(),
    n_gated = integer(),
    n_units_gated = integer(),
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

.sg_verdict <- function(tiers, coverage) {
  by_tier <- c(
    tier_a = .sg_tier_verdict(tiers$tier_a$pass),
    tier_b = .sg_tier_verdict(tiers$tier_b$pass),
    tier_b_holdout = .sg_tier_verdict(tiers$tier_b_holdout$pass),
    tier_c = .sg_tier_verdict(tiers$tier_c$pass)
  )
  c(by_tier, overall = .sg_overall_verdict(by_tier, coverage$n_ungated))
}

.sg_tier_verdict <- function(pass) {
  pass <- pass[!is.na(pass)]
  if (length(pass) == 0L) {
    return(NA)
  }
  all(pass)
}

# A failure is a failure whatever else was measured, so it is read
# first. Short of one, a run is certified only if every series it
# carries was judged: a tier verdict of `TRUE` over the series that were
# gated says nothing about the ones that were not, and `TRUE` here would
# be read as saying it did.
.sg_overall_verdict <- function(tiers, n_ungated) {
  if (any(!tiers, na.rm = TRUE)) {
    return(FALSE)
  }
  if (n_ungated > 0L || all(is.na(tiers))) {
    return(NA)
  }
  TRUE
}

# Judged means one thing in this file: a gate returned a `pass` that is
# not `NA`. Counted per series of `shares_backcast`, over the three
# tiers that are keyed on a series -- tier C is keyed on
# `(container, seam year)` and carries no item, so it cannot answer for
# one series of a container's several.
.sg_series_coverage <- function(shares, tiers) {
  series <- .sg_distinct_keys(shares, .sg_series_key())
  judged <- dplyr::bind_rows(
    .sg_judged_series(tiers$tier_a),
    .sg_judged_series(tiers$tier_b),
    .sg_judged_series(tiers$tier_b_holdout)
  )
  ungated <- dplyr::anti_join(series, judged, by = .sg_series_key())
  list(n_series = nrow(series), n_ungated = nrow(ungated))
}

.sg_judged_series <- function(tier) {
  .sg_distinct_keys(dplyr::filter(tier, !is.na(pass)), .sg_series_key())
}

.sg_report <- function(tiers, verdict, coverage) {
  tier_a <- tiers$tier_a
  tier_b <- tiers$tier_b
  holdout_b <- tiers$tier_b_holdout
  tier_c <- tiers$tier_c
  n_series <- coverage$n_series
  n_judged <- n_series - coverage$n_ungated
  n_scored <- sum(tier_b$status == "gated")
  b_judged <- dplyr::n_distinct(tier_b$area_code[!is.na(tier_b$pass)])
  vacuous <- sum(tier_b$basis == "vacuous_by_construction")
  n_holdout <- holdout_b |>
    dplyr::distinct(dplyr::pick(dplyr::all_of(.sg_series_key()))) |>
    nrow()
  a_failing <- sum(!tier_a$pass, na.rm = TRUE)
  a_no_seam <- sum(tier_a$reason == "no_start_seam")
  a_no_anchor <- sum(tier_a$reason == "no_observed_anchor")
  unchecked <- sum(!tier_c$regime_checked)
  cli::cli_inform(c(
    "{.fn seam_gate}: overall {.val {verdict[['overall']]}}.",
    "*" = "Coverage: {n_judged} of {n_series} series judged by tier A,
           tier B or the hold-out, which is what {.field overall}
           requires.",
    "*" = "Tier A {.val {verdict[['tier_a']]}}: {nrow(tier_a)} series,
           {a_failing} failing, {a_no_seam} with no start seam,
           {a_no_anchor} with no observed row.",
    "*" = "Tier B {.val {verdict[['tier_b']]}}: {n_scored} of
           {nrow(tier_b)} unit-seam pair{?s} scored,
           {b_judged} of {dplyr::n_distinct(tier_b$area_code)}
           container{?s} judged; {vacuous} pair{?s} vacuous by
           construction and out of the verdict.",
    "*" = "Tier B hold-out {.val {verdict[['tier_b_holdout']]}}:
           {sum(holdout_b$status == 'gated')} of {nrow(holdout_b)}
           unit-year{?s} scored over {n_holdout} start-seam series.",
    "*" = "Tier C {.val {verdict[['tier_c']]}}: {nrow(tier_c)}
           container-seam gate{?s},
           {sum(tier_c$pass, na.rm = TRUE)} passing,
           {unchecked} with the regime axis unchecked."
  ))
  invisible(NULL)
}
