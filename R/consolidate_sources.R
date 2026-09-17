#' Consolidate a multi-source panel to one winning row per cell.
#'
#' @description
#'   Reduce a long panel in which several sources report the same
#'   (`.by`, `time_col`) cell to a single winning row per cell, chosen by an
#'   explicit source-priority ranking with measure-aware demotion, a coverage
#'   tie-break, an optional quality tie-break, and a continuity override. It is
#'   the general form of the priority-based deduplication used to build the
#'   long-term historical energy panel.
#'
#' @details
#'   Selection proceeds in four stages.
#'
#'   1. **Hard drop.** Every row whose source ranks at or above `drop_at` is
#'   removed before any cell is contested, so a pinned source can never win even
#'   an uncontested cell. Sources absent from `priority` receive the documented
#'   fallback rank `drop_at - 1L`: kept in play but ranked below every source
#'   listed with a smaller rank. To exclude an unreliable source, list it at
#'   `drop_at` or above.
#'
#'   `priority` may also be **scoped**. A table keyed on the source plus
#'   further columns of `data` (a category, say) pins one source's rank inside
#'   one category while leaving its ordinary rank everywhere else, which
#'   listing that source at a different global rank cannot do: that would move
#'   the outcome in every other category too. A row takes the rank of the most
#'   specific entry it matches, an `NA` scope key meaning "any value", and two
#'   equally specific entries that disagree abort rather than let table order
#'   decide. `priority_scope = "source"` reads the same table with its scoped
#'   entries ignored, which is exactly what it bought before scoping existed
#'   and is therefore the comparison that quantifies what the scope changes.
#'
#'   2. **Measure-aware demotion.** A source can report a different measure than
#'   the panel's target concept (production where the panel means consumption,
#'   generation shares where it means primary energy, a sector fragment where it
#'   means a category total). Rows flagged by `measure$basis` receive
#'   `measure$penalty` added to their effective rank, so a measure-mismatched
#'   source loses any cell a measure-consistent source also reports, yet still
#'   wins a cell it alone reports (a lone reporter is never demoted away). Rows
#'   matching `measure$exempt` keep their base rank (for example world-level
#'   cells, where production equals consumption).
#'
#'   3. **Winner selection.** Within each (`.by`, `time_col`) cell any row with
#'   a real (non-missing) value outranks every `value_col`-missing row, so a
#'   higher-priority source's `NA` never discards a lower-priority source's real
#'   observation; a cell wins `NA` only when no source reports a real value. Among
#'   rows with a real value the winner is the row of lowest effective rank; ties
#'   are broken by broader within-series coverage (the count of cells the source
#'   reports across the `.by` group, or across the coarser group
#'   `tie_break$coverage_by` names) when `tie_break$coverage`, then by
#'   `tie_break$quality_col` ordered per `tie_break$quality_levels`, then by
#'   ascending source name (reported when `verbose`). Coverage counts the cells
#'   where `value_col` is non-missing, or only the strictly positive ones under
#'   `tie_break$coverage = "positive"`, for panels where an exact zero reads as
#'   "not reported" as often as "measured zero" and would otherwise inflate the
#'   coverage of a mostly-zero series.
#'
#'   4. **Continuity override.** When enabled, an isolated single-period winner
#'   flip is reverted: if the immediately preceding and following periods share
#'   a different winner that also reports the middle period, that continuous
#'   source reclaims the middle cell, removing single-period teeth from
#'   otherwise smooth series. The reversion is skipped when the flanking source's
#'   middle-period value is itself missing (continuity never reinstates an `NA`)
#'   and when it would hand a cell won by a measure-consistent source back to a
#'   measure-demoted one: continuity never undoes the measure penalty, because a
#'   single-period source switch is cosmetic while a measure switch corrupts the
#'   series.
#'
#'   A source whose observations are deliberately sparse -- a milestone grid
#'   meant to be interpolated between -- looks like a run of isolated flips
#'   inside another source's annual run, and the override strips every one of
#'   its anchors, collapsing the backbone to a lower-priority partial series.
#'   `continuity_override$exempt` selects the rows the isolation flag never
#'   applies to. `continuity_override$adjacency` states what counts as a
#'   flanking period: `"step"` (the default) requires both neighbours to sit
#'   exactly one time step away, so only a true single-period tooth is
#'   reverted; `"within"` accepts at most one step, which also reverts a flip
#'   flanked at a finer-than-unit spacing in an irregular series.
#'
#'   This operationalises the AFE decision *Consolidate multi-source panels
#'   measure-consistently* (`wiki/decisions/measure-consistent-panel-consolidation`):
#'   measure identity is part of the dedup key's semantics, and priority alone
#'   cannot arbitrate cells whose sources report different measures.
#'
#'   The input must hold at most one row per source per cell; pre-aggregate any
#'   sub-detail rows first (by default the function aborts on duplicates rather
#'   than sum silently). Set `tie_break$quality_variants` when a source
#'   legitimately contributes several `tie_break$quality_col` variants of one
#'   cell (an observed and an interpolated estimate, say): the variants then
#'   collapse to the best-ranked one before any other stage, and only rows
#'   sharing a source, a cell *and* a quality level still abort.
#'
#' @param data A tibble with one row per source per (`.by`, `time_col`) cell.
#' @param value_col Unquoted name of the value column. Coverage counts the cells
#'   where this column is non-missing, or only those where it is strictly
#'   positive when `tie_break$coverage` is `"positive"`.
#' @param source_col Unquoted name of the source-label column.
#' @param priority Source-to-rank map, as a named integer vector
#'   (`c(OWID = 1L, Malanima = 4L)`), a two-column data frame (source, rank),
#'   or a scoped table: a data frame with the source column named as
#'   `source_col`, a `rank` column, and any number of further columns present
#'   in `data` that scope the entry (`NA` matches any value). Lower rank wins.
#'   A row takes the rank of the most specific entry it matches; sources absent
#'   here take the fallback rank `drop_at - 1L`. A data frame with more than
#'   two columns must name them: which column carries the rank cannot be
#'   guessed, and guessing wrong would publish another source's number.
#' @param priority_scope One of `"specific"` (default) or `"source"`, selecting
#'   how a scoped `priority` table is read. `"specific"` honours the scope
#'   keys, so a source can outrank its usual tier in one category only.
#'   `"source"` drops every scoped entry and ranks by source alone, reproducing
#'   what the table expressed before scoping existed; it is the sensitivity
#'   run that says what the scope is worth. No effect on an unscoped
#'   `priority`.
#' @param .by Character vector of grouping columns that, with `time_col`, key a
#'   cell (for example `c("region", "category")`). `NULL` (default) keys cells
#'   by `time_col` alone.
#' @param time_col Unquoted name of the time column. Default: `year`. Must be
#'   numeric; the continuity override treats a difference of one as adjacent.
#' @param drop_at Integer rank at or above which a source is dropped before
#'   consolidation. Default: `100L`.
#' @param measure Optional named list of measure-demotion options:
#'   * `basis`: data frame flagging measure-mismatched rows. It must contain
#'     the source column and may add further key columns present in `data`
#'     (for example a category column) to scope the flag; a data row is
#'     flagged when it matches any `basis` row on all its columns. Default:
#'     `NULL` (no demotion).
#'   * `penalty`: integer added to the effective rank of a flagged,
#'     non-exempt row. Default: `1000L` (larger than any sensible base rank,
#'     so a flagged source falls below every unflagged one while flagged
#'     sources keep their relative order).
#'   * `exempt`: one-sided formula selecting rows the penalty never applies
#'     to, such as `~ region == "WLD"`, evaluated on the rows that survive
#'     the hard drop. Default: `NULL`.
#' @param tie_break Optional named list of options breaking equal-rank ties:
#'   * `coverage`: break ties by broader within-series coverage. `TRUE`
#'     (default), or equivalently `"nonmissing"`, counts the cells where
#'     `value_col` is non-missing; `"positive"` counts only the cells where it
#'     is strictly positive (`value_col` must then be numeric); `FALSE`
#'     disables the coverage tie-break.
#'   * `coverage_by`: character vector of `.by` columns at which coverage is
#'     counted, overriding the default of the full `.by` group. It must be a
#'     subset of `.by` -- coverage may be counted at a coarser grain than the
#'     cell, never at a different one -- and `character(0)` counts a source's
#'     coverage across the whole panel. Counting coverage one level coarser
#'     than the cell lets a source with broad category-level coverage win a tie
#'     in a subcategory where it is thin. Default: `NULL` (the `.by` grain).
#'   * `quality_col`: string naming a quality column used as a tie-break
#'     after coverage. Default: `NULL`.
#'   * `quality_levels`: character vector ordering `quality_col` values best
#'     first (unlisted values rank last). Required when `quality_col` is set.
#'   * `quality_variants`: logical. When `TRUE`, a source contributing several
#'     `quality_col` variants of one cell keeps its best-ranked variant instead
#'     of aborting; rows sharing source, cell and quality level still abort, as
#'     do variants whose best rank is not unique. Requires `quality_col`.
#'     Default: `FALSE`.
#' @param continuity_override Revert isolated single-period winner flips.
#'   `TRUE` (default) or `FALSE`, or a named list of options, which also turns
#'   the override on:
#'   * `adjacency`: one of `"step"` (default) or `"within"`. `"step"` flags a
#'     flip only when both flanking periods sit exactly one time step away;
#'     `"within"` accepts at most one step, flagging flips in a series whose
#'     spacing is finer or irregular.
#'   * `exempt`: one-sided formula selecting winning rows the isolation flag
#'     never applies to, such as `~ source == "Smil_2017"`, evaluated on the
#'     winners. Use it for a source whose observations are deliberately sparse,
#'     which would otherwise lose every anchor to the override. Default:
#'     `NULL`.
#' @param verbose Logical. Report the drop count, how many rows took a
#'   scope-specific priority rank, any resolved quality variants, name-order
#'   ties, and continuity reversions. Default: `TRUE`.
#'
#' @return
#'   A tibble with the winning row per (`.by`, `time_col`) cell, the original
#'   columns of `data`, and five added provenance columns: `n_sources` (distinct
#'   sources contesting the cell after the hard drop), `source_rank` (the
#'   winner's base priority rank), `effective_rank` (base rank plus any measure
#'   penalty applied), `measure_demoted` (whether the winner carried the
#'   measure penalty; a flagged source only wins a cell that no
#'   measure-consistent source reports), and `method_source`, naming the stage
#'   that decided the cell: `"sole_source"` (no rival contested it),
#'   `"nonmissing"` (the rival reported no value), `"priority"` or
#'   `"priority_scoped"` (a lower effective rank, from a source-keyed or a
#'   scope-keyed `priority` entry), `"coverage"`, `"quality"`, `"name_order"`
#'   (ascending source name settled a full tie), or `"continuity"` (the
#'   continuity override handed the cell back). Rows are ordered by `.by` then
#'   `time_col`.
#'
#' @export
#'
#' @examples
#' panel <- tibble::tribble(
#'   ~year, ~region, ~category, ~source, ~value,
#'   1900, "WLD", "Coal", "OWID", 10,
#'   1900, "WLD", "Coal", "Malanima", 20,
#'   1901, "WLD", "Coal", "Malanima", 21,
#'   1902, "WLD", "Coal", "Malanima", 22
#' )
#'
#' consolidate_sources(
#'   panel,
#'   value_col = value,
#'   source_col = source,
#'   priority = c(OWID = 1L, Malanima = 4L),
#'   .by = c("region", "category"),
#'   verbose = FALSE
#' )
#'
#' # A scoped table pins Malanima above OWID for Coal alone: its rank in every
#' # other category stays 4, which inflating its global rank could not do.
#' scoped <- tibble::tribble(
#'   ~source, ~category, ~rank,
#'   "OWID", NA_character_, 1L,
#'   "Malanima", NA_character_, 4L,
#'   "Malanima", "Coal", 0L
#' )
#'
#' consolidate_sources(
#'   panel,
#'   value_col = value,
#'   source_col = source,
#'   priority = scoped,
#'   .by = c("region", "category"),
#'   verbose = FALSE
#' )
consolidate_sources <- function(
  data,
  value_col,
  source_col,
  priority,
  .by = NULL,
  time_col = year,
  drop_at = 100L,
  measure = NULL,
  tie_break = NULL,
  continuity_override = TRUE,
  verbose = TRUE,
  priority_scope = c("specific", "source")
) {
  cols <- list(
    value = rlang::as_name(rlang::enquo(value_col)),
    source = rlang::as_name(rlang::enquo(source_col)),
    time = rlang::as_name(rlang::enquo(time_col)),
    by = .by
  )
  priority_scope <- rlang::arg_match(priority_scope)
  measure <- .cs_measure_opts(measure)
  tie_break <- .cs_tie_break_opts(tie_break)
  continuity <- .cs_continuity_opts(continuity_override)
  .cs_check_inputs(data, cols, measure, tie_break)

  cell_keys <- c(.by, cols$time)
  spec <- .cs_priority_spec(priority, cols$source, names(data), priority_scope)
  work <- .cs_hard_drop(
    tibble::as_tibble(data),
    cols$source,
    spec,
    drop_at,
    verbose
  )
  if (nrow(work) == 0L) {
    return(.cs_empty_result(data))
  }
  work <- .cs_resolve_variants(work, cell_keys, cols, tie_break, verbose)

  work <- .cs_add_effective_rank(work, measure)
  work <- .cs_add_tiebreaks(work, cols, tie_break)

  won <- .cs_select_winners(work, cell_keys, cols$source, verbose)
  if (continuity$on) {
    won <- .cs_apply_continuity(won, work, cols, continuity, verbose)
  }
  .cs_finalize(won, data, cell_keys)
}

# --- Options ------------------------------------------------------------------

.cs_measure_opts <- function(measure) {
  defaults <- list(basis = NULL, penalty = 1000L, exempt = NULL)
  .cs_merge_opts(measure, defaults, "measure")
}

.cs_tie_break_opts <- function(tie_break) {
  defaults <- list(
    coverage = TRUE,
    coverage_by = NULL,
    quality_col = NULL,
    quality_levels = NULL,
    quality_variants = FALSE
  )
  .cs_merge_opts(tie_break, defaults, "tie_break")
}

# `continuity_override` is a switch that may also carry options, the way
# `tie_break$coverage` is a switch that may also carry a mode. A list turns the
# override on and states how it behaves; `TRUE`/`FALSE` keep the defaults.
.cs_continuity_opts <- function(continuity_override) {
  defaults <- list(adjacency = "step", exempt = NULL)
  if (rlang::is_bool(continuity_override)) {
    return(c(defaults, list(on = continuity_override)))
  }
  if (!is.list(continuity_override)) {
    cli::cli_abort(
      "`continuity_override` must be {.code TRUE}, {.code FALSE} or a named list."
    )
  }
  opts <- .cs_merge_opts(continuity_override, defaults, "continuity_override")
  adjacency <- opts$adjacency
  opts$adjacency <- rlang::arg_match(adjacency, values = c("step", "within"))
  c(opts, list(on = TRUE))
}

# "off" | "nonmissing" | "positive". `TRUE`/`FALSE` are the historical spellings
# of "nonmissing"/"off" and stay accepted.
.cs_coverage_mode <- function(coverage) {
  if (rlang::is_bool(coverage)) {
    return(if (coverage) "nonmissing" else "off")
  }
  coverage
}

.cs_merge_opts <- function(opts, defaults, arg_name) {
  if (is.null(opts)) {
    return(defaults)
  }
  if (!is.list(opts) || (length(opts) > 0L && is.null(names(opts)))) {
    cli::cli_abort("`{arg_name}` must be a named list.")
  }
  unknown <- setdiff(names(opts), names(defaults))
  if (length(unknown) > 0L) {
    cli::cli_abort("Unknown `{arg_name}` option{?s}: {.val {unknown}}.")
  }
  utils::modifyList(defaults, opts, keep.null = TRUE)
}

# --- Validation ---------------------------------------------------------------

.cs_check_inputs <- function(data, cols, measure, tie_break) {
  needed <- c(
    cols$value,
    cols$source,
    cols$time,
    cols$by,
    tie_break$quality_col
  )
  missing <- setdiff(needed, names(data))
  if (length(missing) > 0L) {
    cli::cli_abort("Column{?s} not found in `data`: {.val {missing}}.")
  }
  .cs_check_tie_break(data, cols, tie_break)
  .cs_check_measure_basis(data, measure$basis, cols$source)
  reserved <- c(
    "n_sources",
    "source_rank",
    "effective_rank",
    "measure_demoted",
    "method_source"
  )
  clash <- intersect(reserved, names(data))
  if (length(clash) > 0L) {
    cli::cli_abort(
      "`data` already has reserved output column{?s}: {.val {clash}}."
    )
  }
  invisible(NULL)
}

.cs_check_tie_break <- function(data, cols, tie_break) {
  if (!is.null(tie_break$quality_col) && is.null(tie_break$quality_levels)) {
    cli::cli_abort(
      "`tie_break$quality_levels` is required when `tie_break$quality_col` is set."
    )
  }
  .cs_check_coverage_opt(tie_break$coverage, data, cols$value)
  .cs_check_coverage_by(tie_break$coverage_by, cols)
  if (!rlang::is_bool(tie_break$quality_variants)) {
    cli::cli_abort("`tie_break$quality_variants` must be `TRUE` or `FALSE`.")
  }
  if (tie_break$quality_variants && is.null(tie_break$quality_col)) {
    cli::cli_abort(c(
      "`tie_break$quality_variants` requires `tie_break$quality_col`.",
      "i" = "Variants are resolved by `tie_break$quality_levels` order."
    ))
  }
  invisible(NULL)
}

.cs_check_coverage_opt <- function(coverage, data, value_name) {
  known <- c("nonmissing", "positive")
  ok <- rlang::is_bool(coverage) ||
    (rlang::is_string(coverage) && coverage %in% known)
  if (!ok) {
    cli::cli_abort(
      "`tie_break$coverage` must be {.code TRUE}, {.code FALSE}, {.val nonmissing} or {.val positive}."
    )
  }
  positive <- identical(.cs_coverage_mode(coverage), "positive")
  if (positive && !is.numeric(data[[value_name]])) {
    cli::cli_abort(c(
      "`tie_break$coverage = \"positive\"` needs a numeric value column.",
      "i" = "{.val {value_name}} is {.cls {class(data[[value_name]])}}."
    ))
  }
  invisible(NULL)
}

# Coverage may be counted at a coarser grain than the cell, never at a
# different one: a grouping column that is not a cell key would count cells the
# tie-break is not deciding between. `character(0)` is the coarsest legal
# grain, a source's coverage across the whole panel.
.cs_check_coverage_by <- function(coverage_by, cols) {
  if (is.null(coverage_by)) {
    return(invisible(NULL))
  }
  if (!is.character(coverage_by)) {
    cli::cli_abort(
      "`tie_break$coverage_by` must be a character vector of `.by` columns."
    )
  }
  extra <- setdiff(coverage_by, cols$by)
  if (length(extra) > 0L) {
    cli::cli_abort(c(
      "`tie_break$coverage_by` column{?s} not in `.by`: {.val {extra}}.",
      "i" = "Coverage may be counted at a coarser grain than the cell, never a different one."
    ))
  }
  invisible(NULL)
}

.cs_check_measure_basis <- function(data, basis, source_name) {
  if (is.null(basis)) {
    return(invisible(NULL))
  }
  mb_cols <- names(tibble::as_tibble(basis))
  if (!source_name %in% mb_cols) {
    cli::cli_abort(
      "`measure$basis` must contain the source column {.val {source_name}}."
    )
  }
  extra <- setdiff(mb_cols, names(data))
  if (length(extra) > 0L) {
    cli::cli_abort(
      "`measure$basis` column{?s} not in `data`: {.val {extra}}."
    )
  }
  invisible(NULL)
}

.cs_assert_unique <- function(work, keys, quality_col = NULL) {
  if (anyDuplicated(work[keys]) == 0L) {
    return(invisible(NULL))
  }
  if (is.null(quality_col)) {
    cli::cli_abort(c(
      "Multiple rows share the same source and cell.",
      "i" = "Pre-aggregate `data` to one row per source per cell first.",
      "i" = "Set `tie_break$quality_variants = TRUE` to keep a source's best quality variant instead."
    ))
  }
  cli::cli_abort(c(
    "Multiple rows share the same source, cell and {.val {quality_col}} value.",
    "i" = "Pre-aggregate `data` to one row per source per cell and quality level first."
  ))
}

# --- Priority and hard drop ---------------------------------------------------

# The priority table normalised to one shape: the source column, zero or more
# scope-key columns, and `.rank`. Scoping a rank on a (source, category) pair
# is the only way to pin a source inside one category without moving its rank
# everywhere else, which is why the table is keyed rather than named.
.cs_priority_spec <- function(priority, source_name, data_names, scope) {
  tbl <- .cs_priority_table(priority, source_name)
  keys <- setdiff(names(tbl), c(source_name, ".rank"))
  extra <- setdiff(keys, data_names)
  if (length(extra) > 0L) {
    cli::cli_abort("`priority` key column{?s} not in `data`: {.val {extra}}.")
  }
  if (identical(scope, "source") && length(keys) > 0L) {
    tbl <- tbl[!.cs_scoped_rows(tbl, keys), c(source_name, ".rank")]
    keys <- character(0)
  }
  .cs_check_priority_dups(tbl, c(source_name, keys))
  list(tbl = tbl, keys = keys)
}

.cs_priority_table <- function(priority, source_name) {
  if (!is.data.frame(priority)) {
    if (is.null(names(priority))) {
      cli::cli_abort("`priority` vector must be named (source = rank).")
    }
    out <- tibble::tibble(
      .source = as.character(names(priority)),
      .rank = as.integer(priority)
    )
    names(out)[1L] <- source_name
    return(out)
  }
  p <- tibble::as_tibble(priority)
  if (ncol(p) < 2L) {
    cli::cli_abort("`priority` data frame needs >= 2 columns (source, rank).")
  }
  if (ncol(p) == 2L && !all(c(source_name, "rank") %in% names(p))) {
    names(p) <- c(source_name, ".rank")
    return(.cs_coerce_priority(p, source_name))
  }
  .cs_check_named_priority(p, source_name)
  names(p)[names(p) == "rank"] <- ".rank"
  .cs_coerce_priority(p, source_name)
}

.cs_coerce_priority <- function(p, source_name) {
  p[[source_name]] <- as.character(p[[source_name]])
  p$.rank <- as.integer(p$.rank)
  p
}

# Past the two positional columns the table must name them: which column
# carries the rank and which are scope keys cannot be guessed, and guessing
# wrong silently publishes another source's number.
.cs_check_named_priority <- function(p, source_name) {
  absent <- setdiff(c(source_name, "rank"), names(p))
  if (length(absent) > 0L) {
    cli::cli_abort(c(
      "A scoped `priority` table needs the column{?s} {.val {absent}}.",
      "i" = "Name the source column {.val {source_name}} and the rank column {.val rank}; every other column scopes the entry."
    ))
  }
  invisible(NULL)
}

.cs_scoped_rows <- function(tbl, keys) {
  if (length(keys) == 0L) {
    return(rep(FALSE, nrow(tbl)))
  }
  rowSums(!is.na(tbl[keys])) > 0L
}

.cs_check_priority_dups <- function(tbl, key_cols) {
  if (anyDuplicated(tbl[key_cols]) == 0L) {
    return(invisible(NULL))
  }
  cli::cli_abort(c(
    "`priority` gives the same source and scope key more than one rank.",
    "i" = "List one rank per source, or per source and scope-key combination."
  ))
}

.cs_hard_drop <- function(work, source_name, spec, drop_at, verbose) {
  assigned <- .cs_assign_ranks(work, spec, source_name)
  base_rank <- assigned$rank
  base_rank[is.na(base_rank)] <- as.integer(drop_at) - 1L
  work$.base_rank <- as.integer(base_rank)
  work$.rank_scoped <- assigned$scoped
  keep <- work$.base_rank < drop_at
  if (verbose && any(!keep)) {
    cli::cli_alert_info(
      "Dropped {sum(!keep)} row{?s} from sources ranked >= {drop_at} before consolidation."
    )
  }
  .cs_log_scoped_ranks(sum(work$.rank_scoped[keep]), verbose)
  work[keep, , drop = FALSE]
}

.cs_log_scoped_ranks <- function(n_scoped, verbose) {
  if (verbose && n_scoped > 0L) {
    cli::cli_alert_info(
      "{n_scoped} row{?s} took a scope-specific priority rank."
    )
  }
  invisible(NULL)
}

.cs_assign_ranks <- function(work, spec, source_name) {
  probe <- .cs_as_key_chr(work[c(source_name, spec$keys)])
  probe$.cs_row <- seq_len(nrow(work))
  hits <- .cs_rank_matches(probe, spec, source_name)
  rank <- rep(NA_integer_, nrow(work))
  scoped <- rep(FALSE, nrow(work))
  # An empty priority table matches nothing, and `hits` then carries no columns
  # to index by: every row falls through to the `drop_at - 1L` fallback.
  if (nrow(hits) == 0L) {
    return(list(rank = rank, scoped = scoped))
  }
  rank[hits$.cs_row] <- hits$.rank
  scoped[hits$.cs_row] <- hits$.specificity > 0L
  list(rank = rank, scoped = scoped)
}

# Scope keys match as character on both sides: a category column may be a
# factor in `data` and a string in `priority`, or an integer one side and a
# double the other, and none of those is a reason to miss the match.
.cs_as_key_chr <- function(df) {
  dplyr::mutate(df, dplyr::across(dplyr::everything(), as.character))
}

.cs_rank_matches <- function(probe, spec, source_name) {
  tbl <- spec$tbl
  key_cols <- c(source_name, spec$keys)
  tbl[key_cols] <- .cs_as_key_chr(tbl[key_cols])
  if (length(spec$keys) == 0L) {
    out <- dplyr::inner_join(probe, tbl, by = source_name)
    out$.specificity <- 0L
    return(out[c(".cs_row", ".rank", ".specificity")])
  }
  patterns <- split(seq_len(nrow(tbl)), .cs_key_pattern(tbl, spec$keys))
  matched <- purrr::map(
    patterns,
    \(idx) .cs_pattern_match(probe, tbl[idx, ], spec$keys, source_name)
  )
  .cs_resolve_specificity(dplyr::bind_rows(matched))
}

# Priority rows sharing a pattern of stated (non-`NA`) keys join in one pass,
# so the match costs one join per distinct pattern rather than one per entry.
.cs_key_pattern <- function(tbl, keys) {
  do.call(paste0, lapply(keys, \(k) as.integer(is.na(tbl[[k]]))))
}

.cs_pattern_match <- function(probe, rows, keys, source_name) {
  stated <- keys[!is.na(unlist(rows[1L, keys], use.names = FALSE))]
  by <- c(source_name, stated)
  out <- dplyr::inner_join(probe, rows[c(by, ".rank")], by = by)
  out$.specificity <- length(stated)
  out[c(".cs_row", ".rank", ".specificity")]
}

# A row takes the rank of the most specific entry it matches. Two entries that
# are equally specific and disagree would make the published value depend on
# the order of the priority table, so they abort instead.
.cs_resolve_specificity <- function(matched) {
  if (nrow(matched) == 0L) {
    return(matched)
  }
  best <- dplyr::filter(
    matched,
    .specificity == max(.specificity),
    .by = .cs_row
  )
  clash <- best |>
    dplyr::summarise(.n_rank = dplyr::n_distinct(.rank), .by = .cs_row) |>
    dplyr::filter(.n_rank > 1L)
  if (nrow(clash) > 0L) {
    cli::cli_abort(c(
      "{nrow(clash)} row{?s} matched two equally specific `priority` entries with different ranks.",
      "i" = "Scope the entries on the same key columns, or give them one rank."
    ))
  }
  dplyr::distinct(best, .cs_row, .keep_all = TRUE)
}

# An all-dropped input yields no winning cells: return the shaped empty tibble
# (original columns plus the five provenance columns) rather than erroring, so a
# panel of only pinned sources consolidates to zero rows just like any other.
.cs_empty_result <- function(data) {
  out <- tibble::as_tibble(data)[0, , drop = FALSE]
  out$n_sources <- integer(0)
  out$source_rank <- integer(0)
  out$effective_rank <- integer(0)
  out$measure_demoted <- logical(0)
  out$method_source <- character(0)
  out
}

# --- Per-source quality variants ----------------------------------------------

# Duplicate (cell, source) rows abort by default: silently summing them would
# hide a double-counted series. When `tie_break$quality_variants` is on they are
# read as one source's quality variants of the same cell (an observed and an
# interpolated estimate, say) and collapse to the best-ranked variant. This runs
# before coverage and rank, so every later stage still sees one row per source
# per cell. It only ever drops rows, never reorders them.
.cs_resolve_variants <- function(work, cell_keys, cols, tie_break, verbose) {
  keys <- c(cell_keys, cols$source)
  if (!tie_break$quality_variants) {
    .cs_assert_unique(work, keys)
    return(work)
  }
  quality_col <- tie_break$quality_col
  .cs_assert_unique(work, c(keys, quality_col), quality_col)
  best <- .cs_best_variant_mask(
    work[keys],
    .cs_quality_rank(work[[quality_col]], tie_break$quality_levels)
  )
  .cs_check_variant_tie(work[best, keys, drop = FALSE])
  .cs_log_variants(sum(!best), verbose)
  work[best, , drop = FALSE]
}

.cs_best_variant_mask <- function(key_df, rank) {
  key_names <- names(key_df)
  key_df$.variant_rank <- rank
  key_df |>
    dplyr::mutate(
      .best_variant = .variant_rank == min(.variant_rank),
      .by = dplyr::all_of(key_names)
    ) |>
    dplyr::pull(.best_variant)
}

# Every quality value outside `quality_levels` ranks last, so two unlisted
# variants of one cell tie for best. Resolving that would need a tie-break the
# caller never stated, so it aborts instead.
.cs_check_variant_tie <- function(best_keys) {
  if (anyDuplicated(best_keys) > 0L) {
    cli::cli_abort(c(
      "Per-source cell variants tie on best quality rank.",
      "i" = "Values outside `tie_break$quality_levels` all rank last; list every quality level to break the tie."
    ))
  }
  invisible(NULL)
}

.cs_log_variants <- function(n_dropped, verbose) {
  if (verbose && n_dropped > 0L) {
    cli::cli_alert_info(
      "Resolved {n_dropped} per-source cell variant{?s} to the best quality level."
    )
  }
  invisible(NULL)
}

# --- Effective rank (measure demotion) ----------------------------------------

.cs_add_effective_rank <- function(work, measure) {
  flag <- .cs_measure_flag(work, measure$basis)
  exempt <- if (is.null(measure$exempt)) {
    rep(FALSE, nrow(work))
  } else {
    .cs_eval_exempt(work, measure$exempt)
  }
  work$.measure_demoted <- flag & !exempt
  work$.effective_rank <- work$.base_rank +
    as.integer(measure$penalty) * work$.measure_demoted
  work
}

.cs_measure_flag <- function(work, measure_basis) {
  if (is.null(measure_basis)) {
    return(rep(FALSE, nrow(work)))
  }
  mb <- tibble::as_tibble(measure_basis)
  keys <- names(mb)
  probe <- work[keys]
  probe$.cs_row <- seq_len(nrow(work))
  hit <- dplyr::semi_join(probe, mb, by = keys)
  seq_len(nrow(work)) %in% hit$.cs_row
}

.cs_eval_exempt <- function(work, exempt) {
  .cs_eval_mask(work, exempt, "measure$exempt")
}

.cs_eval_mask <- function(work, exempt, arg_name) {
  if (!rlang::is_formula(exempt, lhs = FALSE)) {
    cli::cli_abort(
      "`{arg_name}` must be a one-sided formula, e.g. `~ region == \"WLD\"`."
    )
  }
  mask <- rlang::eval_tidy(rlang::as_quosure(exempt), data = work)
  if (!is.logical(mask) || length(mask) != nrow(work)) {
    cli::cli_abort(
      "`{arg_name}` must evaluate to one logical per row."
    )
  }
  mask[is.na(mask)] <- FALSE
  mask
}

# --- Coverage, quality, n_sources ---------------------------------------------

.cs_add_tiebreaks <- function(work, cols, tie_break) {
  cell_keys <- c(cols$by, cols$time)
  mode <- .cs_coverage_mode(tie_break$coverage)
  work$.value_na <- is.na(work[[cols$value]])
  work <- .cs_add_coverage(work, cols, mode, tie_break$coverage_by)
  work$.coverage_ord <- if (mode == "off") 0L else work$.coverage
  work$.quality_rank <- if (is.null(tie_break$quality_col)) {
    0L
  } else {
    .cs_quality_rank(
      work[[tie_break$quality_col]],
      tie_break$quality_levels
    )
  }
  .cs_add_n_sources(work, cell_keys, cols$source)
}

# Coverage always counts cells -- the (`.by`, `time_col`) combinations a source
# reports -- and `coverage_by` moves only the grain they are counted at. At the
# default grain the two are the same thing; at a coarser one a source with
# broad category-level coverage can win a tie in a subcategory where it is
# thin, which is a different tie-break answer, not a different unit.
.cs_add_coverage <- function(work, cols, mode, coverage_by) {
  cover_grp <- if (is.null(coverage_by)) cols$by else coverage_by
  grp <- c(cover_grp, cols$source)
  cell_keys <- c(cols$by, cols$time)
  counted <- .cs_coverage_keep(work[[cols$value]], mode)
  cov <- work[counted, union(grp, cell_keys), drop = FALSE]
  cov <- dplyr::distinct(cov)
  cov <- dplyr::count(
    cov,
    dplyr::across(dplyr::all_of(grp)),
    name = ".coverage"
  )
  out <- dplyr::left_join(work, cov, by = grp)
  out$.coverage <- dplyr::coalesce(out$.coverage, 0L)
  out
}

# Which cells count towards a source's coverage. "positive" exists for panels
# where an exact zero reads as "not reported" as often as "measured zero", so
# counting it would inflate the coverage of a series that is mostly zeros.
.cs_coverage_keep <- function(x, mode) {
  if (identical(mode, "positive")) {
    return(!is.na(x) & x > 0)
  }
  !is.na(x)
}

.cs_add_n_sources <- function(work, cell_keys, source_name) {
  ns <- dplyr::distinct(
    work,
    dplyr::across(dplyr::all_of(c(cell_keys, source_name)))
  )
  ns <- dplyr::count(
    ns,
    dplyr::across(dplyr::all_of(cell_keys)),
    name = ".n_sources"
  )
  dplyr::left_join(work, ns, by = cell_keys)
}

.cs_quality_rank <- function(x, levels) {
  r <- match(as.character(x), levels)
  r[is.na(r)] <- length(levels) + 1L
  as.integer(r)
}

# --- Winner selection ---------------------------------------------------------

.cs_select_winners <- function(work, cell_keys, source_name, verbose) {
  ordered <- dplyr::arrange(
    work,
    .value_na,
    .effective_rank,
    dplyr::desc(.coverage_ord),
    .quality_rank,
    .data[[source_name]]
  )
  if (verbose) {
    .cs_log_name_ties(ordered, cell_keys, source_name)
  }
  top <- ordered |>
    dplyr::group_by(dplyr::across(dplyr::all_of(cell_keys))) |>
    dplyr::slice_head(n = 2L) |>
    dplyr::mutate(.pos = dplyr::row_number()) |>
    dplyr::ungroup()
  won <- top[top$.pos == 1L, , drop = FALSE]
  won$.method <- .cs_decide_method(
    won,
    top[top$.pos == 2L, , drop = FALSE],
    cell_keys
  )
  won$.pos <- NULL
  won
}

# Which stage decided the cell, recorded per row as `method_source`. The
# winner is compared with its closest rival, the runner-up in the same
# ordering: the first ordering field the two differ on is what the winner won
# on, and agreement on all of them means ascending source name settled it.
.cs_decide_method <- function(won, rival, cell_keys) {
  fields <- c(".value_na", ".effective_rank", ".coverage_ord", ".quality_rank")
  challenger <- rival[c(cell_keys, fields)]
  names(challenger) <- c(cell_keys, paste0(fields, "_rival"))
  cmp <- dplyr::left_join(
    won[c(cell_keys, fields, ".rank_scoped")],
    challenger,
    by = cell_keys
  )
  dplyr::case_when(
    is.na(cmp$.effective_rank_rival) ~ "sole_source",
    cmp$.value_na != cmp$.value_na_rival ~ "nonmissing",
    cmp$.effective_rank != cmp$.effective_rank_rival & cmp$.rank_scoped ~
      "priority_scoped",
    cmp$.effective_rank != cmp$.effective_rank_rival ~ "priority",
    cmp$.coverage_ord != cmp$.coverage_ord_rival ~ "coverage",
    cmp$.quality_rank != cmp$.quality_rank_rival ~ "quality",
    .default = "name_order"
  )
}

.cs_log_name_ties <- function(ordered, cell_keys, source_name) {
  key_cols <- c(
    ".value_na",
    ".effective_rank",
    ".coverage_ord",
    ".quality_rank"
  )
  tie <- ordered |>
    dplyr::group_by(dplyr::across(dplyr::all_of(cell_keys))) |>
    dplyr::filter(
      dplyr::if_all(dplyr::all_of(key_cols), \(v) v == dplyr::first(v))
    ) |>
    dplyr::summarise(
      .n_tie = dplyr::n_distinct(.data[[source_name]]),
      .groups = "drop"
    ) |>
    dplyr::filter(.n_tie > 1L)
  if (nrow(tie) > 0L) {
    cli::cli_alert_warning(
      "Ascending source name resolved {nrow(tie)} cell tie{?s} after rank, coverage, and quality ranking."
    )
  }
  invisible(NULL)
}

# --- Continuity override ------------------------------------------------------

.cs_apply_continuity <- function(won, work, cols, continuity, verbose) {
  cell_keys <- c(cols$by, cols$time)
  flagged <- .cs_flag_isolated(won, cols, continuity)
  clean <- .cs_drop_iso_cols(flagged)
  iso <- flagged[flagged$.isolated, , drop = FALSE]
  if (nrow(iso) == 0L) {
    return(clean)
  }
  repl_keys <- iso[cell_keys]
  repl_keys[[cols$source]] <- iso$.neighbor
  repl <- dplyr::inner_join(work, repl_keys, by = c(cell_keys, cols$source))
  repl <- repl[!repl$.value_na, , drop = FALSE]
  repl <- .cs_block_demoting_reversion(repl, iso, cell_keys)
  if (nrow(repl) == 0L) {
    return(clean)
  }
  repl$.method <- "continuity"
  out <- dplyr::bind_rows(
    dplyr::anti_join(clean, repl, by = cell_keys),
    repl[names(clean)]
  )
  if (verbose) {
    cli::cli_alert_info(
      "Continuity override reverted {nrow(repl)} isolated single-period winner flip{?s}."
    )
  }
  out
}

# Continuity must not reintroduce measure mixing: an isolated
# measure-consistent winner is never handed back to a measure-demoted flanking
# source. The reversion stays allowed when both rows carry the penalty.
.cs_block_demoting_reversion <- function(repl, iso, cell_keys) {
  displaced <- iso[c(cell_keys, ".measure_demoted")]
  names(displaced)[names(displaced) == ".measure_demoted"] <-
    ".displaced_demoted"
  repl <- dplyr::left_join(repl, displaced, by = cell_keys)
  keep <- !(repl$.measure_demoted & !repl$.displaced_demoted)
  repl$.displaced_demoted <- NULL
  repl[keep, , drop = FALSE]
}

.cs_flag_isolated <- function(won, cols, continuity) {
  time_name <- cols$time
  source_name <- cols$source
  won$.cs_exempt <- .cs_continuity_exempt(won, continuity$exempt)
  won |>
    dplyr::group_by(dplyr::across(dplyr::all_of(cols$by))) |>
    dplyr::arrange(.data[[time_name]], .by_group = TRUE) |>
    dplyr::mutate(
      .prev_source = dplyr::lag(.data[[source_name]]),
      .next_source = dplyr::lead(.data[[source_name]]),
      .prev_time = dplyr::lag(as.numeric(.data[[time_name]])),
      .next_time = dplyr::lead(as.numeric(.data[[time_name]])),
      .neighbor = .prev_source,
      .isolated = !.cs_exempt &
        !is.na(.prev_source) &
        !is.na(.next_source) &
        .prev_source == .next_source &
        .data[[source_name]] != .prev_source &
        .cs_adjacent(
          as.numeric(.data[[time_name]]) - .prev_time,
          continuity$adjacency
        ) &
        .cs_adjacent(
          .next_time - as.numeric(.data[[time_name]]),
          continuity$adjacency
        )
    ) |>
    dplyr::ungroup()
}

# A source whose observations are deliberately sparse -- a milestone grid meant
# to be interpolated between -- is a run of isolated flips inside another
# source's annual run, and the override would strip every one of its anchors.
# The exemption keeps those cells with the source that reported them.
.cs_continuity_exempt <- function(won, exempt) {
  if (is.null(exempt)) {
    return(rep(FALSE, nrow(won)))
  }
  .cs_eval_mask(won, exempt, "continuity_override$exempt")
}

# "step": both flanking periods sit exactly one time step away, so only a true
# single-period tooth is reverted -- the conservative reading, and the default.
# "within": at most one step, which also reverts a flip flanked at a finer
# spacing in a series whose time axis is not a regular unit grid.
.cs_adjacent <- function(gap, adjacency) {
  if (identical(adjacency, "within")) {
    return(!is.na(gap) & gap <= 1)
  }
  !is.na(gap) & gap == 1
}

.cs_drop_iso_cols <- function(flagged) {
  iso_cols <- c(
    ".cs_exempt",
    ".prev_source",
    ".next_source",
    ".prev_time",
    ".next_time",
    ".neighbor",
    ".isolated"
  )
  flagged[setdiff(names(flagged), iso_cols)]
}

# --- Finalize -----------------------------------------------------------------

.cs_finalize <- function(won, data, cell_keys) {
  won$source_rank <- won$.base_rank
  won$effective_rank <- won$.effective_rank
  won$measure_demoted <- won$.measure_demoted
  won$n_sources <- won$.n_sources
  won$method_source <- won$.method
  keep <- c(
    names(data),
    "n_sources",
    "source_rank",
    "effective_rank",
    "measure_demoted",
    "method_source"
  )
  won <- won[intersect(keep, names(won))]
  won <- dplyr::arrange(won, dplyr::across(dplyr::all_of(cell_keys)))
  tibble::as_tibble(won)
}
