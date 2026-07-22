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
#'   2. **Measure-aware demotion.** A source can report a different measure than
#'   the panel's target concept (production where the panel means consumption,
#'   generation shares where it means primary energy, a sector fragment where it
#'   means a category total). Rows flagged by `measure_basis` receive
#'   `measure_penalty` added to their effective rank, so a measure-mismatched
#'   source loses any cell a measure-consistent source also reports, yet still
#'   wins a cell it alone reports (a lone reporter is never demoted away). Rows
#'   matching `measure_exempt` keep their base rank (for example world-level
#'   cells, where production equals consumption).
#'
#'   3. **Winner selection.** Within each (`.by`, `time_col`) cell the winner is
#'   the row of lowest effective rank; ties are broken by broader within-series
#'   coverage (the count of cells the source reports across the `.by` group)
#'   when `tie_break_coverage`, then by `quality_col` ordered per
#'   `quality_levels`, then by ascending source name (reported when `verbose`).
#'
#'   4. **Continuity override.** When enabled, an isolated single-period winner
#'   flip is reverted: if the immediately preceding and following periods share
#'   a different winner that also reports the middle period, that continuous
#'   source reclaims the middle cell, removing single-period teeth from
#'   otherwise smooth series.
#'
#'   This operationalises the AFE decision *Consolidate multi-source panels
#'   measure-consistently* (`wiki/decisions/measure-consistent-panel-consolidation`):
#'   measure identity is part of the dedup key's semantics, and priority alone
#'   cannot arbitrate cells whose sources report different measures.
#'
#'   The input must hold at most one row per source per cell; pre-aggregate any
#'   sub-detail rows first (the function aborts on duplicates rather than sum
#'   silently).
#'
#' @param data A tibble with one row per source per (`.by`, `time_col`) cell.
#' @param value_col Unquoted name of the value column. Coverage counts the cells
#'   where this column is non-missing.
#' @param source_col Unquoted name of the source-label column.
#' @param priority Source-to-rank map, as either a named integer vector
#'   (`c(OWID = 1L, Malanima = 4L)`) or a two-column data frame (source, rank).
#'   Lower rank wins. Sources absent here take the fallback rank `drop_at - 1L`.
#' @param .by Character vector of grouping columns that, with `time_col`, key a
#'   cell (for example `c("region", "category")`). `NULL` (default) keys cells
#'   by `time_col` alone.
#' @param time_col Unquoted name of the time column. Default: `year`. Must be
#'   numeric; the continuity override treats a difference of one as adjacent.
#' @param drop_at Integer rank at or above which a source is dropped before
#'   consolidation. Default: `100L`.
#' @param measure_basis Optional data frame flagging measure-mismatched rows. It
#'   must contain the source column and may add further key columns present in
#'   `data` (for example a category column) to scope the flag; a data row is
#'   flagged when it matches any `measure_basis` row on all its columns. Default:
#'   `NULL` (no demotion).
#' @param measure_penalty Integer added to the effective rank of a flagged,
#'   non-exempt row. Default: `1000L` (larger than any sensible base rank, so a
#'   flagged source falls below every unflagged one while flagged sources keep
#'   their relative order).
#' @param measure_exempt Optional filter expression (evaluated in `data`)
#'   selecting rows the penalty never applies to, such as `region == "WLD"`.
#'   Default: `NULL`.
#' @param tie_break_coverage Logical. Break equal-rank ties by broader coverage.
#'   Default: `TRUE`.
#' @param quality_col Optional unquoted name of a quality column used as a
#'   tie-break after coverage. Default: `NULL`.
#' @param quality_levels Character vector ordering `quality_col` values best
#'   first (unlisted values rank last). Required when `quality_col` is set.
#' @param continuity_override Logical. Revert isolated single-period winner
#'   flips. Default: `TRUE`.
#' @param verbose Logical. Report the drop count, name-order ties, and
#'   continuity reversions. Default: `TRUE`.
#'
#' @return
#'   A tibble with the winning row per (`.by`, `time_col`) cell, the original
#'   columns of `data`, and four added provenance columns: `n_sources` (distinct
#'   sources contesting the cell after the hard drop), `source_rank` (the
#'   winner's base priority rank), `effective_rank` (base rank plus any measure
#'   penalty applied), and `measure_demoted` (whether the winner carried the
#'   measure penalty, true only for a lone flagged source). Rows are ordered by
#'   `.by` then `time_col`.
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
consolidate_sources <- function(
  data,
  value_col,
  source_col,
  priority,
  .by = NULL,
  time_col = year,
  drop_at = 100L,
  measure_basis = NULL,
  measure_penalty = 1000L,
  measure_exempt = NULL,
  tie_break_coverage = TRUE,
  quality_col = NULL,
  quality_levels = NULL,
  continuity_override = TRUE,
  verbose = TRUE
) {
  value_name <- rlang::as_name(rlang::enquo(value_col))
  source_name <- rlang::as_name(rlang::enquo(source_col))
  time_name <- rlang::as_name(rlang::enquo(time_col))
  quality_quo <- rlang::enquo(quality_col)
  has_quality <- !rlang::quo_is_null(quality_quo)
  quality_name <- if (has_quality) rlang::as_name(quality_quo) else NULL
  exempt_quo <- rlang::enquo(measure_exempt)
  has_exempt <- !rlang::quo_is_null(exempt_quo)

  .cs_check_inputs(
    data,
    value_name,
    source_name,
    time_name,
    .by,
    quality_name,
    has_quality,
    quality_levels,
    measure_basis
  )

  cell_keys <- c(.by, time_name)
  work <- .cs_hard_drop(
    tibble::as_tibble(data),
    source_name,
    priority,
    drop_at,
    verbose
  )
  if (nrow(work) == 0L) {
    return(.cs_empty_result(data))
  }
  .cs_assert_unique(work, c(cell_keys, source_name))

  work <- .cs_add_effective_rank(
    work,
    measure_basis,
    measure_penalty,
    exempt_quo,
    has_exempt
  )
  work <- .cs_add_tiebreaks(
    work,
    .by,
    source_name,
    value_name,
    time_name,
    cell_keys,
    tie_break_coverage,
    quality_name,
    has_quality,
    quality_levels
  )

  won <- .cs_select_winners(work, cell_keys, source_name, verbose)
  if (continuity_override) {
    won <- .cs_apply_continuity(
      won,
      work,
      .by,
      time_name,
      source_name,
      cell_keys,
      verbose
    )
  }
  .cs_finalize(won, data, cell_keys)
}

# --- Validation ---------------------------------------------------------------

.cs_check_inputs <- function(
  data,
  value_name,
  source_name,
  time_name,
  by,
  quality_name,
  has_quality,
  quality_levels,
  measure_basis
) {
  needed <- c(value_name, source_name, time_name, by, quality_name)
  missing <- setdiff(needed, names(data))
  if (length(missing) > 0L) {
    cli::cli_abort("Column{?s} not found in `data`: {.val {missing}}.")
  }
  if (has_quality && is.null(quality_levels)) {
    cli::cli_abort("`quality_levels` is required when `quality_col` is set.")
  }
  if (!is.null(measure_basis)) {
    mb_cols <- names(tibble::as_tibble(measure_basis))
    if (!source_name %in% mb_cols) {
      cli::cli_abort(
        "`measure_basis` must contain the source column {.val {source_name}}."
      )
    }
    extra <- setdiff(mb_cols, names(data))
    if (length(extra) > 0L) {
      cli::cli_abort(
        "`measure_basis` column{?s} not in `data`: {.val {extra}}."
      )
    }
  }
  reserved <- c("n_sources", "source_rank", "effective_rank", "measure_demoted")
  clash <- intersect(reserved, names(data))
  if (length(clash) > 0L) {
    cli::cli_abort(
      "`data` already has reserved output column{?s}: {.val {clash}}."
    )
  }
  invisible(NULL)
}

.cs_assert_unique <- function(work, keys) {
  if (anyDuplicated(work[keys]) > 0L) {
    cli::cli_abort(c(
      "Multiple rows share the same source and cell.",
      "i" = "Pre-aggregate `data` to one row per source per cell first."
    ))
  }
  invisible(NULL)
}

# --- Priority and hard drop ---------------------------------------------------

.cs_priority_vector <- function(priority) {
  if (is.data.frame(priority)) {
    if (ncol(priority) < 2L) {
      cli::cli_abort("`priority` data frame needs >= 2 columns (source, rank).")
    }
    return(stats::setNames(
      as.integer(priority[[2L]]),
      as.character(priority[[1L]])
    ))
  }
  if (is.null(names(priority))) {
    cli::cli_abort("`priority` vector must be named (source = rank).")
  }
  stats::setNames(as.integer(priority), names(priority))
}

.cs_hard_drop <- function(work, source_name, priority, drop_at, verbose) {
  ranks <- .cs_priority_vector(priority)
  base_rank <- unname(ranks[as.character(work[[source_name]])])
  base_rank[is.na(base_rank)] <- as.integer(drop_at) - 1L
  work$.base_rank <- as.integer(base_rank)
  keep <- work$.base_rank < drop_at
  if (verbose && any(!keep)) {
    cli::cli_alert_info(
      "Dropped {sum(!keep)} row{?s} from sources ranked >= {drop_at} before consolidation."
    )
  }
  work[keep, , drop = FALSE]
}

# An all-dropped input yields no winning cells: return the shaped empty tibble
# (original columns plus the four provenance columns) rather than erroring, so a
# panel of only pinned sources consolidates to zero rows just like any other.
.cs_empty_result <- function(data) {
  out <- tibble::as_tibble(data)[0, , drop = FALSE]
  out$n_sources <- integer(0)
  out$source_rank <- integer(0)
  out$effective_rank <- integer(0)
  out$measure_demoted <- logical(0)
  out
}

# --- Effective rank (measure demotion) ----------------------------------------

.cs_add_effective_rank <- function(
  work,
  measure_basis,
  measure_penalty,
  exempt_quo,
  has_exempt
) {
  flag <- .cs_measure_flag(work, measure_basis)
  exempt <- if (has_exempt) {
    .cs_eval_exempt(work, exempt_quo)
  } else {
    rep(FALSE, nrow(work))
  }
  work$.measure_demoted <- flag & !exempt
  work$.effective_rank <- work$.base_rank +
    as.integer(measure_penalty) * work$.measure_demoted
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

.cs_eval_exempt <- function(work, exempt_quo) {
  mask <- rlang::eval_tidy(exempt_quo, data = work)
  if (!is.logical(mask) || length(mask) != nrow(work)) {
    cli::cli_abort(
      "`measure_exempt` must evaluate to a logical vector of length nrow(data)."
    )
  }
  mask[is.na(mask)] <- FALSE
  mask
}

# --- Coverage, quality, n_sources ---------------------------------------------

.cs_add_tiebreaks <- function(
  work,
  by,
  source_name,
  value_name,
  time_name,
  cell_keys,
  tie_break_coverage,
  quality_name,
  has_quality,
  quality_levels
) {
  work <- .cs_add_coverage(work, by, source_name, value_name, time_name)
  work$.coverage_ord <- if (tie_break_coverage) work$.coverage else 0L
  work$.quality_rank <- if (has_quality) {
    .cs_quality_rank(work[[quality_name]], quality_levels)
  } else {
    0L
  }
  .cs_add_n_sources(work, cell_keys, source_name)
}

.cs_add_coverage <- function(work, by, source_name, value_name, time_name) {
  grp <- c(by, source_name)
  cov <- work[!is.na(work[[value_name]]), c(grp, time_name), drop = FALSE]
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
    .effective_rank,
    dplyr::desc(.coverage_ord),
    .quality_rank,
    .data[[source_name]]
  )
  if (verbose) {
    .cs_log_name_ties(ordered, cell_keys, source_name)
  }
  ordered |>
    dplyr::group_by(dplyr::across(dplyr::all_of(cell_keys))) |>
    dplyr::slice_head(n = 1L) |>
    dplyr::ungroup()
}

.cs_log_name_ties <- function(ordered, cell_keys, source_name) {
  key_cols <- c(".effective_rank", ".coverage_ord", ".quality_rank")
  tie <- ordered |>
    dplyr::group_by(dplyr::across(dplyr::all_of(cell_keys))) |>
    dplyr::filter(
      dplyr::across(dplyr::all_of(key_cols), \(v) v == dplyr::first(v))
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

.cs_apply_continuity <- function(
  won,
  work,
  by,
  time_name,
  source_name,
  cell_keys,
  verbose
) {
  flagged <- .cs_flag_isolated(won, by, time_name, source_name)
  clean <- .cs_drop_iso_cols(flagged)
  iso <- flagged[flagged$.isolated, , drop = FALSE]
  if (nrow(iso) == 0L) {
    return(clean)
  }
  repl_keys <- iso[cell_keys]
  repl_keys[[source_name]] <- iso$.neighbor
  repl <- dplyr::inner_join(work, repl_keys, by = c(cell_keys, source_name))
  if (nrow(repl) == 0L) {
    return(clean)
  }
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

.cs_flag_isolated <- function(won, by, time_name, source_name) {
  won |>
    dplyr::group_by(dplyr::across(dplyr::all_of(by))) |>
    dplyr::arrange(.data[[time_name]], .by_group = TRUE) |>
    dplyr::mutate(
      .prev_source = dplyr::lag(.data[[source_name]]),
      .next_source = dplyr::lead(.data[[source_name]]),
      .prev_time = dplyr::lag(as.numeric(.data[[time_name]])),
      .next_time = dplyr::lead(as.numeric(.data[[time_name]])),
      .neighbor = .prev_source,
      .isolated = !is.na(.prev_source) &
        !is.na(.next_source) &
        .prev_source == .next_source &
        .data[[source_name]] != .prev_source &
        (as.numeric(.data[[time_name]]) - .prev_time) == 1 &
        (.next_time - as.numeric(.data[[time_name]])) == 1
    ) |>
    dplyr::ungroup()
}

.cs_drop_iso_cols <- function(flagged) {
  iso_cols <- c(
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
  keep <- c(
    names(data),
    "n_sources",
    "source_rank",
    "effective_rank",
    "measure_demoted"
  )
  won <- won[intersect(keep, names(won))]
  won <- dplyr::arrange(won, dplyr::across(dplyr::all_of(cell_keys)))
  tibble::as_tibble(won)
}
