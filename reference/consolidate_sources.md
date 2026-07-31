# Consolidate a multi-source panel to one winning row per cell.

Reduce a long panel in which several sources report the same (`.by`,
`time_col`) cell to a single winning row per cell, chosen by an explicit
source-priority ranking with measure-aware demotion, a coverage
tie-break, an optional quality tie-break, and a continuity override. It
is the general form of the priority-based deduplication used to build
the long-term historical energy panel.

## Usage

``` r
consolidate_sources(
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
  verbose = TRUE
)
```

## Arguments

- data:

  A tibble with one row per source per (`.by`, `time_col`) cell.

- value_col:

  Unquoted name of the value column. Coverage counts the cells where
  this column is non-missing, or only those where it is strictly
  positive when `tie_break$coverage` is `"positive"`.

- source_col:

  Unquoted name of the source-label column.

- priority:

  Source-to-rank map, as either a named integer vector
  (`c(OWID = 1L, Malanima = 4L)`) or a two-column data frame (source,
  rank). Lower rank wins. Sources absent here take the fallback rank
  `drop_at - 1L`.

- .by:

  Character vector of grouping columns that, with `time_col`, key a cell
  (for example `c("region", "category")`). `NULL` (default) keys cells
  by `time_col` alone.

- time_col:

  Unquoted name of the time column. Default: `year`. Must be numeric;
  the continuity override treats a difference of one as adjacent.

- drop_at:

  Integer rank at or above which a source is dropped before
  consolidation. Default: `100L`.

- measure:

  Optional named list of measure-demotion options:

  - `basis`: data frame flagging measure-mismatched rows. It must
    contain the source column and may add further key columns present in
    `data` (for example a category column) to scope the flag; a data row
    is flagged when it matches any `basis` row on all its columns.
    Default: `NULL` (no demotion).

  - `penalty`: integer added to the effective rank of a flagged,
    non-exempt row. Default: `1000L` (larger than any sensible base
    rank, so a flagged source falls below every unflagged one while
    flagged sources keep their relative order).

  - `exempt`: one-sided formula selecting rows the penalty never applies
    to, such as `~ region == "WLD"`, evaluated on the rows that survive
    the hard drop. Default: `NULL`.

- tie_break:

  Optional named list of options breaking equal-rank ties:

  - `coverage`: break ties by broader within-series coverage. `TRUE`
    (default), or equivalently `"nonmissing"`, counts the cells where
    `value_col` is non-missing; `"positive"` counts only the cells where
    it is strictly positive (`value_col` must then be numeric); `FALSE`
    disables the coverage tie-break.

  - `quality_col`: string naming a quality column used as a tie-break
    after coverage. Default: `NULL`.

  - `quality_levels`: character vector ordering `quality_col` values
    best first (unlisted values rank last). Required when `quality_col`
    is set.

  - `quality_variants`: logical. When `TRUE`, a source contributing
    several `quality_col` variants of one cell keeps its best-ranked
    variant instead of aborting; rows sharing source, cell and quality
    level still abort, as do variants whose best rank is not unique.
    Requires `quality_col`. Default: `FALSE`.

- continuity_override:

  Logical. Revert isolated single-period winner flips. Default: `TRUE`.

- verbose:

  Logical. Report the drop count, any resolved quality variants,
  name-order ties, and continuity reversions. Default: `TRUE`.

## Value

A tibble with the winning row per (`.by`, `time_col`) cell, the original
columns of `data`, and four added provenance columns: `n_sources`
(distinct sources contesting the cell after the hard drop),
`source_rank` (the winner's base priority rank), `effective_rank` (base
rank plus any measure penalty applied), and `measure_demoted` (whether
the winner carried the measure penalty; a flagged source only wins a
cell that no measure-consistent source reports). Rows are ordered by
`.by` then `time_col`.

## Details

Selection proceeds in four stages.

1.  **Hard drop.** Every row whose source ranks at or above `drop_at` is
    removed before any cell is contested, so a pinned source can never
    win even an uncontested cell. Sources absent from `priority` receive
    the documented fallback rank `drop_at - 1L`: kept in play but ranked
    below every source listed with a smaller rank. To exclude an
    unreliable source, list it at `drop_at` or above.

2.  **Measure-aware demotion.** A source can report a different measure
    than the panel's target concept (production where the panel means
    consumption, generation shares where it means primary energy, a
    sector fragment where it means a category total). Rows flagged by
    `measure$basis` receive `measure$penalty` added to their effective
    rank, so a measure-mismatched source loses any cell a
    measure-consistent source also reports, yet still wins a cell it
    alone reports (a lone reporter is never demoted away). Rows matching
    `measure$exempt` keep their base rank (for example world-level
    cells, where production equals consumption).

3.  **Winner selection.** Within each (`.by`, `time_col`) cell any row
    with a real (non-missing) value outranks every `value_col`-missing
    row, so a higher-priority source's `NA` never discards a
    lower-priority source's real observation; a cell wins `NA` only when
    no source reports a real value. Among rows with a real value the
    winner is the row of lowest effective rank; ties are broken by
    broader within-series coverage (the count of cells the source
    reports across the `.by` group) when `tie_break$coverage`, then by
    `tie_break$quality_col` ordered per `tie_break$quality_levels`, then
    by ascending source name (reported when `verbose`). Coverage counts
    the cells where `value_col` is non-missing, or only the strictly
    positive ones under `tie_break$coverage = "positive"`, for panels
    where an exact zero reads as "not reported" as often as "measured
    zero" and would otherwise inflate the coverage of a mostly-zero
    series.

4.  **Continuity override.** When enabled, an isolated single-period
    winner flip is reverted: if the immediately preceding and following
    periods share a different winner that also reports the middle
    period, that continuous source reclaims the middle cell, removing
    single-period teeth from otherwise smooth series. The reversion is
    skipped when the flanking source's middle-period value is itself
    missing (continuity never reinstates an `NA`) and when it would hand
    a cell won by a measure-consistent source back to a measure-demoted
    one: continuity never undoes the measure penalty, because a
    single-period source switch is cosmetic while a measure switch
    corrupts the series.

This operationalises the AFE decision *Consolidate multi-source panels
measure-consistently*
(`wiki/decisions/measure-consistent-panel-consolidation`): measure
identity is part of the dedup key's semantics, and priority alone cannot
arbitrate cells whose sources report different measures.

The input must hold at most one row per source per cell; pre-aggregate
any sub-detail rows first (by default the function aborts on duplicates
rather than sum silently). Set `tie_break$quality_variants` when a
source legitimately contributes several `tie_break$quality_col` variants
of one cell (an observed and an interpolated estimate, say): the
variants then collapse to the best-ranked one before any other stage,
and only rows sharing a source, a cell *and* a quality level still
abort.

## Examples

``` r
panel <- tibble::tribble(
  ~year, ~region, ~category, ~source, ~value,
  1900, "WLD", "Coal", "OWID", 10,
  1900, "WLD", "Coal", "Malanima", 20,
  1901, "WLD", "Coal", "Malanima", 21,
  1902, "WLD", "Coal", "Malanima", 22
)

consolidate_sources(
  panel,
  value_col = value,
  source_col = source,
  priority = c(OWID = 1L, Malanima = 4L),
  .by = c("region", "category"),
  verbose = FALSE
)
#> # A tibble: 3 × 9
#>    year region category source   value n_sources source_rank effective_rank
#>   <dbl> <chr>  <chr>    <chr>    <dbl>     <int>       <int>          <int>
#> 1  1900 WLD    Coal     OWID        10         2           1              1
#> 2  1901 WLD    Coal     Malanima    21         1           4              4
#> 3  1902 WLD    Coal     Malanima    22         1           4              4
#> # ℹ 1 more variable: measure_demoted <lgl>
```
