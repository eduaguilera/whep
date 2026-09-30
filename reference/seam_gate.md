# Gate an admin-shares back-cast at its seams

Judge a back-cast share table at every seam the resolver found, in the
three tiers the subnational-spatialization plan's seam section asks for.
Nothing is aborted and nothing is repaired: each tier returns its own
table with a `pass` column and the numbers behind it, and the caller
decides what a failure means. That is deliberate. The gate is evidence
for a release decision, not a guard inside a pipeline.

Seam years are read from `seams`, never assumed. This function contains
no year: a run whose statistics start in 1850 and one whose statistics
start in 1974 are gated by the same code at different years.

## Usage

``` r
seam_gate(
  shares_backcast,
  seams,
  cells = NULL,
  holdout = NULL,
  tolerances = seam_gate_tolerances()
)
```

## Arguments

- shares_backcast:

  A back-cast share table, the `shares` element of
  [`backcast_admin_shares()`](https://eduaguilera.github.io/whep/reference/backcast_admin_shares.md).
  Columns: `area_code`, `level`, `item_prod_code`, `level_polity_code`,
  `year`, `share`, `treatment`, optionally `value`. One row per unit and
  year.

- seams:

  A seam list, the `seams` element of
  [`resolve_admin_shares()`](https://eduaguilera.github.io/whep/reference/resolve_admin_shares.md):
  `area_code`, `level`, `item_prod_code`, `seam_year`, `seam_kind`.
  Every seam must name a series `shares_backcast` carries, or the gate
  aborts (`whep_seam_gate_seams_unmatched`): such a seam is gated by
  nothing and leaves no row in any tier, so it can be reported nowhere
  else. The resolver emits one `"start"` seam per series, so a matched
  pair satisfies this by construction and a breach means the two tables
  come from different runs, or one of them was filtered. A zero-row
  `shares_backcast` – the tier-C-only call – is exempt.

- cells:

  Optional crop-level engine output for tier C: `lon`, `lat`, `year`,
  `area_code`, `crop_name`, and either `harvested_ha` or `rainfed_ha`
  plus `irrigated_ha`. `polycell_id`, `cell_id`, `level_polity_code` and
  `regime` are used when present. `NULL` (default) leaves tier C
  unevaluated.

- holdout:

  What the tier-B hold-out needs, or `NULL` (default), which leaves it
  unevaluated. A list of `extent`, the per-unit extent table from
  [`aggregate_unit_extent()`](https://eduaguilera.github.io/whep/reference/aggregate_unit_extent.md)
  that the back-cast was run with, and optionally `args`, a named list
  of further arguments for
  [`backcast_admin_shares()`](https://eduaguilera.github.io/whep/reference/backcast_admin_shares.md)
  – normally
  `list(settings = <the knobs the original back-cast ran with>)`, so the
  hold-out is run the way the run was. `shares`, `extent` and `seam` are
  set by the leg and are refused in `args`. With `holdout` supplied,
  `shares_backcast` must also carry `indicator_used` and
  `treatment_year`, which the back-cast reads. The re-run's own input
  errors – an `extent` with two rows for a unit-year, say – surface as
  errors from it. That is not the gate failing: a gate outcome is always
  a returned row.

- tolerances:

  The numbers to judge with, from
  [`seam_gate_tolerances()`](https://eduaguilera.github.io/whep/reference/seam_gate_tolerances.md).

## Value

A list:

- `tier_a`: one row per series `shares_backcast` carries, with `t0`,
  `n_units`, `n_observed`, `share_sum`, `max_rel_diff`, `basis`,
  `seam_start_year`, `matches_seam_start`, `pass` and `reason`. `pass`
  is `NA` where the tier judged nothing, and `reason` says which of the
  two ways.

- `tier_b`: one row per gated unit-seam pair, with `basis`, `log_ratio`,
  `q_reference`, `n_reference`, `beyond_quantile` and `status`, plus the
  container gate (`n_gated`, `n_beyond`, `frac_beyond`, `threshold`,
  `gate_status`, `pass`) broadcast onto every row of that container and
  `basis`.

- `tier_b_holdout`: one row per unit and withheld year, with
  `anchor_year`, `horizon`, `share_observed`, `share_backcast`,
  `log_ratio`, `status` and the per-series gate; plus one row, keyed on
  the series alone, for every start-seam series the leg could not run
  (`status` beginning `"not_applicable"`). Zero rows when no series has
  a `"start"` seam.

- `tier_c`: one row per gated `(area_code, seam_year)`, with the three
  pairs' scanned series counts and flag rates, `neighbour_rate`,
  `excess`, `n_regime_mismatch`, `pass` and `reason`. Zero rows when
  `cells` is `NULL`.

- `verdict`: a named logical, `tier_a` / `tier_b` / `tier_b_holdout` /
  `tier_c` / `overall`. A tier is `TRUE` when every gate it evaluated
  passed, `FALSE` when any failed and `NA` when it evaluated none –
  which is what a container of nothing but `"vacuous_by_construction"`
  rows gives, and what an empty seam list gives in every tier at once.
  `overall` is `FALSE` if any tier failed; `NA` if any series of
  `shares_backcast` left tier A, tier B and the hold-out with nothing
  but `NA` for a `pass`, or if no tier evaluated anything; else `TRUE`.
  A pass therefore covers every series the gate was handed. Tier C does
  not count towards that coverage: it is keyed on
  `(container, seam year)` and carries no item, so it cannot stand in
  for a series.

## Tier A – identity at the anchor

At each series' `t0` – the first year carrying an observed row in
`shares_backcast`, which is the anchor the back-cast actually used – the
table must still hold the observation:

- every unit's row at `t0` is `treatment == "observed"`;

- the unit shares at `t0` sum to 1 within `identity_rel`;

- where the anchor rows carry `value`, each share equals that value's
  share of the anchor-year total within `identity_rel`
  (`basis = "value"`; `"share_only"` where any anchor value is missing,
  as it is for a residual pseudo-unit, and then only the checks above
  bite);

- `t0` is the `"start"` seam `seams` names for that series.

What tier A cannot see is the modulation itself: the extent table is not
an argument, so `E_u(t0) / E_u(t0) = 1` is verified by
[`backcast_admin_shares()`](https://eduaguilera.github.io/whep/reference/backcast_admin_shares.md)'s
own tests, not here. The last check is the one that catches a
`shares_backcast` and a `seams` built from different runs – and a
deliberate `seam =` override, which moves `t0` on purpose, so a temporal
hold-out is expected to fail it and says so in `reason`.

Two states are not a pass, and each says so on its own row rather than
in a verdict. Both keep their measured numbers, because what is withheld
is the verdict and not the evidence:

- **no `"start"` seam names the series**, so the last check above did
  not run: `pass` is `NA`, `reason = "no_start_seam"`. Gated here means
  what the check means – a seam year to compare `t0` with – so a series
  named only by, say, a `source_switch` seam is ungated in tier A and
  gated in tier B, where that seam actually lands. An empty or filtered
  seam list would otherwise certify a run by handing tier A nothing to
  check.

- **no observed row at all**, so there is no anchor to measure: `pass`
  is `NA`, `reason = "no_observed_anchor"` and every anchor measurement
  on the row is `NA`. Such a series has a row here rather than none,
  because a series that never reached the tier is the one state a table
  of tier-A rows cannot otherwise show.

Both reasons come after the identity ones, so a series whose identity is
wrong still fails on the identity: a missing seam withholds a verdict,
it does not excuse one.

## Tier B – the governed quantity

The seam log-ratio `|log(s_u(t0) / s_u(t0 - 1))|` of every unit at every
seam year, judged against the empirical distribution of the **observed**
consecutive log-ratios of the same `(container, level, item)`, pooled
over its units. Gated pairs are held out of their own reference
distribution.

The statistic is the fraction of gated pairs beyond that distribution's
`reference_quantile`, aggregated per container across its items, and the
gate is
`null_rate + binomial_sigma * sqrt(null_rate * (1 - null_rate) / n)`:
under the null that a seam pair is an ordinary pair the fraction is
binomial, and this is its upper band. A container whose seams are
invisible against its own year-to-year variation passes; one whose seams
step further than its ordinary years do, does not.

**That reasoning holds only where both sides of the pair are
observations**, which `basis` records per pair and the container gate is
grouped by:

- `basis = "observed_both_sides"`: the seam year's row and the year
  before it are both `treatment == "observed"`. The tier keeps its
  meaning, and its `pass` enters the verdict. This is the
  `source_switch`, `grain_switch`, `nuts_version_switch`,
  `coverage_change` and `indicator_switch` case.

- `basis = "vacuous_by_construction"`: the year before the seam is a row
  the back-cast produced, or is absent. It is read off the two rows'
  `treatment`, not off the seam kind, because that is the operative
  fact. At a `"start"` seam of a table whose anchor matches it – tier
  A's last check – it always holds: `t0` is the series' first observed
  year, so `s_u(t0 - 1)` is `s_u(t0) * E_u(t0 - 1) / E_u(t0)`
  renormalised, one year of smooth extent change. Scored against a
  reference built from the noisier observed year-to-year moves it cannot
  fail: two extent proxies with opposite per-unit trends, whose
  reconstructions differ by a factor of 0.40 to 1.63 ten years down,
  both return `n_beyond = 0`. Such rows report
  `gate_status = "vacuous_by_construction"` and `pass = NA`, and do
  **not** enter the verdict; `frac_beyond` and `threshold` are still
  filled in, because the numbers are worth reading even though they
  decide nothing.

## Tier B hold-out – what stands in at a start seam

Only when `holdout` is supplied. For every series with a `"start"` seam,
the first `holdout_k` observed years are withheld, the series is
back-cast again from the next observed year (the `seam =` override of
[`backcast_admin_shares()`](https://eduaguilera.github.io/whep/reference/backcast_admin_shares.md),
which is the same lever the verification protocol's temporal hold-out
uses), and the reconstruction is scored against what was actually
observed in those years: `|log(s_hat_u(t) / s_u(t))|` per unit and
withheld year, against the same reference quantile of that series'
observed consecutive log-ratios. The two are commensurable under iid
year-to-year noise: a reconstruction error is a difference of two years'
noise (the anchor's and the withheld year's), like a one-year move, and
neither widens with the horizon. On this package's fixture the error
distribution comes out slightly narrower than the reference, so the
realised null exceedance rate sits under `null_rate` rather than over
it; under persistent noise a deep hold-out over-rejects instead. Both
departures are the safe direction for a gate, and neither is assumed:
the measured fractions are in the tests.

The reference excludes every pair at or below the hold-out anchor as
well as every seam year, for the reason tier B holds its gated pairs out
of their own reference: a series whose earliest years are unusual would
otherwise both widen the yardstick and be measured by it. The
consequence is worth stating plainly – where the earliest years
genuinely move differently from the rest of the record, this leg fails a
proxy that is right about the rest. It fails loudly rather than passing
quietly.

The band divides by the number of **units** with a scored pair, not by
the number of pairs. A unit's `holdout_k` errors all carry the anchor
year's own noise, so they are one cluster and not `holdout_k`
independent draws; dividing by pairs made this leg flag a correct proxy
on the package's own fixture. `min_gated` therefore reads as a minimum
unit count here (`gate_status = "too_few_units"`).

A series with `holdout_k` or fewer observed years is reported
`not_applicable`, never passed, and so is every series when `holdout` is
not supplied: a start seam then has no evidence at all behind it, which
the returned table says on its own row.

What a pass certifies is a `holdout_k`-year extrapolation. Where the
published back-cast runs deeper – `n_backcast_years` on every row says
how much deeper – it is evidence about the first `holdout_k` years of it
and no more.

Every seam kind in `seams` is gated, coverage changes and indicator
switches included: the seam list is taken as given rather than filtered,
so a kind added upstream is gated without a change here. A pair that
cannot be judged is reported with its reason in `status` (`"na_share"`,
`"zero_share"`, `"no_previous_row"`, `"no_reference"`) and left out of
the statistic rather than counted as a pass.

A unit that *stops* reporting at a seam year has no `s_u(t0)` and so no
pair at all: it is not a row of `tier_b`. Its mass does not vanish from
the gate, because it moves into its siblings' shares, whose ratios at
that year are gated.

## Tier C – cell smoke

Only when `cells` is supplied.
[`check_series_jumps()`](https://eduaguilera.github.io/whep/reference/check_series_jumps.md)
on each cell's share of the national total, across three pairs per gated
seam year: `(t0-2, t0-1)`, `(t0-1, t0)` and `(t0, t0+1)`. Dividing by
the national total is what takes the national series' own splice out of
the comparison; that splice is not this feature's to answer for. The
gate is that the seam pair's flag rate exceeds the mean of its
neighbours' by at most `cell_excess`.

The scan runs one pair at a time, on a table pre-subset to that pair's
two years, so the flags returned are exactly that pair's and the memory
bound is two years of cells rather than the whole run. Cells under
`cell_min_ha` in **both** years are dropped first.

Tier C is keyed on `(container, seam year)`, not on the item: the
crop-level engine output carries `crop_name`, not `item_prod_code`, so
there is nothing to join a per-item seam to. Pass the crop-level output,
never the CFT aggregation, whose rows pool several items.

Where `cells` carries a `regime` column – the within-unit weight regime
the allocation records per (unit, item, year) – it must be identical on
both sides of every gated pair. A regime flip at a seam fails tier C on
its own, whatever the flag rates do, because a cell series that changes
weight regime at the seam is discontinuous by construction rather than
by measurement.

Nothing in the package writes that column today: the allocation's regime
label is `method_crop_alloc`, which lives on the targets table and not
on the cell grid, so on the shipped crop-level output this axis is **not
evaluated**. Such a gate reports `regime_checked = FALSE` and
`n_regime_mismatch = NA` – `NA` rather than zero, because an axis nobody
looked at has no count of mismatches – and the report line says how many
of the gates are in that state, so a pass is not read as evidence that
no regime flipped.

## Examples

``` r
# Two units back-cast to 1898 from an anchor at 1900, with the seam
# list the resolver would have emitted for that series.
shares <- tibble::tibble(
  area_code = 900L,
  level = 1L,
  item_prod_code = 15L,
  level_polity_code = rep(c("A1", "A2"), each = 3),
  year = rep(1898:1900, times = 2),
  share = c(0.36, 0.38, 0.40, 0.64, 0.62, 0.60),
  treatment = rep(
    c("backcast_t0_geometry", "backcast_t0_geometry", "observed"),
    times = 2
  )
)
seams <- tibble::tibble(
  area_code = 900L,
  level = 1L,
  item_prod_code = 15L,
  seam_year = 1900L,
  seam_kind = "start"
)
seam_gate(shares, seams)$tier_a
#> `seam_gate()`: overall TRUE.
#> • Coverage: 1 of 1 series judged by tier A, tier B or the hold-out, which is
#>   what overall requires.
#> • Tier A TRUE: 1 series, 0 failing, 0 with no start seam, 0 with no observed
#>   row.
#> • Tier B NA: 0 of 2 unit-seam pairs scored, 0 of 1 container judged; 2 pairs
#>   vacuous by construction and out of the verdict.
#> • Tier B hold-out NA: 0 of 1 unit-year scored over 1 start-seam series.
#> • Tier C NA: 0 container-seam gates, 0 passing, 0 with the regime axis
#>   unchecked.
#> # A tibble: 1 × 13
#>   area_code level item_prod_code    t0 n_units n_observed share_sum max_rel_diff
#>       <int> <int>          <int> <int>   <int>      <int>     <dbl>        <dbl>
#> 1       900     1             15  1900       2          2         1           NA
#> # ℹ 5 more variables: basis <chr>, seam_start_year <int>,
#> #   matches_seam_start <lgl>, pass <lgl>, reason <chr>
```
