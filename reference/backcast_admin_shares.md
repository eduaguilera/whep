# Back-cast admin shares across the seam

Complete an admin-shares table to the full year range of its extent and
fill the pre-seam years by modulating each unit's seam-year share with
its own land extent, the plan's decision-7 back-cast:

\$\$\tilde{s}\_u(t) = s_u(t_0) \frac{E_u(t)}{E_u(t_0)}, \qquad s_u(t) =
\frac{\tilde{s}\_u(t)}{\sum_v \tilde{s}\_v(t)}\$\$

The first step is
[`fill_proxy_growth()`](https://eduaguilera.github.io/whep/reference/fill_proxy_growth.md)
with the extent as proxy, the same machinery `.fill_pre_faostat()`
(`R/build_production.R:3008`) uses to back-cast the national totals
these shares split, so the two compose. The second is a per-year
renormalisation over the `t0` unit set, the residual pseudo-unit
included when the shares carry one.

Nothing here is a fallback: a year the rules cannot resolve is refused
with a diagnostic and keeps `share = NA`, never a carried value.

Two things the plan's seam section names are deliberately **not** here.
Pre-seam irrigation needs no separate treatment: the unit irrigated
target is built from the engine's own annual `ir_potential`
(`R/spatialize.R:420`), whose LUH2 `irrigated_ha` varies every year,
pre-seam years included, so it moves with the same extent that modulates
the area share. And the within-unit weight *regime* per
`(unit, item, year)` is the allocation's to record, not this function's:
`treatment` here says how a share was obtained, while the regime says
which weight then spread it inside the unit.

## Usage

``` r
backcast_admin_shares(
  shares,
  extent,
  seam = NULL,
  binding_indicator = "area_harvested",
  settings = list()
)
```

## Arguments

- shares:

  An admin-shares table (see
  [`admin_shares_schema()`](https://eduaguilera.github.io/whep/reference/admin_shares_schema.md)).
  Minimum columns: `area_code`, `level`, `item_prod_code`,
  `indicator_used`, `level_polity_code`, `year`, `share`,
  `treatment_year`. Every other contract column present is carried onto
  the produced rows from their anchor row.

- extent:

  A per-unit extent table from
  [`aggregate_unit_extent()`](https://eduaguilera.github.io/whep/reference/aggregate_unit_extent.md).
  Its year range is the range the shares are completed to.

- seam:

  Optional tibble overriding `t0` per series: `area_code`,
  `item_prod_code`, `t0`, optionally `level`. An override is the
  temporal hold-out lever: every year below the given `t0` becomes a
  back-cast year even where an observation exists, and the superseded
  observations are counted (`seam_supersedes_observed`).

- binding_indicator:

  The indicator that binds, `"area_harvested"` by the plan's decision 8.
  `t0` is the series' first observed year carrying it; a series with
  none is refused (`no_binding_anchor`).

- settings:

  Named list of tuning knobs, each defaulting as below. An unknown key
  aborts, and what the call ran with comes back in the returned
  `settings` tibble:

  - `max_gap` (`Inf`): passed to
    [`fill_proxy_growth()`](https://eduaguilera.github.io/whep/reference/fill_proxy_growth.md),
    its own default.

  - `max_gap_linear` (`0`): passed to
    [`fill_proxy_growth()`](https://eduaguilera.github.io/whep/reference/fill_proxy_growth.md);
    a placeholder pending T31(e), see the interior-gaps section.

  - `zero_policy` (`"hold_outside"`): how case (a) is handled. Only
    `"hold_outside"` is signed off (decision 9); the key exists so a
    later decision can add a value without changing call sites.

  - `repair` (`FALSE`): on a flagged extent jump, `TRUE` applies the
    unit-keyed analogue of `.fix_luh2_crop_collapse()` and refuses only
    what stays flagged; `FALSE`, pending T31, refuses the affected
    series outright. Either way the choice is in `settings` and the
    outcome in `diagnostics`.

  - `ratio_bounds` (`c(0.55, 1.6)`): plausible band for
    [`check_extent_jumps()`](https://eduaguilera.github.io/whep/reference/check_extent_jumps.md).

  - `collapse_ratio` (`0.02`): fraction of the adjacent-year mean below
    which a year counts as an isolated collapse, as in
    `.fix_luh2_crop_collapse()` (`R/build_production.R:629-630`).

  - `min_neighbour_ha` (`100`): both neighbours must exceed this for a
    collapse to be repaired – that function's `min_neighbor_mha = 0.001`
    Mha expressed in hectares.

  - `tolerance` (`1e-9`): absolute tolerance for the share and extent
    arithmetic.

## Value

A list of four tibbles:

- `shares`: the input rows plus the completed year set for the `t0` unit
  sets, with two added columns. `treatment` is `"observed"` for a row
  that arrived in `shares`, `"backcast_t0_geometry"` for a row this
  function produced, `"luh2_clamped"` where `t0` lies beyond the
  extent's last year so `E_u(t0)` is the clamped last slice, and `NA`
  for a completed row nothing filled, whose reason is in `diagnostics`.
  `gap_rule` records the interior-gap setting. A produced row carries
  its anchor's provenance columns, `value = NA` (no reported value
  exists) and `treatment_year = NA` (reserved for the interior-gap
  rule), so produced rows deliberately sit outside the closed
  [`admin_shares_schema()`](https://eduaguilera.github.io/whep/reference/admin_shares_schema.md);
  the `"observed"` subset on the contract columns still conforms.

- `diagnostics`: one row per event, with the series and unit keys,
  `year` where the event is year-specific, `diagnostic` from a closed
  vocabulary and free-text `detail`.

- `counters`: every vocabulary member with its count, zeros included.

- `settings`: the one-row record of what this call was run with.

## Territory basis

Shares and `E_u` are defined on the `t0` unit set with `t0` geometry
held fixed, the
[`read_luh2_landuse()`](https://eduaguilera.github.io/whep/reference/read_luh2_landuse.md)
snapshot convention and the analogue of
`mapping_status == "backcast_anchor"`. Every row produced that way
carries `treatment = "backcast_t0_geometry"`, so a unit whose
containment edge starts after a back-cast year still gets a row at that
year, on its `t0` cells.

Translating those rows onto the partition valid at `t` through the
containment-edge lineage – an earlier unit that later split taking the
sum of its `t0`-descendants' modulated shares – happens before the rows
enter the engine and is **not implemented here**. It is a follow-up,
because the relation it needs is succession (which unit became which),
while `polity_containment` carries containment (`member_code`,
`container_code`, `start_year`, `end_year`, `basis`) only: nothing in
the delivered data says a unit starting in one year descends from one
that ended the year before. Until it lands, a container whose partition
changed inside the back-cast window is back-cast on its `t0` partition,
which `treatment` states on every row.

## Zero cases

Post-processing applied after the fill, which itself leaves such
positions `NA` (`.fg_fill_backward_vec()` keeps a fill only where the
result is finite and positive). The plan's four cases, with precedence
(a) over (b):

- \(a\) `E_u(t0) == 0` with `s_u(t0) > 0`: hold `s_u(t0)` **outside**
  the renormalisation, siblings renormalised to `1 - s_u(t0)`. Holding
  it inside would anti-modulate the unit. Counter `zero_extent_t0_held`.

- \(b\) `E_u(t) == 0` at a back-cast year of a unit not held by (a):
  share `0` there, its mass going to the siblings through the
  renormalisation. Counter `zero_extent_year_zeroed`.

- \(c\) the renormalisable sum is zero while there is mass to
  distribute: the year is refused for that series and all its units left
  `NA`. Counter `zero_sum_year_refused`.

- \(d\) `E_u(t0) == 0` with no positive `s_u(t0)`: nothing to hold and
  no mass to move, so the unit contributes `0`. Counter
  `zero_extent_t0_no_share`.

Case (b) counts every zero-extent back-cast year, including the ones
inside a year (c) then refuses: both statements are true of such a year,
and (c) is what decides the output.

A zero year also breaks the growth chain:
[`fill_proxy_growth()`](https://eduaguilera.github.io/whep/reference/fill_proxy_growth.md)
telescopes a product of consecutive growth factors, so it cannot reach
the years beyond the break and leaves them `NA` even though the ratio
above is perfectly well defined there. Those positions are completed
from that ratio directly – the same expression, not a second method –
and every one is counted (`direct_ratio_repair`). Where the chain is
intact the two agree to machine precision, which the tests assert.

## Interior gaps

The fill window ends at `t0`, so no gap between two observed years is
ever filled: such rows are completed, left `NA` and counted
(`interior_gap_refused`, `trailing_gap_refused`). That is the
placeholder default until T31(e) decides the rule; it is recorded on
every row in `gap_rule` and in `settings`. `max_gap` and
`max_gap_linear` are passed straight to
[`fill_proxy_growth()`](https://eduaguilera.github.io/whep/reference/fill_proxy_growth.md)
so that decision needs no API change. Today `max_gap` bounds the
back-cast run itself – a longer run is left unfilled and counted
(`max_gap_exceeded`), because
[`fill_proxy_growth()`](https://eduaguilera.github.io/whep/reference/fill_proxy_growth.md)'s
gap test governs the leading run as well as the interior ones – while
`max_gap_linear` cannot bite, there being no interior run inside the
window.

## Examples

``` r
shares <- tibble::tibble(
  area_code = 900L,
  level = 1L,
  item_prod_code = 15L,
  indicator_used = "area_harvested",
  level_polity_code = c("A1", "A2"),
  year = 1902L,
  share = c(0.6, 0.4),
  treatment_year = "observed"
)
extent <- tibble::tibble(
  area_code = 900L,
  level_polity_code = rep(c("A1", "A2"), each = 3),
  level = 1L,
  year = rep(1900:1902, times = 2),
  extent_ha = c(800, 900, 1000, 600, 550, 500)
)
out <- backcast_admin_shares(shares, extent)
out$shares[, c("level_polity_code", "year", "share", "treatment")]
#> # A tibble: 6 × 4
#>   level_polity_code  year share treatment           
#>   <chr>             <int> <dbl> <chr>               
#> 1 A1                 1900 0.5   backcast_t0_geometry
#> 2 A1                 1901 0.551 backcast_t0_geometry
#> 3 A1                 1902 0.6   observed            
#> 4 A2                 1900 0.5   backcast_t0_geometry
#> 5 A2                 1901 0.449 backcast_t0_geometry
#> 6 A2                 1902 0.4   observed            
```
