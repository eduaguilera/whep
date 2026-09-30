# Choose an allocation depth per country and assert no cell is claimed twice

Decision 10's allocation layer: ONE country grid in which each country
is represented at exactly one depth. A container listed in `granted`
arrives as its granted-depth units and as nothing else; every other
country arrives as its level-0 row. The engines see a single grid and
need no depth logic.

## Usage

``` r
build_allocation_layer(grid0, grid_deep, granted)
```

## Arguments

- grid0:

  The level-0 country grid, e.g. `read_level_country_grid(0L)`.

- grid_deep:

  A granted-depth country grid, e.g. `read_level_country_grid(1L)`. May
  also carry level-0 rows for countries that are not granted; they are
  ignored here and taken from `grid0`.

- granted:

  A tibble with `area_code` (integer container code) and `level`
  (integer depth), one row per container – the granted rows of the T25
  coverage report. `NULL` or a zero-row tibble returns `grid0`.

## Value

`grid0`-shaped `tibble` with `level_polity_code` and `level`, carrying
the ragged-coverage diagnostic in its `"ragged_coverage"` attribute and
the unclaimed-land report in its `"unclaimed_land"` attribute (each a
zero-row tibble when nothing was found).

## The two-part assertion

- (a), a one-sided abort:

  The shares of one physical cell may never EXCEED 1, within `1e-8`,
  **at any one year**: the sum is taken over the rows valid together,
  once per epoch the layer carries, and a layer with no validity
  intervals is one epoch and therefore the unconditional sum T37 agreed
  on. Summing ACROSS epochs was the defect: two successive polities each
  hold their cell whole and never coexist, so every succession of the
  year-aware level-0 read – the USSR, Yugoslavia, Sudan, Czechoslovakia
  – summed to 2 and no year-aware grid could be combined with a granted
  depth. A sum above 1 is ground claimed twice – a container kept beside
  its own units – and is invisible downstream because every national
  total still reconciles, so it aborts with class
  `whep_alloc_layer_not_partition`. A sum BELOW 1 is not an error: it is
  land inside the cell that no reporting polity claims, the same
  unclaimed ground the year-aware level-0 read leaves out, and it is
  reported rather than refused (see the unclaimed-land section). A
  compartment holding two different shares of one cell IN ONE EPOCH is a
  contradiction rather than a share that moves, and is refused with
  class `whep_alloc_layer_varying_share`; a share that differs between
  two DISJOINT intervals of one compartment is now read at each of them,
  which is what the year-aware level-0 read produces on real data.

- (b), a diagnostic:

  For every granted country: no container-keyed row survives anywhere,
  and per cell AND EPOCH the unit shares reproduce the country's level-0
  share **in that same epoch**. Both sides are swept together per (cell,
  container), so neither is ever compared against a total taken across
  every epoch of the other. The epochs read are those of the container's
  own depth in the cell – from its first unit row to its last, gaps
  inside it included – because outside that span the layer holds no
  units for it at all and the 2015 snapshot claims every year, which is
  the vintage question open at T31(j) rather than anything the units
  did; land the layer then leaves unclaimed is reported by the other
  attribute. A container with no unit row in a cell at all is reported
  over its level-0 span. Rows that fail are RETURNED, in the
  `"ragged_coverage"` attribute, and are never back-filled with the
  container – back-filling would restore exactly the fold this feature
  removes. (b) failing is not by itself a defect: level 0 is a fixed
  2015 snapshot while granted depths are year-filtered by edge validity,
  and how the two coexist is open at T31(j).

## Unclaimed land

A cell whose shares fall short of 1 holds land that no reporting polity
claims. That is a real state of the grid and not corruption: on the
`20260825T102349Z-1a0eb` polycell support, the level-0 grid at the 2015
snapshot has 37 such cells out of 66,709 – the worst claimed to 0.0116
at (20.75, 42.75) – and the year-aware read leaves whole cells out on
top of that, which is what `validation/spatialize_grid_vintage.R` sizes
per year. Aborting on the shortfall would stop every granted-depth run
on real data; ignoring it would hide land that no national total
accounts for. So it is measured and returned, ONE ROW PER (CELL, EPOCH)
that falls short, in the `"unclaimed_land"` attribute: `lon`, `lat`,
`start_year`, `end_year` (`NA` for a bound the layer never gave),
`claimed_share`, `unclaimed_share`, and `unclaimed_ha`. A caller reads
that attribute to learn how much land is unclaimed, where, AND WHEN.

The epoch is in the report because a cell is not one claim: it is short
in the years it is short. Judging each cell at its fullest epoch instead
– the moment the double-claim check has to read – forgives a cell short
only before one of its holders existed, but by the same line of code it
also hides a cell that goes short in a LATER epoch, which is what a
granted depth ending before its container produces, and it turned an
abort into silence. So every epoch is read.

WHICH YEARS ARE READ is a second question, and reading each cell between
its own first claim and its last answered it wrongly. A depth stops
covering its container either by holding a smaller share or by holding
NO ROW – which is what a real deep grid produces, since a unit has no
rows outside its validity – and in the second case the cell's last claim
IS the depth's end, so the window closed exactly where the silence
began. On the `20260825T102349Z-1a0eb` support, a depth for `area_code`
33 that reproduces its level-0 shares exactly but stops in 1990 left
7,810 of the 8,109 cells it still holds after 1990 in neither report –
826.5 Mha of land, each cell counted once – and 0 ragged rows. It is now
0 cells.

The window is therefore the union of the layer's span in the cell and
the level-0 span of the granted containers holding it: the same rule at
both ends, so a depth that starts late is caught like one that ends
early, and a cell the depth never reaches at all is read over the
level-0 span alone. It widens only to years level 0 itself states – a
row carrying no validity interval (the 2015 snapshot, which has no time
dimension) gives no bound to fall short of, and the window then stops at
the layer's own.

The ragged-coverage report does NOT widen with it, and the asymmetry is
deliberate. This report asks what the LAYER claims, where absence is a
fact whatever `grid0`'s vintage; assertion (b) COMPARES two grids, and
outside a container's depth span it would be comparing year-scoped units
against a level 0 that may claim every year by having no time dimension
at all – the open vintage question at T31(j), not a defect of the units.
So the land a stopped depth leaves is reported here, once, rather than
twice or not at all.

`unclaimed_ha` is `NA` where the layer carries no `land_area_ha` at all
– never 0, which would deny the shortfall. For an epoch no compartment
claims, the shortfall is instead read from `base`: `grid0`'s own row for
the granted container, in that same cell and on its own interval, which
`.level_cell_segments(shares, base)` already receives `base` for and
which carries a measurement rather than an absence. `unclaimed_ha` is
`NA` there only when `base` itself gives no matching land for the epoch
– no `base` was supplied (a direct caller of the internal
`.level_unclaimed_ha()`), or the container's own row carries no
`land_area_ha` either. Every hectare figure this function reports
assumes `land_area_ha` on each row already equals `cell_area_frac` times
the cell's whole land; that is a contract on the CALLER's `grid0` and
`grid_deep`, not something re-derived or checked here, so a layer whose
two disagree reports the column's stated value. The attribute is always
present; it is a zero-row tibble when every cell is fully claimed in
every epoch, and also on the no-grant path, which returns `grid0`
unexamined.

## See also

[`read_level_country_grid()`](https://eduaguilera.github.io/whep/reference/read_level_country_grid.md).

## Examples

``` r
grid0 <- tibble::tribble(
  ~lon,  ~lat, ~area_code, ~cell_area_frac,
  10.25, 40.25,       900L,             0.6,
  10.25, 40.25,       901L,             0.4
)
grid_deep <- tibble::tribble(
  ~lon,  ~lat, ~area_code, ~level_polity_code, ~level, ~cell_area_frac,
  10.25, 40.25,       900L, "A-A1-1900-2100",      1L,             0.25,
  10.25, 40.25,       900L, "A-A2-1900-2100",      1L,             0.35,
  10.25, 40.25,       901L, NA_character_,         0L,             0.4
)
granted <- tibble::tibble(area_code = 900L, level = 1L)
build_allocation_layer(grid0, grid_deep, granted)
#> # A tibble: 3 × 6
#>     lon   lat area_code cell_area_frac level_polity_code level
#>   <dbl> <dbl>     <int>          <dbl> <chr>             <int>
#> 1  10.2  40.2       901           0.4  NA                    0
#> 2  10.2  40.2       900           0.25 A-A1-1900-2100        1
#> 3  10.2  40.2       900           0.35 A-A2-1900-2100        1
```
