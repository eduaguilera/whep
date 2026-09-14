# Resolve a national row onto the polity its grid support carries that year

The national tables and the cell support are keyed on **different polity
vintages**. A 1961 harvested-area row is keyed `area_code` 185, whose
reporting polity is `RUS-1991-2014`; a year-aware read of the polycell
support holds `F228-1945-1991` (the USSR) in 1961 and no Russian polity
at all. The row then matches no cell and
[`build_gridded_landuse()`](https://eduaguilera.github.io/whep/reference/build_gridded_landuse.md)'s
missing-reporter guard drops its whole national total.

This is the **national-side lineage step**: for every
`(area_code, year)` it walks the `predecessor` edges of
[polities](https://eduaguilera.github.io/whep/reference/polities.md)
from the row's reporting polity back to the polity the supplied
`support` actually carries in that year, and records which rule
answered. It moves no value and allocates nothing – splitting a
predecessor's cells between its successors is the consumer's job, and
this function only says **which polity's cells** a row belongs on.

Measured on the live `polycell_support` pin (`20260907T111653Z-e654d`)
and the `spatialize-country-areas` pin, over the 1961 crop tables: 27 of
194 reporting areas carry 164.2 Mha of harvested area (17.2% of the
world) that a year-aware support cannot place. Under
`basis = "historical_polity"` the lineage places 21 of them, 156.5 Mha,
leaving a 7.7 Mha (0.81%) residue that is **not** a lineage failure:
0.13 Mha is four Rest-of-region reporting buckets, which are aggregate
polities the support excludes by construction under either vintage, and
7.6 Mha is Viet Nam and Yemen, whose reporting areas resolve to
aggregate polities (`F237-1954-1975`, `F249-1918-1990`) whose members
the support carries separately – splitting those between members needs a
share rule this function does not invent.

## Usage

``` r
resolve_polity_lineage(
  national,
  support,
  basis = c("historical_polity", "constant_territory"),
  polities = whep::polities
)
```

## Arguments

- national:

  A tibble of national rows with at least `area_code` and `year`. Any
  other column is carried through untouched; no row is added, dropped or
  reordered.

- support:

  A tibble of the polity intervals the spatial support actually holds
  cells for, with `polity_code`, `start_year` and `end_year`.
  `start_year` is inclusive and `end_year` exclusive, the
  [polities](https://eduaguilera.github.io/whep/reference/polities.md)
  convention.
  [`read_polycell_support()`](https://eduaguilera.github.io/whep/reference/read_polycell_support.md)
  returns a superset of these columns.

- basis:

  Which polity vintage binds a row, `"historical_polity"` (default) or
  `"constant_territory"`. See *Which vintage binds is a choice, not a
  fact*.

- polities:

  The polity table carrying the `predecessor` edges the walk follows;
  defaults to
  [polities](https://eduaguilera.github.io/whep/reference/polities.md).
  Supplied as an argument so a caller can pin a vintage rather than
  inherit whichever one the package ships.

## Value

`national` with three columns added: `lineage_polity_code`, the polity
whose cells the row belongs on, `lineage_polity_name`, and
`method_polity_lineage`, one of `"anchor"` (the reporting polity is
itself carried at that year), `"predecessor"` (a predecessor is),
`"sibling_interval"` (a different interval of the polity the walk landed
on is – the support and `polities` disagree about the interval),
`"constant_territory"`, or `"unresolved"`. An unresolved row keeps `NA`
rather than being dropped, so the gap stays visible, and is warned about
with condition class `whep_lineage_unresolved`.

## Which vintage binds is a choice, not a fact

Two bases are defensible and they differ numerically, so `basis` selects
between them and the answer is stamped on every row in
`method_polity_lineage`. They are alternatives, never a silent fallback.

- `"historical_polity"`:

  The default and the more rigorous. A row is understood as a share of
  the entity that reported in that year – the FAOSTAT back-series is a
  modern-territory reconstruction of it – and is placed on that entity's
  cells. Conserves every national total against a year-aware support;
  leaves the successor's share of the predecessor's cells for the
  consumer to decide.

- `"constant_territory"`:

  The row keeps its reporting polity, i.e. modern borders wherever a
  territory changed. Simpler, and what a present-day snapshot support
  does implicitly, but it keeps the attribution error at every
  succession: 164.2 Mha of 1961 harvested area has no cell to land on.

## The support is an input, and its supply is asserted

`support` is required and is checked for rows covering every year in
`national`, because a lineage resolved against
[polities](https://eduaguilera.github.io/whep/reference/polities.md)
alone can be confidently wrong. `polities` carries two overlapping
Yugoslav intervals, `F248-1920-1991` and `F248-1947-1991`, and the
published support emitted only the second: a walk that consulted the
edges alone would answer `F248-1920-1991`, satisfy every totals check,
and still find no cell. An edgeless `polities` is refused for the same
reason – it would return every anchor unchanged and reconcile perfectly.

## Examples

``` r
national <- tibble::tribble(
  ~area_code, ~year, ~harvested_area_ha,
         185, 1961L,           85049155,
         185, 2015L,           45000000
)
support <- tibble::tribble(
  ~polity_code, ~start_year, ~end_year,
  "F228-1945-1991",      1945L,     1991L,
  "RUS-2014-2025",       2014L,     2025L
)
resolve_polity_lineage(national, support)
#> # A tibble: 2 × 7
#>   area_code  year harvested_area_ha lineage_polity_code method_polity_lineage
#>       <dbl> <int>             <dbl> <chr>               <chr>                
#> 1       185  1961          85049155 F228-1945-1991      predecessor          
#> 2       185  2015          45000000 RUS-2014-2025       anchor               
#> # ℹ 2 more variables: lineage_polity_name <chr>, geom <MULTIPOLYGON [°]>
```
