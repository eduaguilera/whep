# Build gridded urban/human-excreta nitrogen inputs to agriculture.

Estimates the nitrogen from urban human excreta and municipal waste
applied to agricultural land, per WHEP 0.5-degree grid cell. Each cell's
urban population (from
[`read_hyde_population()`](https://eduaguilera.github.io/whep/reference/read_hyde_population.md))
is converted to a nitrogen load via a per-capita rate interpolated from
Spain's own historical benchmark series (`urban_n_reference` /
`urban_kgn_cap_reference`; see Details), then spilled from cells with no
local cropland room to same-polity neighbouring cells with spare
capacity via
[`allocate_manure_transport()`](https://eduaguilera.github.io/whep/reference/allocate_manure_transport.md),
the same buffering used by the manure engine.

## Usage

``` r
build_urban_n(
  years = NULL,
  polity_validity = c("keep", "flag", "drop"),
  data = list(),
  example = FALSE
)
```

## Arguments

- years:

  Optional integer vector of calendar years to keep. `NULL` keeps every
  year `data$urban_population` covers.

- polity_validity:

  What to do with a row whose `(area_code, year)` resolves to a polity
  that did not exist in that year (the cell-polity crosswalk has no year
  dimension, so an early-20th-century cell is labelled with its
  present-day territory). `"keep"` (default) keeps every row, which is
  the historical behaviour, and warns naming the rows, years and area
  codes involved. `"flag"` keeps them and adds the per-row logical
  `reporting_polity_out_of_span`, marking exactly which rows are
  stand-ins. `"drop"` removes them. All three warn; only `"drop"`
  changes the numbers. See
  [`polity_coverage_gaps()`](https://eduaguilera.github.io/whep/reference/polity_coverage_gaps.md),
  which reports the same rows for an already-built table.

- data:

  Optional named list of pre-loaded inputs: `urban_population` (`lon`,
  `lat`, `year`, `urban_pop`, falling back to
  [`read_hyde_population()`](https://eduaguilera.github.io/whep/reference/read_hyde_population.md)
  when absent), `cell_polity` (`lon`, `lat`, `area_code`, plus optional
  `polity_frac`; a missing `polity_frac` is treated as 1 for backwards
  compatibility) and `cropland_ha` (`lon`, `lat`, `area_code`, `year`,
  `cropland_ha`, required: the gridded cropland area used as the simple
  room proxy, `cropland_ha * 0.170` t N/ha, the same EU-Nitrates fixed
  ceiling used by
  [`allocate_manure_to_land()`](https://eduaguilera.github.io/whep/reference/allocate_manure_to_land.md)'s
  `fixed_ceiling_kg_ha` default).

- example:

  If `TRUE`, return a small fixture instead of reading data. Defaults to
  `FALSE`.

## Value

A tibble with `lon`, `lat`, `area_code`, `year`, `urban_n_t` and
`method_urban`, plus the polity columns below, plus
`reporting_polity_out_of_span` when `polity_validity = "flag"`.

## Details

The current per-capita rate is a documented placeholder (Spain's own
historical urban-N series applied as a global default). For a future
refinement, urban N should instead be derived from two distinct, more
mechanistic streams: (1) sewage/human-excreta N estimated from actual
historical per-capita dietary protein/N intake (already reconstructable
in WHEP via its FAOSTAT/commodity-balance food-supply data, rather than
a fixed external per-capita constant), and (2)
food-waste/municipal-solid- waste N from actual historical food-loss and
waste estimates. This is out of scope for the current task and is not
implemented here.

## Polity columns

Every area-keyed output carries the polity its `area_code` resolves to
in that row's year:

- `polity_area_code`: The numeric key rows are AGGREGATED on, for the
  matrix workflows. It is a bucket, not an identity: use
  `reporting_polity_code` to say which territory a row belongs to.

- `reporting_polity_code`: The polity itself, e.g. `ESP-1846-1914`. It
  is year-aware, so the same `area_code` resolves to different polities
  in different years, which is the point of the crosswalk.

- `reporting_polity_name`: Its name. It can differ from the area's own
  name where the area folds into an aggregate.

- `reporting_polity_has_geometry`: Whether the polity has a polygon in
  the WHEP polity database, for callers that need to map or intersect
  it. `FALSE` is a documented gap upstream, not an error.

Rows whose `area_code` resolves to no polity keep the columns with `NA`
rather than being dropped, so a gap is visible instead of silent.

Rows before the back-cast anchor year resolve to the polity live in that
anchor year rather than to the polity live in the row's own year,
because WHEP's pre-anchor series are back-cast onto the anchor-year
territory. See
[`add_polity_code()`](https://eduaguilera.github.io/whep/reference/add_polity_code.md)
for the reasoning. Where that polity is not live in the row's own year –
41.5% of the pre-1961 `(area, year)` cells –
[`add_polity_code()`](https://eduaguilera.github.io/whep/reference/add_polity_code.md)
says so as `mapping_status == "backcast_anchor"`, and
[`polity_coverage_gaps()`](https://eduaguilera.github.io/whep/reference/polity_coverage_gaps.md)
reports it as `gap_kind == "backcast_anchor"`. These columns do not say
so either way.

A row whose year no mapped period covers is resolved to the NEAREST
period of the same area instead, so `reporting_polity_code` can name a
polity that did not exist in that row's year – FAOSTAT bucket 206 "Sudan
(former)" keeps reporting after `SUD-1956-2011` ends, and its post-2011
rows carry that code. These columns do not say so:
[`add_polity_code()`](https://eduaguilera.github.io/whep/reference/add_polity_code.md)
reports such a row as `mapping_status == "out_of_span"`, and that column
is dropped here so that adding it does not change the schema of every
area-keyed output at once.
[`polity_coverage_gaps()`](https://eduaguilera.github.io/whep/reference/polity_coverage_gaps.md)
reports the stand-in rows of a built table, and
`options(whep.polity_mapping_status = "flag")` (or `"status"`) carries
the signal on the outputs themselves. Both are opt-in; the default is no
extra column.

## Examples

``` r
build_urban_n(example = TRUE)
#> # A tibble: 1 × 10
#>    year area_code polity_area_code reporting_polity_code reporting_polity_name
#>   <int>     <int>            <int> <chr>                 <chr>                
#> 1  2020       203              203 ESP-1800-2025         Spain                
#> # ℹ 5 more variables: reporting_polity_has_geometry <lgl>, lon <dbl>,
#> #   lat <dbl>, urban_n_t <dbl>, method_urban <chr>
```
