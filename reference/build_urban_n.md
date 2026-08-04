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
build_urban_n(years = NULL, data = list(), example = FALSE)
```

## Arguments

- years:

  Optional integer vector of calendar years to keep. `NULL` keeps every
  year `data$urban_population` covers.

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
`method_urban`, plus the polity columns below.

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
for the reasoning.

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
