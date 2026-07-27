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
`method_urban`.

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

## Examples

``` r
build_urban_n(example = TRUE)
#> # A tibble: 1 × 6
#>     lon   lat area_code  year urban_n_t method_urban                 
#>   <dbl> <dbl> <chr>     <int>     <dbl> <chr>                        
#> 1 -0.25 -0.25 ESP        2020       4.5 spain_hist_rate|room_weighted
```
