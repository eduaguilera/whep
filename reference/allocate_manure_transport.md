# Spill surplus manure to neighbouring cells with spare capacity.

Moves the manure nitrogen that a cell could not place locally (its
surplus above the local cropland and grassland caps) to nearby
same-polity cells that still have room, on the 0.5-degree grid. Each
source offers its surplus to its single-ring (king-move) neighbours in
proportion to their remaining room; a sink that is over-subscribed by
several sources is filled only to its room and the rejected manure
returns to the sources as residual. This is the resolution-robust,
mass-conserving analogue of Spain's room-weighted first-ring
redistribution (`Calc_OA_redistribution`); cross-polity transport is not
allowed. Carbon and volatile solids ride along with each nitrogen flow
at the source cell's bundle ratio.

## Usage

``` r
allocate_manure_transport(source_cells, sink_cells, options = list())
```

## Arguments

- source_cells:

  A tibble of cells exporting manure, keyed by `year`, `territory` and
  `sub_territory` (a `"lon_lat"` cell id), with `surplus_n`, `surplus_c`
  and `surplus_vs` (t).

- sink_cells:

  A tibble of cells with spare capacity, keyed by `year`, `territory`
  and `sub_territory`, with `room_n` (remaining cropland plus grassland
  N capacity, t).

- options:

  A named list. `n_rings` (default 1) is the neighbourhood radius in
  grid steps; only the single-ring kernel is shipped.

## Value

A tibble with one row per landing site: `year`, `territory`,
`sub_territory` (the sink cell for transported manure, the source cell
for residual), `applied_n`, `applied_c`, `applied_vs`, `kind`
(`"transported"` or `"residual"`) and `method_transport`. The
`"residual"` rows are the un-transportable surplus handed back for local
disposal.

## Examples

``` r
source_cells <- tibble::tribble(
  ~year, ~territory, ~sub_territory, ~surplus_n, ~surplus_c, ~surplus_vs,
  2020L, "ESP", "1.5_40", 10, 90, 6
)
sink_cells <- tibble::tribble(
  ~year, ~territory, ~sub_territory, ~room_n,
  2020L, "ESP", "1_40", 4,
  2020L, "ESP", "2_40", 6
)
allocate_manure_transport(source_cells, sink_cells)
#> # A tibble: 2 × 8
#>    year territory sub_territory applied_n applied_c applied_vs kind       
#>   <int> <chr>     <chr>             <dbl>     <dbl>      <dbl> <chr>      
#> 1  2020 ESP       1_40                  4        36        2.4 transported
#> 2  2020 ESP       2_40                  6        54        3.6 transported
#> # ℹ 1 more variable: method_transport <chr>
```
