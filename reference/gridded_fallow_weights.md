# Build agro-climatic, rainfed-gated fallow allocation weights.

Compute a per-(area, item) weight for distributing reported fallow to
crops, from gridded crop placement and a crop x agro-climatic-zone
propensity. For each grid cell, rainfed crop area is multiplied by the
crop's propensity in the cell's agro-climatic zone (derived from GAEZ
length of growing period and thermal climate), then summed to country x
item. Dryland cereals/pulses score high in arid/semi-arid zones, rainfed
rice high in the humid tropics (rice-fallow); perennials and the
irrigated share score ~zero.

## Usage

``` r
gridded_fallow_weights(gridded_crops, grid_aez = NULL, propensity = NULL)
```

## Arguments

- gridded_crops:

  Tibble keyed by grid cell and `item_cbs_code` with columns `lon`,
  `lat`, `area_code`, `rainfed_ha`.

- grid_aez:

  Tibble of `lon`, `lat`, `lgp` (length of growing period in days),
  `thermal` (GAEZ thermal-climate class). If `NULL`, the packaged
  `grid_aez.csv` is used.

- propensity:

  Tibble of `item_cbs_code`, `zone`, `fallow_propensity`. If `NULL`, the
  packaged `fallow_propensity.csv` is used.

## Value

A tibble with `area_code`, `item_cbs_code`, `weight`.

## Examples

``` r
gridded_crops <- tibble::tribble(
  ~lon, ~lat, ~area_code, ~item_cbs_code, ~rainfed_ha,
  0.25, 50.25, 1L, 2511L, 500
)
grid_aez <- tibble::tribble(~lon, ~lat, ~lgp, ~thermal, 0.25, 50.25, 100, 7L)
propensity <- tibble::tribble(
  ~item_cbs_code, ~zone, ~fallow_propensity,
  2511L, "semiarid", 0.8
)
gridded_fallow_weights(gridded_crops, grid_aez, propensity)
#> # A tibble: 1 × 3
#>   area_code item_cbs_code weight
#>       <int>         <int>  <dbl>
#> 1         1          2511    400
```
