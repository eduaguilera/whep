# Attribute reported fallow land to crops.

Distribute each country's FAOSTAT-reported fallow area among its crops
using a precomputed allocation weight, adding the result to each crop's
cropped physical area. The weight is typically
[`gridded_fallow_weights()`](https://eduaguilera.github.io/whep/reference/gridded_fallow_weights.md),
which puts fallow on rainfed dryland cereals and rainfed monsoon rice
and keeps it off irrigated/continuous systems.

The fallow *magnitude* comes from `fallow_total` (FAOSTAT "Land with
temporary fallow", item 6640) — reported separately from temporary
meadows/pastures, so it isolates real fallow from fodder.

## Usage

``` r
attribute_fallow_to_crops(cropgrids, fallow_total, alloc_weight)
```

## Arguments

- cropgrids:

  Tibble of national crop areas with columns `area_code`,
  `item_cbs_code`, `physical_ha` (cropped physical area),
  `harvested_ha`.

- fallow_total:

  Tibble of reported fallow area with columns `area_code` and
  `fallow_ha`.

- alloc_weight:

  Tibble of `area_code`, `item_cbs_code`, `weight` giving the
  within-country allocation weight, e.g. from
  [`gridded_fallow_weights()`](https://eduaguilera.github.io/whep/reference/gridded_fallow_weights.md).

## Value

A tibble with `area_code`, `item_cbs_code`, `physical_ha` (cropped
physical area plus attributed fallow), and `harvested_ha`.

## Examples

``` r
cropgrids <- tibble::tribble(
  ~area_code, ~item_cbs_code, ~physical_ha, ~harvested_ha,
  1L, 2511L, 500, 500,
  1L, 2807L, 400, 400
)
fallow_total <- tibble::tribble(~area_code, ~fallow_ha, 1L, 200)
# all weight on wheat -> the 200 ha reported fallow goes to wheat
alloc_weight <- tibble::tribble(
  ~area_code, ~item_cbs_code, ~weight,
  1L, 2511L, 1,
  1L, 2807L, 0
)
attribute_fallow_to_crops(cropgrids, fallow_total, alloc_weight)
#> # A tibble: 2 × 4
#>   area_code item_cbs_code physical_ha harvested_ha
#>       <int>         <int>       <dbl>        <dbl>
#> 1         1          2511         700          500
#> 2         1          2807         400          400
```
