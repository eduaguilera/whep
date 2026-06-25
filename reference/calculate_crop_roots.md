# Estimate crop below-ground (root) biomass.

Estimates root dry matter from above-ground biomass using an ensemble of
an IPCC root:shoot ratio and a reference below-ground biomass per
hectare. The nitrogen-input and irrigation adjustments activate only
when their driver columns are present in `x`.

## Usage

``` r
calculate_crop_roots(
  x,
  method = c("ensemble", "root_shoot", "reference"),
  weights = list(w_ref = 0.5)
)
```

## Arguments

- x:

  A tibble with `item_prod_code`, `product_dm_t`, `residue_dm_t` and
  `area_ha`. Optional `n_input_kg_ha` enables the nitrogen adjustment;
  optional `water_regime` enables the irrigation adjustment.

- method:

  Root method: `"ensemble"` (default, weighted root:shoot + reference),
  `"root_shoot"` (root:shoot ratio only) or `"reference"` (reference
  below-ground biomass only).

- weights:

  Named list; `w_ref` is the reference weight in the ensemble (0-1,
  default 0.5), used only when `method = "ensemble"`.

## Value

The input tibble with `root_dm_t` and `method_root`.

## Examples

``` r
calculate_crop_roots(
  tibble::tibble(
    item_prod_code = "15",
    product_dm_t = 87.9,
    residue_dm_t = 135.75,
    area_ha = 40
  )
)
#> # A tibble: 1 × 6
#>   item_prod_code product_dm_t residue_dm_t area_ha root_dm_t method_root
#>   <chr>                 <dbl>        <dbl>   <dbl>     <dbl> <chr>      
#> 1 15                     87.9         136.      40      66.3 ensemble   
```
