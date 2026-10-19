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
  weights = list(w_ref = 0.5),
  irrigation = c("except_paddy", "all_crops", "none")
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

- irrigation:

  Which irrigated crops take the root:shoot factor of `irrigation_adj`
  (0.85 irrigated), used only when `x` has `water_regime`:
  `"except_paddy"` (default) applies it to every crop but paddy rice,
  which is flooded whether irrigated or not; `"all_crops"` applies it to
  every crop, as afsetools does; `"none"` applies no irrigation factor.
  Water stress raises root:shoot across crops (Eziz et al. 2017,
  doi:10.1002/ece3.3630; Kou et al. 2022, doi:10.3389/fpls.2022.1085409;
  Seidel et al. 2024, doi:10.1016/j.scitotenv.2024.176738), implying an
  irrigated factor of about 0.70 to 0.88 (whep#1536).

## Value

The input tibble with `root_dm_t` and `method_root`, plus
`method_root_irrigation` when `x` has `water_regime`.

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
#> 1 15                     87.9         136.      40      67.4 ensemble   
```
