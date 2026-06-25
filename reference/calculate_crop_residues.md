# Estimate crop above-ground residue biomass.

Estimates crop residue dry matter from production and area using an
ensemble of an IPCC 2019 linear model and a `bio_coefs` residue:product
ratio. The irrigation and modern-variety adjustments activate only when
their driver columns are present in `x`.

## Usage

``` r
calculate_crop_residues(
  x,
  method = c("ensemble", "ipcc", "ratio"),
  weights = list(w_ipcc = 0.5)
)
```

## Arguments

- x:

  A tibble with `item_prod_code`, `production_t` (fresh matter) and
  `area_ha`. Optional `water_regime` enables the irrigation adjustment;
  optional `year` plus `region_hanpp` enable the modern-variety
  adjustment.

- method:

  Residue method: `"ensemble"` (default, weighted IPCC + ratio),
  `"ipcc"` (IPCC linear only) or `"ratio"` (`bio_coefs` ratio only).

- weights:

  Named list; `w_ipcc` is the IPCC weight in the ensemble (0-1, default
  0.5), used only when `method = "ensemble"`.

## Value

The input tibble with `product_dm_t`, `yield_dm_t_ha`, `residue_dm_t`
and `method_residue`.

## Examples

``` r
calculate_crop_residues(
  tibble::tibble(item_prod_code = "15", production_t = 100, area_ha = 40)
)
#> # A tibble: 1 × 7
#>   item_prod_code production_t area_ha product_dm_t yield_dm_t_ha residue_dm_t
#>   <chr>                 <dbl>   <dbl>        <dbl>         <dbl>        <dbl>
#> 1 15                      100      40         87.9          2.20         136.
#> # ℹ 1 more variable: method_residue <chr>
```
