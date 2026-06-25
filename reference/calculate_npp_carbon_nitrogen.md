# Partition crop and weed NPP into dry matter, carbon and nitrogen.

Converts crop NPP components (product, residue, root) and weed biomass
to nitrogen and carbon using the `bio_coefs` per-component coefficients
and the `weed_coefs` scalars. Root and weed below-ground nitrogen
include rhizodeposits. Root carbon uses `root_c_kgdm`, which includes
root biomass carbon plus rhizodeposit carbon per tonne of root dry
matter; use `root_mass_c_kgdm` in `bio_coefs` for root tissue carbon
alone. The residue-to-soil split is computed only when a
`residue_soil_dm_t` column (from
[`calculate_residue_destinies()`](https://eduaguilera.github.io/whep/reference/calculate_residue_destinies.md))
is present.

## Usage

``` r
calculate_npp_carbon_nitrogen(x)
```

## Arguments

- x:

  A tibble with `item_prod_code`, `product_dm_t`, `residue_dm_t` and
  `root_dm_t`. Optional `weed_ag_dm_t` (above-ground weed dry matter;
  treated as 0 when absent), `crop_npp_dm_t` (kept when present) and
  `residue_soil_dm_t` (enables the soil-residue nitrogen and carbon
  split).

## Value

The input tibble with weed dry matter, and nitrogen (`*_n_t`) and carbon
(`*_c_t`) for product, residue, root, weeds, crop NPP and total NPP.

## Examples

``` r
tibble::tibble(item_prod_code = "15", production_t = 100, area_ha = 40) |>
  calculate_crop_npp() |>
  calculate_npp_carbon_nitrogen()
#> # A tibble: 1 × 31
#>   item_prod_code production_t area_ha product_dm_t yield_dm_t_ha residue_dm_t
#>   <chr>                 <dbl>   <dbl>        <dbl>         <dbl>        <dbl>
#> 1 15                      100      40         87.9          2.20         136.
#> # ℹ 25 more variables: method_residue <chr>, root_dm_t <dbl>,
#> #   method_root <chr>, crop_npp_dm_t <dbl>, weed_ag_dm_t <dbl>,
#> #   weed_bg_dm_t <dbl>, weed_npp_dm_t <dbl>, weed_ag_n_t <dbl>,
#> #   weed_bg_n_t <dbl>, weed_npp_n_t <dbl>, weed_ag_c_t <dbl>,
#> #   weed_bg_c_t <dbl>, weed_npp_c_t <dbl>, total_npp_dm_t <dbl>,
#> #   product_n_t <dbl>, residue_n_t <dbl>, root_n_t <dbl>, crop_npp_n_t <dbl>,
#> #   total_npp_n_t <dbl>, product_c_t <dbl>, residue_c_t <dbl>, …
```
