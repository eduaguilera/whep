# Estimate cropland NPP components including weeds.

Assembles full cropland net primary production: scales weed biomass from
potential NPP and the `weed_npp_scaling` factors, then partitions crop
and weed biomass into dry matter, carbon and nitrogen via
[`calculate_npp_carbon_nitrogen()`](https://eduaguilera.github.io/whep/reference/calculate_npp_carbon_nitrogen.md).

## Usage

``` r
calculate_crop_npp_components(
  x,
  .by = NULL,
  potential = list(method = "lpjml")
)
```

## Arguments

- x:

  A tibble with `item_prod_code`, `area_ha`, `year`, `product_dm_t`,
  `residue_dm_t` and `root_dm_t` (e.g. the output of
  [`calculate_crop_npp()`](https://eduaguilera.github.io/whep/reference/calculate_crop_npp.md)).
  When `npp_potential_dm_t_ha` is absent it is computed via
  [`calculate_potential_npp()`](https://eduaguilera.github.io/whep/reference/calculate_potential_npp.md)
  (which, for the default `lpjml` method, needs `lon`/`lat`).

- .by:

  Optional character vector of grouping columns used to fill missing
  weed-scaling factors with the group mean.

- potential:

  A named list selecting the potential-NPP source: `method` (default
  `"lpjml"`) and `lpjml` options.

## Value

The input tibble with weed dry matter, the dry-matter / nitrogen /
carbon partition for crop, weeds and total, and
`weed_scaling_to_be_revised`.

## Details

The `weed_npp_scaling` table is taken from Spain_Hist and is flagged
`to_be_revised`: it is Spain-specific and not validated for WHEP's
global scope. A `weed_scaling_to_be_revised` column records this on the
output and a one-time warning is emitted.

## Examples

``` r
tibble::tibble(
  item_prod_code = "15", production_t = 100, area_ha = 40,
  year = 2000, npp_potential_dm_t_ha = 5
) |>
  calculate_crop_npp() |>
  calculate_crop_npp_components()
#> Warning: Weed scaling uses the Spain-specific weed_npp_scaling table.
#> ℹ It is flagged to_be_revised: not validated for global scope.
#> This warning is displayed once per session.
#> # A tibble: 1 × 35
#>   item_prod_code production_t area_ha  year npp_potential_dm_t_ha product_dm_t
#>   <chr>                 <dbl>   <dbl> <dbl>                 <dbl>        <dbl>
#> 1 15                      100      40  2000                     5         87.9
#> # ℹ 29 more variables: yield_dm_t_ha <dbl>, residue_dm_t <dbl>,
#> #   method_residue <chr>, root_dm_t <dbl>, method_root <chr>,
#> #   crop_npp_dm_t <dbl>, weed_scaling <dbl>, weed_ag_dm_t <dbl>,
#> #   weed_scaling_to_be_revised <lgl>, weed_bg_dm_t <dbl>, weed_npp_dm_t <dbl>,
#> #   weed_ag_n_t <dbl>, weed_bg_n_t <dbl>, weed_npp_n_t <dbl>,
#> #   weed_ag_c_t <dbl>, weed_bg_c_t <dbl>, weed_npp_c_t <dbl>,
#> #   total_npp_dm_t <dbl>, product_n_t <dbl>, residue_n_t <dbl>, …
```
