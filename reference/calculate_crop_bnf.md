# Estimate symbiotic biological nitrogen fixation by crop legumes.

Estimates symbiotic BNF from leguminous crops via two methods (an
NPP-based estimate and the Anglade product-based estimate).
Environmental modifiers (nitrogen inhibition, temperature, water) are
applied only when their driver columns are present in `x`; an absent
driver leaves that modifier at 1.

## Usage

``` r
calculate_crop_bnf(x, symbiotic_params = list())
```

## Arguments

- x:

  A tibble with `item_prod_code`, `crop_npp_n_t` and `product_n_t`.
  Optional driver columns activate the modifiers: `n_synth_kg_ha` /
  `n_org_kg_ha` (nitrogen inhibition), `temp_c` (temperature),
  `water_input_mm` (or `precip_mm` + `irrig_mm`) and `pet_mm` (water).

- symbiotic_params:

  Named list overriding the symbiotic-BNF parameters `k_n_synth`,
  `k_n_org`, `t_opt`, `t_sigma`, `ai_threshold`.

## Value

The input tibble with the environmental factors, `ndfa_adj`,
`crop_bnf_t` (NPP method), `crop_bnf_anglade_t` (Anglade method) and the
per-product ratios `bnf_product_ratio_npp` /
`bnf_product_ratio_anglade`.

## Examples

``` r
calculate_crop_bnf(
  tibble::tibble(item_prod_code = "176", crop_npp_n_t = 10, product_n_t = 5)
)
#> # A tibble: 1 × 27
#>   item_prod_code crop_npp_n_t product_n_t n_synth_kg_ha n_org_kg_ha temp_c
#>   <chr>                 <dbl>       <dbl>         <dbl>       <dbl>  <dbl>
#> 1 176                      10           5             0           0     NA
#> # ℹ 21 more variables: pet_mm <dbl>, som_pct <dbl>, soil_ph <dbl>,
#> #   clay_pct <dbl>, water_input_mm <dbl>, name_bnf <chr>, ndfa <dbl>,
#> #   n_harvest_index <dbl>, below_ground_n_ratio <dbl>,
#> #   nonsymbiotic_base_kg_ha <int>, leguminous_share <dbl>, n_total_kg_ha <dbl>,
#> #   f_nitrogen_symbiotic <dbl>, f_temperature_symbiotic <dbl>,
#> #   f_water_symbiotic <dbl>, f_env_symbiotic <dbl>, ndfa_adj <dbl>,
#> #   crop_bnf_t <dbl>, crop_bnf_anglade_t <dbl>, bnf_product_ratio_npp <dbl>, …
```
