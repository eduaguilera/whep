# Estimate non-symbiotic biological nitrogen fixation.

Estimates free-living and associative BNF in agricultural soils from a
base rate (crop-specific from `bnf`, or a default) scaled by
environmental modifiers for nitrogen, temperature, water, soil organic
matter, pH and clay. Each modifier activates only when its driver column
is present.

## Usage

``` r
calculate_nonsymbiotic_bnf(
  x,
  nonsymbiotic_params = list(),
  soil_params = list()
)
```

## Arguments

- x:

  A tibble with `area_ha`. Optional `nonsymbiotic_base_kg_ha` (or
  `item_prod_code` to join the crop-specific base rate), plus the
  nitrogen, temperature, water and soil (`som_pct`, `soil_ph`,
  `clay_pct`) drivers.

- nonsymbiotic_params:

  Named list overriding `nsbnf_default_kg_ha`, `k_n_synth`, `k_n_org`,
  `t_opt`, `t_sigma`, `ai_threshold`.

- soil_params:

  Named list overriding `k_som`, `som_ref`, `ph_opt`, `ph_sigma`,
  `k_clay`, `clay_ref`.

## Value

The input tibble with `nonsymbiotic_base_kg_ha`, the six
`f_*_nonsymbiotic` modifiers, `f_env_nonsymbiotic` and
`nonsymbiotic_bnf_t`.

## Examples

``` r
calculate_nonsymbiotic_bnf(tibble::tibble(area_ha = 40))
#> # A tibble: 1 × 18
#>   area_ha n_synth_kg_ha n_org_kg_ha temp_c pet_mm som_pct soil_ph clay_pct
#>     <dbl>         <dbl>       <dbl>  <dbl>  <dbl>   <dbl>   <dbl>    <dbl>
#> 1      40             0           0     NA     NA      NA      NA       NA
#> # ℹ 10 more variables: water_input_mm <dbl>, nonsymbiotic_base_kg_ha <dbl>,
#> #   f_nitrogen_nonsymbiotic <dbl>, f_temperature_nonsymbiotic <dbl>,
#> #   f_water_nonsymbiotic <dbl>, f_som_nonsymbiotic <dbl>,
#> #   f_ph_nonsymbiotic <dbl>, f_clay_nonsymbiotic <dbl>,
#> #   f_env_nonsymbiotic <dbl>, nonsymbiotic_bnf_t <dbl>
```
