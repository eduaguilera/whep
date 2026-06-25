# Estimate symbiotic biological nitrogen fixation by weeds and cover crops.

Estimates symbiotic BNF from leguminous weeds and seeded cover crops.
The legume fraction is a weighted average of spontaneous weeds and
seeded cover crops. Environmental modifiers activate only when their
driver columns are present.

## Usage

``` r
calculate_weed_bnf(x, symbiotic_params = list())
```

## Arguments

- x:

  A tibble with `weed_npp_n_t`, `land_use`, `legumes_seeded` and
  `seeded_cover_crop_share`. Optional `legumes_spontaneous` overrides
  the land-use default; the same environmental driver columns as
  [`calculate_crop_bnf()`](https://eduaguilera.github.io/whep/reference/calculate_crop_bnf.md)
  are supported.

- symbiotic_params:

  Named list overriding the symbiotic-BNF parameters (see
  [`calculate_crop_bnf()`](https://eduaguilera.github.io/whep/reference/calculate_crop_bnf.md)).

## Value

The input tibble with `weed_ndfa_ref`, `weed_ndfa`, `weed_leg_share`,
`f_env_weed` and `weed_bnf_t`.

## Examples

``` r
calculate_weed_bnf(
  tibble::tibble(
    weed_npp_n_t = 10, land_use = "Cropland",
    legumes_seeded = 0, seeded_cover_crop_share = 0
  )
)
#> # A tibble: 1 × 18
#>   weed_npp_n_t land_use legumes_seeded seeded_cover_crop_share n_synth_kg_ha
#>          <dbl> <chr>             <dbl>                   <dbl>         <dbl>
#> 1           10 Cropland              0                       0             0
#> # ℹ 13 more variables: n_org_kg_ha <dbl>, temp_c <dbl>, pet_mm <dbl>,
#> #   som_pct <dbl>, soil_ph <dbl>, clay_pct <dbl>, water_input_mm <dbl>,
#> #   legumes_spontaneous <dbl>, f_env_weed <dbl>, weed_ndfa_ref <dbl>,
#> #   weed_ndfa <dbl>, weed_leg_share <dbl>, weed_bnf_t <dbl>
```
