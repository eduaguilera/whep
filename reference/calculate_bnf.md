# Estimate total biological nitrogen fixation.

Sums the three BNF components: symbiotic crop legumes, symbiotic
weeds/cover crops, and non-symbiotic free-living fixation, by running
[`calculate_crop_bnf()`](https://eduaguilera.github.io/whep/reference/calculate_crop_bnf.md),
[`calculate_weed_bnf()`](https://eduaguilera.github.io/whep/reference/calculate_weed_bnf.md)
and
[`calculate_nonsymbiotic_bnf()`](https://eduaguilera.github.io/whep/reference/calculate_nonsymbiotic_bnf.md).
When a `climate_type` column is present, the climate-specific parameters
from `bnf_climate_params` override the relevant defaults per climate
type.

The weed component uses the `weed_npp_n_t` already present in `x`. In
the standard crop-NPP chain this is non-zero only when
[`calculate_crop_npp_components()`](https://eduaguilera.github.io/whep/reference/calculate_crop_npp_components.md)
has been run, or when callers supply weed NPP directly;
[`calculate_npp_carbon_nitrogen()`](https://eduaguilera.github.io/whep/reference/calculate_npp_carbon_nitrogen.md)
treats missing weed biomass as zero.

## Usage

``` r
calculate_bnf(
  x,
  symbiotic_params = list(),
  nonsymbiotic_params = list(),
  soil_params = list()
)
```

## Arguments

- x:

  A tibble carrying the required columns of all three component
  functions, optionally with a `climate_type` column.

- symbiotic_params, nonsymbiotic_params, soil_params:

  Named lists passed to the component functions (see those functions).

## Value

The input tibble with all component columns plus `fert_type` (`"BNF"`)
and `bnf_t` (total BNF).

## Examples

``` r
calculate_bnf(
  tibble::tibble(
    item_prod_code = "176", crop_npp_n_t = 10, product_n_t = 5,
    weed_npp_n_t = 4, land_use = "Cropland", legumes_seeded = 0,
    seeded_cover_crop_share = 0, area_ha = 40
  )
)
#> # A tibble: 1 × 48
#>   item_prod_code crop_npp_n_t product_n_t weed_npp_n_t land_use legumes_seeded
#>   <chr>                 <dbl>       <dbl>        <dbl> <chr>             <dbl>
#> 1 176                      10           5            4 Cropland              0
#> # ℹ 42 more variables: seeded_cover_crop_share <dbl>, area_ha <dbl>,
#> #   n_synth_kg_ha <dbl>, n_org_kg_ha <dbl>, temp_c <dbl>, pet_mm <dbl>,
#> #   som_pct <dbl>, soil_ph <dbl>, clay_pct <dbl>, water_input_mm <dbl>,
#> #   name_bnf <chr>, ndfa <dbl>, n_harvest_index <dbl>,
#> #   below_ground_n_ratio <dbl>, nonsymbiotic_base_kg_ha <dbl>,
#> #   leguminous_share <dbl>, n_total_kg_ha <dbl>, f_nitrogen_symbiotic <dbl>,
#> #   f_temperature_symbiotic <dbl>, f_water_symbiotic <dbl>, …
```
