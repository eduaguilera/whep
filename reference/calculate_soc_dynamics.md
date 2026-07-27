# Simulate soil organic carbon dynamics with a selectable model.

Run one of the five soil-organic-carbon turnover models (HSOC, RothC,
ICBM, AMG or Century) through a single interface. When climate drivers
are present in `data` (for example `temp_c`), the selected model's
native annual climate rate modifier is computed from the matching
climate function and passed in; otherwise the model runs with a neutral
modifier of 1. The chosen model is stamped into a `method_soc` column on
the output.

## Usage

``` r
calculate_soc_dynamics(
  model = c("hsoc", "rothc", "icbm", "amg", "century"),
  data = list(),
  example = FALSE
)
```

## Source

Model bodies and climate functions as cited in
[`calculate_soc_hsoc`](https://eduaguilera.github.io/whep/reference/calculate_soc_hsoc.md)
and
[`soc_rate_modifier_rothc`](https://eduaguilera.github.io/whep/reference/soc_rate_modifier_rothc.md).

## Arguments

- model:

  SOC model to run: one of `"hsoc"`, `"rothc"`, `"icbm"`, `"amg"` or
  `"century"`. Defaults to the most detailed pool structure available,
  `"hsoc"`.

- data:

  Named list of model arguments. Always carries `initial_soc_mgc_ha`,
  `c_input_mgc_ha_yr` and `years`; may also carry `clay_pct` and
  model-specific arguments (for example `c_input_type` and `init_mode`
  for AMG). Climate drivers, when present, are read from the same list
  to build the climate modifier (see
  [`soc_rate_modifier_rothc`](https://eduaguilera.github.io/whep/reference/soc_rate_modifier_rothc.md)
  and siblings for the per-model driver names). If the drivers are
  absent but `data$climate_modifier` already holds a value, that value
  is used instead of recomputing it; only when neither is available does
  the model run with a neutral modifier of 1.

- example:

  If `TRUE`, return a small hardcoded example tibble instead of running
  the model.

## Value

A tibble of the selected model's trajectory with an added `method_soc`
column naming the model.

## Examples

``` r
calculate_soc_dynamics(
  model = "icbm",
  data = list(initial_soc_mgc_ha = 50, c_input_mgc_ha_yr = 2, years = 5)
)
#> # A tibble: 6 × 5
#>    year     y     o soc_total method_soc
#>   <int> <dbl> <dbl>     <dbl> <chr>     
#> 1     0  2.75  47.3      50   icbm      
#> 2     1  2.61  47.2      49.9 icbm      
#> 3     2  2.55  47.2      49.8 icbm      
#> 4     3  2.52  47.2      49.7 icbm      
#> 5     4  2.51  47.2      49.7 icbm      
#> 6     5  2.50  47.2      49.7 icbm      
```
