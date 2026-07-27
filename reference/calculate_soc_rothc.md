# Simulate soil organic carbon with the RothC five-pool model.

Annual RothC trajectory integrated by monthly Euler steps. Active carbon
is partitioned into decomposable plant material, resistant plant
material, microbial biomass and humified organic matter, plus a Falloon
(1998) inert pool. Decomposed carbon is split between biomass and humus
by a clay-texture function.

## Usage

``` r
calculate_soc_rothc(
  initial_soc_mgc_ha,
  c_input_mgc_ha_yr,
  years,
  clay_pct = NA,
  climate_modifier = 1
)
```

## Source

Coleman, K. & Jenkinson, D. S. (1996).
[doi:10.1007/978-3-642-61094-3_17](https://doi.org/10.1007/978-3-642-61094-3_17)
; inert organic matter: Falloon, P. et al. (1998).
[doi:10.1016/S0038-0717(97)00256-3](https://doi.org/10.1016/S0038-0717%2897%2900256-3)
.

## Arguments

- initial_soc_mgc_ha:

  Initial soil organic carbon stock (Mg C per ha).

- c_input_mgc_ha_yr:

  Annual carbon input (Mg C per ha per year).

- years:

  Number of years to simulate.

- clay_pct:

  Soil clay content (percent).

- climate_modifier:

  Annual climate rate modifier (dimensionless), applied to every monthly
  decomposition step.

## Value

A tibble with one row per year: `year`, `dpm`, `rpm`, `bio`, `hum`,
`iom` and `soc_total`.

## Examples

``` r
calculate_soc_rothc(
  initial_soc_mgc_ha = 50,
  c_input_mgc_ha_yr = 2,
  years = 5,
  clay_pct = 20
)
#> # A tibble: 6 × 7
#>    year   dpm   rpm   bio   hum   iom soc_total
#>   <int> <dbl> <dbl> <dbl> <dbl> <dbl>     <dbl>
#> 1     0 0.458 0.916 2.29   42.1  4.22      50  
#> 2     1 0.174 1.40  1.46   41.7  4.22      49.0
#> 3     2 0.174 1.75  0.999  41.3  4.22      48.4
#> 4     3 0.174 2.01  0.748  40.8  4.22      48.0
#> 5     4 0.174 2.21  0.614  40.4  4.22      47.6
#> 6     5 0.174 2.35  0.543  39.9  4.22      47.2
```
