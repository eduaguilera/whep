# Simulate soil organic carbon with the HSOC two-pool model.

Annual HSOC trajectory (Spain historical pipeline): a fresh and a humus
decomposing pool plus an inert organic matter pool. The inert pool is
the Falloon (1998) function of initial carbon. Each year a pool stock
loses first-order decomposition and gains its carbon input.
Land-use-change carbon transfer is deferred to a later phase (single
land use here).

## Usage

``` r
calculate_soc_hsoc(
  initial_soc_mgc_ha,
  c_input_mgc_ha_yr,
  years,
  clay_pct = NA,
  climate_modifier = 1,
  humification_fraction = 0.3
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

  Soil clay content (percent); unused, kept for contract.

- climate_modifier:

  Annual climate rate modifier (dimensionless).

- humification_fraction:

  Fraction of carbon input humified into the humus pool (the remainder
  feeds the fresh pool).

## Value

A tibble with one row per year and pool: `year`, `pool` (fresh, humus,
iom), `stock_mgc_ha` and `rate_mgc_ha`.

## Examples

``` r
calculate_soc_hsoc(
  initial_soc_mgc_ha = 50,
  c_input_mgc_ha_yr = 2,
  years = 5
)
#> # A tibble: 18 × 4
#>    pool   year stock_mgc_ha rate_mgc_ha
#>    <chr> <int>        <dbl>       <dbl>
#>  1 fresh     0         2.92           0
#>  2 fresh     1         2.92           0
#>  3 fresh     2         2.92           0
#>  4 fresh     3         2.92           0
#>  5 fresh     4         2.92           0
#>  6 fresh     5         2.92           0
#>  7 humus     0        30              0
#>  8 humus     1        30              0
#>  9 humus     2        30              0
#> 10 humus     3        30              0
#> 11 humus     4        30              0
#> 12 humus     5        30              0
#> 13 iom       0         4.22           0
#> 14 iom       1         4.22           0
#> 15 iom       2         4.22           0
#> 16 iom       3         4.22           0
#> 17 iom       4         4.22           0
#> 18 iom       5         4.22           0
```
