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

A tibble with one row per year: `year`, `fresh`, `humus`, `iom` and
`soc_total`.

## Examples

``` r
calculate_soc_hsoc(
  initial_soc_mgc_ha = 50,
  c_input_mgc_ha_yr = 2,
  years = 5
)
#> # A tibble: 6 × 5
#>    year fresh humus   iom soc_total
#>   <int> <dbl> <dbl> <dbl>     <dbl>
#> 1     0  2.92    30  4.22      37.1
#> 2     1  2.92    30  4.22      37.1
#> 3     2  2.92    30  4.22      37.1
#> 4     3  2.92    30  4.22      37.1
#> 5     4  2.92    30  4.22      37.1
#> 6     5  2.92    30  4.22      37.1
```
