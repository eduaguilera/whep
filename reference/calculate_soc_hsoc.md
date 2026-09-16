# Simulate soil organic carbon with the HSOC two-pool model.

Annual HSOC trajectory (Spain historical pipeline): a fresh and a humus
decomposing pool plus an inert organic matter pool. The inert pool is
the Falloon (1998) function of initial carbon and, as in that paper, is
a component of the measured stock rather than an addition to it, so the
two decomposing pools open on the remainder `initial_soc_mgc_ha - iom`,
split between them in the proportion of their steady states
`input_pool / k_pool`. Each year a pool stock loses first-order
decomposition and gains its carbon input, so the trajectory relaxes from
the supplied stock toward that steady state. Land-use-change carbon
transfer is deferred to a later phase (single land use here).

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

  Initial soil organic carbon stock (Mg C per ha). The trajectory starts
  here: year 0 of the returned tibble reports this stock, as it does for
  the four sibling models.

- c_input_mgc_ha_yr:

  Annual carbon input (Mg C per ha per year).

- years:

  Number of years to simulate.

- clay_pct:

  Soil clay content (percent). Scales `humification_fraction` by the
  Aguilera et al. (2018) Eq. 5-6 texture modifier, which runs 0.72 at 5%
  clay to 1.13 at 60% and is 1 at RothC's Rothamsted reference of 23.4%:
  coarse soils stabilise less of the same carbon input. `NA`, the
  default, means no texture information was supplied and applies no
  texture adjustment. The gridded balance always supplies clay, so this
  function and `build_carbon_balance(model = "hsoc")` now return the
  same stock for the same inputs.

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
#> 1     0  4.06  41.7  4.22      50  
#> 2     1  3.51  41.5  4.22      49.2
#> 3     2  3.22  41.3  4.22      48.7
#> 4     3  3.08  41.0  4.22      48.3
#> 5     4  3.00  40.8  4.22      48.0
#> 6     5  2.96  40.6  4.22      47.8
```
