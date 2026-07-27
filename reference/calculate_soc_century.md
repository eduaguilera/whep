# Simulate soil organic carbon with the Century five-pool model.

Annual Century trajectory integrated with
[`deSolve::lsoda`](https://rdrr.io/pkg/deSolve/man/lsoda.html). Carbon
flows through structural and metabolic litter and active, slow and
passive soil organic matter pools, with texture-dependent rates and
inter-pool transfer fractions.

## Usage

``` r
calculate_soc_century(
  initial_soc_mgc_ha,
  c_input_mgc_ha_yr,
  years,
  clay_pct = NA,
  climate_modifier = 1
)
```

## Source

Parton, W. J. et al. (1987).
[doi:10.2136/sssaj1987.03615995005100050015x](https://doi.org/10.2136/sssaj1987.03615995005100050015x)
; SoilR implementation: Sierra, C. A. et al. (2012).
[doi:10.5194/gmd-5-1045-2012](https://doi.org/10.5194/gmd-5-1045-2012) .

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

  Annual climate rate modifier (dimensionless), scaling every pool
  decomposition rate.

## Value

A tibble with one row per year: `year`, `str`, `met`, `act`, `slw`,
`pas` and `soc_total`.

## Examples

``` r
calculate_soc_century(
  initial_soc_mgc_ha = 50,
  c_input_mgc_ha_yr = 2,
  years = 5,
  clay_pct = 20
)
#> # A tibble: 6 × 7
#>    year   str    met   act   slw   pas soc_total
#>   <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl>     <dbl>
#> 1     0  0    0      2.5    20    27.5      50  
#> 2     1  1.06 0.0143 0.584  18.7  27.4      47.8
#> 3     2  1.42 0.0143 0.528  16.8  27.4      46.2
#> 4     3  1.53 0.0143 0.503  15.4  27.3      44.7
#> 5     4  1.58 0.0143 0.478  14.1  27.2      43.4
#> 6     5  1.59 0.0143 0.455  13.1  27.1      42.2
```
