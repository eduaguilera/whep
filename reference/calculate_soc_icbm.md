# Simulate soil organic carbon with the ICBM two-pool model.

Analytical ICBM trajectory: a young and an old pool with a closed-form
solution evaluated at integer years. A degenerate branch handles the
case where the two effective rate constants coincide.

## Usage

``` r
calculate_soc_icbm(
  initial_soc_mgc_ha,
  c_input_mgc_ha_yr,
  years,
  clay_pct = NA,
  climate_modifier = 1
)
```

## Source

Andren, O. & Katterer, T. (1997).
[doi:10.1890/1051-0761(1997)007\[1226:ITICBM\]2.0.CO;2](https://doi.org/10.1890/1051-0761%281997%29007%5B1226%3AITICBM%5D2.0.CO%3B2)
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

  Annual climate rate modifier (dimensionless), scaling both pool
  decomposition rates.

## Value

A tibble with one row per year: `year`, `y`, `o` and `soc_total`.

## Examples

``` r
calculate_soc_icbm(
  initial_soc_mgc_ha = 50,
  c_input_mgc_ha_yr = 2,
  years = 5
)
#> # A tibble: 6 × 4
#>    year     y     o soc_total
#>   <int> <dbl> <dbl>     <dbl>
#> 1     0  2.75  47.3      50  
#> 2     1  2.61  47.2      49.9
#> 3     2  2.55  47.2      49.8
#> 4     3  2.52  47.2      49.7
#> 5     4  2.51  47.2      49.7
#> 6     5  2.50  47.2      49.7
```
