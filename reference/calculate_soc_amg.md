# Simulate soil organic carbon with the AMG model.

Analytical AMG trajectory: an active decomposing pool and an inert
stable pool. The humification coefficient is selected from the carbon
input type via
[`whep::amg_h_by_input_type`](https://eduaguilera.github.io/whep/reference/amg_h_by_input_type.md)
(default 0.15). The active pool relaxes to its steady state
`h * input / k`; the stable pool is constant.

## Usage

``` r
calculate_soc_amg(
  initial_soc_mgc_ha,
  c_input_mgc_ha_yr,
  years,
  clay_pct = NA,
  climate_modifier = 1,
  c_input_type = NA,
  init_mode = c("fixed_iom", "steady_state")
)
```

## Source

Saffih-Hdadi, K. & Mary, B. (2008).
[doi:10.1016/j.soilbio.2007.08.022](https://doi.org/10.1016/j.soilbio.2007.08.022)
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

  Annual climate rate modifier (dimensionless), scaling the active-pool
  decomposition rate.

- c_input_type:

  Carbon input type label used to look up the humification coefficient.

- init_mode:

  Initial pool split. `"fixed_iom"` splits the supplied initial stock by
  the published stable fraction. `"steady_state"` ignores the supplied
  stock and starts from the analytical equilibrium `ca_ss / (1 - f_iom)`
  (active pool at its steady state `ca_ss = h * input / k`, stable pool
  the matching inert share).

## Value

A tibble with one row per year: `year`, `ca`, `cs` and `soc_total`.

## Examples

``` r
calculate_soc_amg(
  initial_soc_mgc_ha = 50,
  c_input_mgc_ha_yr = 2,
  years = 5
)
#> # A tibble: 6 × 4
#>    year    ca    cs soc_total
#>   <int> <dbl> <dbl>     <dbl>
#> 1     0 17.5   32.5      50  
#> 2     1 15.1   32.5      47.6
#> 3     2 13.1   32.5      45.6
#> 4     3 11.4   32.5      43.9
#> 5     4  9.92  32.5      42.4
#> 6     5  8.69  32.5      41.2
```
