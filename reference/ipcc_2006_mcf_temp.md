# IPCC 2006 MCF by temperature.

Table 10.17 (2006): MCF values by MMS type and annual temperature.

## Usage

``` r
ipcc_2006_mcf_temp
```

## Format

A tibble with `system`, `temp_c`, `mcf_percent`.

## Source

IPCC 2006, Vol 4, Ch 10, Table 10.17.

## Examples

``` r
ipcc_2006_mcf_temp
#> # A tibble: 16 × 3
#>    system                temp_c mcf_percent
#>    <chr>                  <dbl>       <dbl>
#>  1 Liquid/Slurry             10        17  
#>  2 Liquid/Slurry             15        25  
#>  3 Liquid/Slurry             20        35  
#>  4 Liquid/Slurry             25        48  
#>  5 Solid Storage             10         2  
#>  6 Solid Storage             15         4  
#>  7 Solid Storage             20         5  
#>  8 Solid Storage             25         6  
#>  9 Pasture/Range/Paddock     10         1  
#> 10 Pasture/Range/Paddock     15         1.5
#> 11 Pasture/Range/Paddock     20         2  
#> 12 Pasture/Range/Paddock     25         2.5
#> 13 Daily Spread              10         0.1
#> 14 Daily Spread              15         0.5
#> 15 Daily Spread              20         1  
#> 16 Daily Spread              25         1.5
```
