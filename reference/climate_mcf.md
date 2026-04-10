# Climate-zone MCF values.

Methane Conversion Factors by MMS type and climate zone
(Cool/Temperate/Warm).

## Usage

``` r
climate_mcf
```

## Format

A tibble with `mms_type`, `climate_zone`, `mcf_percent`.

## Source

IPCC 2019 Refinement, Vol 4, Ch 10, Table 10.17.

## Examples

``` r
climate_mcf
#> # A tibble: 25 × 3
#>    mms_type      climate_zone mcf_percent
#>    <chr>         <chr>              <dbl>
#>  1 Daily Spread  Cool                 0.1
#>  2 Daily Spread  Temperate            0.5
#>  3 Daily Spread  Warm                 1  
#>  4 Solid Storage Cool                 2  
#>  5 Solid Storage Temperate            4  
#>  6 Solid Storage Warm                 5  
#>  7 Dry Lot       Cool                 1.5
#>  8 Dry Lot       Temperate            2.5
#>  9 Dry Lot       Warm                 4  
#> 10 Liquid/Slurry Cool                17  
#> # ℹ 15 more rows
```
