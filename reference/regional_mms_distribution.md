# Regional MMS distribution.

Fraction of manure managed in each MMS type by region and species.

## Usage

``` r
regional_mms_distribution
```

## Format

A tibble with `region`, `species`, `mms_type`, `fraction`.

## Source

GLEAM 3.0 / FAO statistics.

## Examples

``` r
regional_mms_distribution
#> # A tibble: 33 × 4
#>    region                      species mms_type              fraction
#>    <chr>                       <chr>   <chr>                    <dbl>
#>  1 North America               Cattle  Liquid/Slurry             0.4 
#>  2 North America               Cattle  Solid Storage             0.3 
#>  3 North America               Cattle  Pasture/Range/Paddock     0.25
#>  4 North America               Cattle  Daily Spread              0.05
#>  5 Western Europe              Cattle  Liquid/Slurry             0.35
#>  6 Western Europe              Cattle  Solid Storage             0.45
#>  7 Western Europe              Cattle  Pasture/Range/Paddock     0.15
#>  8 Western Europe              Cattle  Daily Spread              0.05
#>  9 Latin America and Caribbean Cattle  Pasture/Range/Paddock     0.7 
#> 10 Latin America and Caribbean Cattle  Solid Storage             0.15
#> # ℹ 23 more rows
```
