# GLEAM manure management system shares.

Regional MMS allocation by species and system.

## Usage

``` r
gleam_mms_shares
```

## Format

A tibble with `region`, `species`, `system`, `mms`, `share_percent`.

## Source

MacLeod et al. (2018) GLEAM 3.0.

## Examples

``` r
gleam_mms_shares
#> # A tibble: 14 × 5
#>    region             species system mms           share_percent
#>    <chr>              <chr>   <chr>  <chr>                 <dbl>
#>  1 Western Europe     Cattle  Dairy  Liquid/Slurry            60
#>  2 Western Europe     Cattle  Dairy  Solid Storage            30
#>  3 Western Europe     Cattle  Dairy  Pasture                  10
#>  4 Western Europe     Cattle  Beef   Pasture                  70
#>  5 Western Europe     Cattle  Beef   Solid Storage            30
#>  6 Sub-Saharan Africa Cattle  All    Pasture                  90
#>  7 Sub-Saharan Africa Cattle  All    Daily Spread             10
#>  8 Latin America      Cattle  All    Pasture                  95
#>  9 Latin America      Cattle  All    Solid Storage             5
#> 10 South Asia         Cattle  All    Daily Spread             60
#> 11 South Asia         Cattle  All    Solid Storage            30
#> 12 South Asia         Cattle  All    Pasture                  10
#> 13 East Asia          Pigs    All    Liquid/Slurry            70
#> 14 East Asia          Pigs    All    Solid Storage            30
```
