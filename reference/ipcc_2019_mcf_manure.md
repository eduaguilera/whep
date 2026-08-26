# IPCC 2019 MCF for manure management.

Methane conversion factors (percent) by manure management system and
climate zone, using `"All"` for the systems that take a single factor.

## Usage

``` r
ipcc_2019_mcf_manure
```

## Format

A tibble with `system`, `climate_zone`, `mcf_percent`.

## Source

Predominantly the 2006 Guidelines, Vol 4, Ch 10, Table 10.17, whose
cool/temperate/warm structure this table follows. The 2019 Refinement's
Table 10.17 (Updated) is resolved by ten climate zones and by
liquid-system retention time instead, and differs in level: it gives a
single 0.47 percent for pasture/range/paddock against 1.0/1.5/2.0 here,
and 1.0/2.0/2.5 for static-pile and passive-windrow composting against
0.5/0.5/0.5 and 1.0/1.0/1.5 here. Some cells match neither edition: dry
lot 1.5/2.5/4.0 (both editions give 1.0/1.5/2.0), intensive-windrow
composting 0.5/0.5/0.5 (both give 0.5/1.0/1.5) and pit storage under one
month 3/3/5 (2006 gives 3/3/30). Where a 2006 row is resolved per degree
Celsius the value taken is not always the mid-point of the class
(uncovered anaerobic lagoon temperate 73 percent is the 14 degree
column, not the 78 percent of 20 degrees); tracked in whep#601.

## Examples

``` r
ipcc_2019_mcf_manure
#> # A tibble: 47 × 3
#>    system                climate_zone mcf_percent
#>    <chr>                 <chr>              <dbl>
#>  1 Pasture/Range/Paddock Cool                 1  
#>  2 Pasture/Range/Paddock Temperate            1.5
#>  3 Pasture/Range/Paddock Warm                 2  
#>  4 Daily Spread          Cool                 0.1
#>  5 Daily Spread          Temperate            0.5
#>  6 Daily Spread          Warm                 1  
#>  7 Solid Storage         Cool                 2  
#>  8 Solid Storage         Temperate            4  
#>  9 Solid Storage         Warm                 5  
#> 10 Dry Lot               Cool                 1.5
#> # ℹ 37 more rows
```
