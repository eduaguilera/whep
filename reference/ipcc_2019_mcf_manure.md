# IPCC 2019 MCF for manure management.

Table 10.17: methane conversion factors (percent) by manure management
system and climate zone. The IPCC table resolves ten climate zones
grouped under cool, temperate and warm; this table keeps the three
groups, and uses `"All"` for the systems that take a single factor.

## Usage

``` r
ipcc_2019_mcf_manure
```

## Format

A tibble with `system`, `climate_zone`, `mcf_percent`.

## Source

IPCC 2019 Refinement, Vol 4, Ch 10, Table 10.17.

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
