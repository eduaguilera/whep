# GLEAM enteric fermentation parameters.

Ym (% GE) values by species and production system. Feedlot cattle use
3.0% per IPCC 2019 Table 10.12.

## Usage

``` r
gleam_enteric_params
```

## Format

A tibble with `species`, `system`, `ym_percent`, `notes`.

## Source

IPCC 2019 Refinement, Vol 4, Ch 10, Table 10.12.

## Examples

``` r
gleam_enteric_params
#> # A tibble: 10 × 4
#>    species system  ym_percent notes                 
#>    <chr>   <chr>        <dbl> <chr>                 
#>  1 Cattle  Grazing        6.5 IPCC default          
#>  2 Cattle  Mixed          6.5 IPCC default          
#>  3 Cattle  Feedlot        3   High concentrate diet 
#>  4 Buffalo Grazing        6.5 IPCC default          
#>  5 Buffalo Mixed          6.5 IPCC default          
#>  6 Sheep   Grazing        6.5 IPCC default          
#>  7 Sheep   Mixed          6.5 IPCC default          
#>  8 Goats   Grazing        5.5 IPCC default          
#>  9 Goats   Mixed          5.5 IPCC default          
#> 10 Pigs    All            0   Negligible enteric CH4
```
