# IPCC 2019 enteric EF for non-cattle.

Table 10.11: Tier 1 enteric fermentation emission factors for non-cattle
species (kg CH4/head/yr).

## Usage

``` r
ipcc_2019_enteric_ef_other
```

## Format

A tibble with `category`, `ef_kg_head_yr`, `source`.

## Source

IPCC 2019 Refinement, Vol 4, Ch 10, Table 10.11.

## Examples

``` r
ipcc_2019_enteric_ef_other
#> # A tibble: 9 × 2
#>   category         ef_kg_head_yr
#>   <chr>                    <dbl>
#> 1 Buffalo                   55  
#> 2 Sheep                      8  
#> 3 Goats                      5  
#> 4 Camels                    46  
#> 5 Horses                    18  
#> 6 Mules and Asses           10  
#> 7 Swine - Market             1.5
#> 8 Swine - Breeding           1.5
#> 9 Poultry                    0  
```
