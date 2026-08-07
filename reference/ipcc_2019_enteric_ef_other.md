# IPCC 2019 enteric EF for non-cattle.

Tier 1 enteric fermentation emission factors for non-cattle species (kg
CH4/head/yr). Non-cattle species are Table 10.10 in both the 2006
Guidelines and the 2019 Refinement; regional cattle factors are Table
10.11 (see
[ipcc_2019_enteric_ef_cattle](https://eduaguilera.github.io/whep/reference/ipcc_2019_enteric_ef_cattle.md)).

## Usage

``` r
ipcc_2019_enteric_ef_other
```

## Format

A tibble with `category`, `ef_kg_head_yr`.

## Source

IPCC 2019 Refinement, Vol 4, Ch 10, Table 10.10. The stored values are
in fact the 2006 Guidelines defaults, from its developed-countries
column; tracked in whep#601.

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
