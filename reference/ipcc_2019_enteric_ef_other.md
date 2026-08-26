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

The 2006 Guidelines, Vol 4, Ch 10, Table 10.10, developed-countries
column (buffalo 55, sheep 8, goats 5, camels 46, horses 18, mules and
asses 10, swine 1.5), not the 2019 Refinement. The Refinement's Table
10.10 (Updated) splits every ruminant and swine factor by productivity
system (sheep 9 high / 5 low, goats 9 / 5, swine 1.5 / 1.0), leaves
camels, horses and mules unchanged, and moves buffalo out of this table
into the regional Table 10.11. Poultry is stored as `0`; both editions
say "insufficient data for calculation", so the zero is a project choice
rather than a published factor. Tracked in whep#601.

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
