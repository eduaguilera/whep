# IPCC 2019 manure CH4 EF for non-cattle.

Tier 1 manure management CH4 emission factors for non-cattle species (kg
CH4/head/yr).

## Usage

``` r
ipcc_2019_manure_ch4_ef_other
```

## Format

A tibble with `category`, `ef_kg_head_yr`.

## Source

2006 Guidelines, Vol 4, Ch 10, Tables 10.14 (buffalo, swine) and 10.15
(sheep, goats, poultry, horses, mules and asses, camels), not the 2019
Refinement, which publishes no per-head Tier 1 manure CH4 table. The
temperature column each value is taken from varies by species (sheep
0.19 and goats 0.13 are the developed-country cool column, while horses
1.64, mules 0.90 and camels 1.92 are the developing-country temperate
column); tracked in whep#601.

## Examples

``` r
ipcc_2019_manure_ch4_ef_other
#> # A tibble: 10 × 3
#>    category           climate ef_kg_head_yr
#>    <chr>              <chr>           <dbl>
#>  1 Buffalo            All              2   
#>  2 Sheep              All              0.19
#>  3 Goats              All              0.13
#>  4 Swine - Market     All              6   
#>  5 Swine - Breeding   All              6   
#>  6 Poultry - Broilers All              0.02
#>  7 Poultry - Layers   All              0.03
#>  8 Horses             All              1.64
#>  9 Mules and Asses    All              0.9 
#> 10 Camels             All              1.92
```
