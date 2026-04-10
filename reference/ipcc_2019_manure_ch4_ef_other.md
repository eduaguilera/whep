# IPCC 2019 manure CH4 EF for non-cattle.

Table 10.14: Tier 1 manure management CH4 emission factors for
non-cattle species (kg CH4/head/yr).

## Usage

``` r
ipcc_2019_manure_ch4_ef_other
```

## Format

A tibble with `category`, `ef_kg_head_yr`.

## Source

IPCC 2019 Refinement, Vol 4, Ch 10, Table 10.14.

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
