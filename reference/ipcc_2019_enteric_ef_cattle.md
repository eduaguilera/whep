# IPCC 2019 enteric EF for cattle.

Table 10.10: Tier 1 enteric fermentation emission factors for cattle by
region (kg CH4/head/yr).

## Usage

``` r
ipcc_2019_enteric_ef_cattle
```

## Format

A tibble with `region`, `category`, `ef_kg_head_yr`, `source`.

## Source

IPCC 2019 Refinement, Vol 4, Ch 10, Table 10.10.

## Examples

``` r
ipcc_2019_enteric_ef_cattle
#> # A tibble: 20 × 3
#>    region              category     ef_kg_head_yr
#>    <chr>               <chr>                <dbl>
#>  1 North America       Dairy Cattle           128
#>  2 North America       Other Cattle            53
#>  3 Western Europe      Dairy Cattle           117
#>  4 Western Europe      Other Cattle            57
#>  5 Eastern Europe      Dairy Cattle            99
#>  6 Eastern Europe      Other Cattle            58
#>  7 Oceania             Dairy Cattle            90
#>  8 Oceania             Other Cattle            60
#>  9 Latin America       Dairy Cattle            72
#> 10 Latin America       Other Cattle            56
#> 11 Asia                Dairy Cattle            68
#> 12 Asia                Other Cattle            47
#> 13 Africa              Dairy Cattle            46
#> 14 Africa              Other Cattle            31
#> 15 Middle East         Dairy Cattle            63
#> 16 Middle East         Other Cattle            31
#> 17 Indian Subcontinent Dairy Cattle            68
#> 18 Indian Subcontinent Other Cattle            47
#> 19 Global              Dairy Cattle            80
#> 20 Global              Other Cattle            47
```
