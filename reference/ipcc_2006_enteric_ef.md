# IPCC 2006 Tier 1 enteric emission factors.

Table 10.11 (2006): Tier 1 regional EFs for enteric fermentation.

## Usage

``` r
ipcc_2006_enteric_ef
```

## Format

A tibble with `region`, `category`, `ef_kg_head_yr`.

## Source

IPCC 2006, Vol 4, Ch 10, Table 10.11.

## Examples

``` r
ipcc_2006_enteric_ef
#> # A tibble: 22 × 3
#>    region         category     ef_kg_head_yr
#>    <chr>          <chr>                <dbl>
#>  1 North America  Dairy Cattle           128
#>  2 North America  Other Cattle            53
#>  3 Western Europe Dairy Cattle           117
#>  4 Western Europe Other Cattle            57
#>  5 Eastern Europe Dairy Cattle            99
#>  6 Eastern Europe Other Cattle            58
#>  7 Oceania        Dairy Cattle            90
#>  8 Oceania        Other Cattle            60
#>  9 Latin America  Dairy Cattle            72
#> 10 Latin America  Other Cattle            56
#> # ℹ 12 more rows
```
