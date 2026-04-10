# IPCC 2006 Tier 1 manure emission factors.

Table 10.14 (2006): Tier 1 regional EFs for manure CH4.

## Usage

``` r
ipcc_2006_manure_ef
```

## Format

A tibble with `region`, `category`, `ef_kg_head_yr`, `temp_zone`.

## Source

IPCC 2006, Vol 4, Ch 10, Table 10.14.

## Examples

``` r
ipcc_2006_manure_ef
#> # A tibble: 13 × 4
#>    region         category     ef_kg_head_yr temp_zone
#>    <chr>          <chr>                <dbl> <chr>    
#>  1 North America  Dairy Cattle         53    Cool     
#>  2 North America  Other Cattle          2    Cool     
#>  3 Western Europe Dairy Cattle         20    Cool     
#>  4 Western Europe Other Cattle          6    Cool     
#>  5 Latin America  Dairy Cattle          1    Warm     
#>  6 Latin America  Other Cattle          1    Warm     
#>  7 Asia           Dairy Cattle         16    Warm     
#>  8 Asia           Other Cattle          1    Warm     
#>  9 Global         Buffalo               2    Warm     
#> 10 Global         Sheep                 0.19 All      
#> 11 Global         Goats                 0.13 All      
#> 12 Global         Swine                 6    All      
#> 13 Global         Poultry               0.02 All      
```
