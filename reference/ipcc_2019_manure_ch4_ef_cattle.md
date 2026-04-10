# IPCC 2019 manure CH4 EF for cattle.

Table 10.14: Tier 1 manure management CH4 emission factors for cattle by
region (kg CH4/head/yr).

## Usage

``` r
ipcc_2019_manure_ch4_ef_cattle
```

## Format

A tibble with `region`, `category`, `ef_kg_head_yr`.

## Source

IPCC 2019 Refinement, Vol 4, Ch 10, Table 10.14.

## Examples

``` r
ipcc_2019_manure_ch4_ef_cattle
#> # A tibble: 26 × 4
#>    region         category     climate   ef_kg_head_yr
#>    <chr>          <chr>        <chr>             <dbl>
#>  1 North America  Dairy Cattle Cool                 27
#>  2 North America  Dairy Cattle Temperate            42
#>  3 North America  Dairy Cattle Warm                 60
#>  4 North America  Other Cattle Cool                  2
#>  5 North America  Other Cattle Temperate             3
#>  6 North America  Other Cattle Warm                  4
#>  7 Western Europe Dairy Cattle Cool                 31
#>  8 Western Europe Dairy Cattle Temperate            39
#>  9 Western Europe Other Cattle Cool                  1
#> 10 Western Europe Other Cattle Temperate             1
#> # ℹ 16 more rows
```
