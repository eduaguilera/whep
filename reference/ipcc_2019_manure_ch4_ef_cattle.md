# IPCC 2019 manure CH4 EF for cattle.

Tier 1 manure management CH4 emission factors for cattle by region (kg
CH4/head/yr).

## Usage

``` r
ipcc_2019_manure_ch4_ef_cattle
```

## Format

A tibble with `region`, `category`, `ef_kg_head_yr`.

## Source

Not the 2019 Refinement. Its Table 10.14 (Updated) publishes manure CH4
per kilogram of volatile solids (g CH4 kg VS-1), by productivity class
and ten climate zones; the Refinement contains no per-head Tier 1 manure
CH4 table at all (its only per-head CH4 tables are the enteric Tables
10.10/10.11 and Table 10.15 for deer, reindeer, rabbits, ostrich and
fur-bearing animals). The per-head quantity stored here is the form of
the 2006 Guidelines Table 10.14, but the values do not match it either
(North American dairy cattle 27/42/60 for cool/temperate/warm against
48/78/112 in 2006; Latin American dairy cattle 47 against 2). **The
provenance of these values is unknown and unverified**; tracked in
whep#601.

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
