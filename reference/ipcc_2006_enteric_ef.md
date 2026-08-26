# IPCC 2006 Tier 1 enteric emission factors.

Table 10.11 (2006): Tier 1 regional EFs for enteric fermentation, with
the non-cattle species of Table 10.10 appended under a `"Global"`
region.

## Usage

``` r
ipcc_2006_enteric_ef
```

## Format

A tibble with `region`, `category`, `ef_kg_head_yr`.

## Source

IPCC 2006, Vol 4, Ch 10, Table 10.11 for cattle and Table 10.10
(developed-countries column) for the non-cattle rows. Two departures
from the published table: Oceania dairy cattle is stored as 90 where
Table 10.11 gives 100, and the published table groups Africa **and** the
Middle East in one row (46 dairy / 31 other) which is repeated here as
two regions. The Indian Subcontinent row of Table 10.11 (58 dairy / 27
other) is absent. Tracked in whep#601.

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
