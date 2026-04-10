# IPCC 2019 nitrogen excretion rates.

Table 10.19: Daily N excretion rates by species and region (kg N/1000 kg
animal mass/day).

## Usage

``` r
ipcc_2019_n_excretion
```

## Format

A tibble with `region`, `category`, `nex_kg_per_1000kg_day`.

## Source

IPCC 2019 Refinement, Vol 4, Ch 10, Table 10.19.

## Examples

``` r
ipcc_2019_n_excretion
#> # A tibble: 30 × 3
#>    region         category     nex_kg_n_head_yr
#>    <chr>          <chr>                   <dbl>
#>  1 North America  Dairy Cattle              105
#>  2 North America  Other Cattle               56
#>  3 Western Europe Dairy Cattle              100
#>  4 Western Europe Other Cattle               50
#>  5 Eastern Europe Dairy Cattle               80
#>  6 Eastern Europe Other Cattle               50
#>  7 Oceania        Dairy Cattle               80
#>  8 Oceania        Other Cattle               40
#>  9 Latin America  Dairy Cattle               50
#> 10 Latin America  Other Cattle               40
#> # ℹ 20 more rows
```
