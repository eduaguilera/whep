# IPCC 2019 Cfi values (Table 10.4).

Net energy maintenance coefficients (MJ/day/kg^0.75). Dairy (lactating)
cattle use 0.386; non-dairy 0.322.

## Usage

``` r
ipcc_2019_cfi
```

## Format

A tibble with `category`, `subcategory`, `cfi_mj_day_kg075`.

## Source

IPCC 2019 Refinement, Vol 4, Ch 10, Table 10.4.

## Examples

``` r
ipcc_2019_cfi
#> # A tibble: 6 × 3
#>   category subcategory         cfi_mj_day_kg075
#>   <chr>    <chr>                          <dbl>
#> 1 Cattle   Lactating cow                  0.386
#> 2 Cattle   Non-lactating/Bulls            0.322
#> 3 Buffalo  Lactating cow                  0.386
#> 4 Buffalo  Non-lactating/Bulls            0.322
#> 5 Sheep    All                            0.217
#> 6 Goats    All                            0.217
```
