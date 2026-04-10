# Tier 2 manure ash content.

Ash content of manure as percent of dry matter, used in VS calculation
(Eq 10.24).

## Usage

``` r
ipcc_tier2_manure_ash
```

## Format

A tibble with `category`, `ash_percent`.

## Source

IPCC 2019 Refinement, Vol 4, Ch 10.

## Examples

``` r
ipcc_tier2_manure_ash
#> # A tibble: 9 × 2
#>   category        ash_percent
#>   <chr>                 <dbl>
#> 1 Cattle                    8
#> 2 Buffalo                   8
#> 3 Sheep                     8
#> 4 Goats                     8
#> 5 Swine                     4
#> 6 Poultry                  25
#> 7 Horses                   20
#> 8 Camels                    8
#> 9 Mules and Asses           8
```
