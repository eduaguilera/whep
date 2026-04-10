# Tier 2 Ym values.

Methane conversion rate by species and feed situation for Tier 2 enteric
CH4. Includes feedlot distinction and sheep body weight differentiation.

## Usage

``` r
ipcc_tier2_ym_values
```

## Format

A tibble with `category`, `feed_situation`, `ym_percent`.

## Source

IPCC 2019 Refinement, Vol 4, Ch 10, Table 10.12.

## Examples

``` r
ipcc_tier2_ym_values
#> # A tibble: 16 × 3
#>    category feed_situation ym_percent
#>    <chr>    <chr>               <dbl>
#>  1 Cattle   High                  6.5
#>  2 Cattle   Medium                6.5
#>  3 Cattle   Low                   6.5
#>  4 Cattle   Feedlot               3  
#>  5 Buffalo  High                  6.5
#>  6 Buffalo  Medium                6.5
#>  7 Buffalo  Low                   6.5
#>  8 Sheep    High                  6.7
#>  9 Sheep    Medium                6.7
#> 10 Sheep    Low                   4.7
#> 11 Goats    High                  5.5
#> 12 Goats    Medium                5.5
#> 13 Goats    Low                   5.5
#> 14 Camels   High                  5  
#> 15 Camels   Medium                5  
#> 16 Camels   Low                   5  
```
