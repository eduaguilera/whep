# Tier 2 nitrogen retention fractions.

Fraction of N intake retained in animal products. Dairy cattle 0.20 vs
other cattle 0.07.

## Usage

``` r
ipcc_tier2_n_retention
```

## Format

A tibble with `category`, `n_retention_frac`.

## Source

IPCC 2019 Refinement, Vol 4, Ch 10, Table 10.20.

## Examples

``` r
ipcc_tier2_n_retention
#> # A tibble: 10 × 2
#>    category        n_retention_frac
#>    <chr>                      <dbl>
#>  1 Dairy Cattle                0.2 
#>  2 Other Cattle                0.07
#>  3 Buffalo                     0.2 
#>  4 Sheep                       0.1 
#>  5 Goats                       0.1 
#>  6 Swine                       0.3 
#>  7 Poultry                     0.3 
#>  8 Horses                      0.05
#>  9 Camels                      0.05
#> 10 Mules and Asses             0.05
```
