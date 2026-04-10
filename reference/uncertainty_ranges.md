# Uncertainty ranges for emission parameters.

Lower and upper multipliers for key emission parameters (Ym, MCF, Bo,
EF_N2O, Nex).

## Usage

``` r
uncertainty_ranges
```

## Format

A tibble with `parameter`, `lower_mult`, `upper_mult`, `distribution`.

## Source

IPCC 2019 Refinement, Vol 4, Ch 10.

## Examples

``` r
uncertainty_ranges
#> # A tibble: 5 × 4
#>   parameter lower_mult upper_mult distribution
#>   <chr>          <dbl>      <dbl> <chr>       
#> 1 Ym              0.85       1.15 normal      
#> 2 MCF             0.7        1.3  normal      
#> 3 Bo              0.8        1.2  normal      
#> 4 EF_N2O          0.5        2    lognormal   
#> 5 Nex             0.9        1.1  normal      
```
