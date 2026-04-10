# Temperature adjustment factors for NEm.

Adjustment multipliers for net energy maintenance under cold stress,
thermoneutral, and heat stress conditions.

## Usage

``` r
temperature_adjustment
```

## Format

A tibble with `temp_range`, `temp_min`, `temp_max`, `adjustment_factor`.

## Source

NRC 2001; IPCC 2019.

## Examples

``` r
temperature_adjustment
#> # A tibble: 3 × 4
#>   temp_range    temp_min temp_max adjustment_factor
#>   <chr>            <dbl>    <dbl>             <dbl>
#> 1 Cold Stress       -Inf        5               0.2
#> 2 Thermoneutral        5       25               0  
#> 3 Heat Stress         25      Inf               0.1
```
