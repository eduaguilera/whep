# Compare two footprint estimates.

Align two consumption-based footprint estimates (for example the
Leontief and land-balance methods) by consuming country and item and
report their difference. Disagreement is diagnostic: it localises where
a method's assumptions matter most.

## Usage

``` r
compare_footprint_methods(method_a, method_b)
```

## Arguments

- method_a, method_b:

  Tibbles with `area_code`, `item_cbs_code` and `value` (consumption
  footprint).

## Value

A tibble with `area_code`, `item_cbs_code`, `value_a`, `value_b`,
`abs_diff` and `rel_diff` (relative to the larger of the two), ordered
by descending `abs_diff`.

## Examples

``` r
a <- tibble::tibble(area_code = 1L, item_cbs_code = 10L, value = 30)
b <- tibble::tibble(area_code = 1L, item_cbs_code = 10L, value = 25)
compare_footprint_methods(a, b)
#> # A tibble: 1 × 6
#>   area_code item_cbs_code value_a value_b abs_diff rel_diff
#>       <int>         <int>   <dbl>   <dbl>    <dbl>    <dbl>
#> 1         1            10      30      25        5    0.167
```
