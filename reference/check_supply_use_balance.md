# Check the commodity balance sheet supply-use identity.

Verify that total supply equals total use for every row of a wide
commodity balance sheet, the fundamental accounting identity behind the
input-output model. Supply is `production + import + stock_withdrawal`;
use is
`export + food + feed + seed + processing + other_uses + stock_addition`.

## Usage

``` r
check_supply_use_balance(cbs, tol = 1e-06)
```

## Arguments

- cbs:

  Wide commodity balance sheet, e.g. from
  [`get_wide_cbs()`](https://eduaguilera.github.io/whep/reference/get_wide_cbs.md).

- tol:

  Absolute tolerance (in the data's mass units) for a row to count as
  balanced.

## Value

A tibble with the key columns present in `cbs` plus:

- `supply`: Total supply.

- `use`: Total use.

- `abs_diff`: `abs(supply - use)`.

- `rel_diff`: `abs_diff / supply` (`NA` when supply is zero).

- `balanced`: `TRUE` when `abs_diff <= tol`. Rows are ordered by
  descending absolute difference.

## Examples

``` r
get_wide_cbs(example = TRUE) |>
  check_supply_use_balance()
#> # A tibble: 10 × 8
#>     year area_code item_cbs_code     supply       use abs_diff rel_diff balanced
#>    <int>     <int>         <dbl>      <dbl>     <dbl>    <dbl>    <dbl> <lgl>   
#>  1  1987       250          2106 13741247      1.37e7    41247 0.00300  FALSE   
#>  2  2012        41          2633   146000      1.50e5     4158 0.0285   FALSE   
#>  3  1984       123          2595     3854      5.36e3     1503 0.390    FALSE   
#>  4  1995        49          2734    71524      7.11e4      407 0.00569  FALSE   
#>  5  1961       236          2620    11200      1.12e4       23 0.00205  FALSE   
#>  6  1961       156          2658     6880      6.88e3        3 0.000436 FALSE   
#>  7  1977       159          2658     2220      2.22e3        2 0.000901 FALSE   
#>  8  1982       165          2633       87.5    8.75e1        0 0        TRUE    
#>  9  1995       234          2671     4490      4.49e3        0 0        TRUE    
#> 10  1975        10           677     2270      2.27e3        0 0        TRUE    
```
