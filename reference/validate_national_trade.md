# Validate national net trade balance

Validates the provincial GRAFS model by comparing Spain's national net
trade computed bottom-up (summing provincial net balances per item)
against official FAOSTAT figures. Because internal inter-province flows
cancel out pairwise, the sum of provincial net balances equals Spain's
true international net trade.

## Usage

``` r
validate_national_trade(n_prov_destiny = NULL, example = FALSE)
```

## Arguments

- n_prov_destiny:

  Optional pre-computed output from
  [`create_n_prov_destiny()`](https://eduaguilera.github.io/whep/reference/create_n_prov_destiny.md).
  If `NULL`, calls that function internally.

- example:

  If `TRUE`, return a small hardcoded output without downloading remote
  data. Default is `FALSE`.

## Value

A tibble with columns `year`, `item`, `net_prov`, `net_fao`, and
`diff_net` (all in MgN).

## Examples

``` r
validate_national_trade(example = TRUE)
#> # A tibble: 6 × 5
#>    year item                 net_prov net_fao diff_net
#>   <dbl> <chr>                   <dbl>   <dbl>    <dbl>
#> 1  2000 Barley and products    56943.   2175.  54767. 
#> 2  2000 Beans                  -1236.  -1459.    223. 
#> 3  2000 Bovine Meat             1601.   1569.     32.4
#> 4  2000 Cassava and products   -1673.  -1738.     65.2
#> 5  2000 Cereals, Other          1635.   -176.   1811. 
#> 6  2000 Coffee and products    -2009   -2328.    319. 
```
