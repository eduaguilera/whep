# Melt a bilateral trade matrix to long format.

Convert the `bilateral_trade` list-column of
[`get_bilateral_trade()`](https://eduaguilera.github.io/whep/reference/get_bilateral_trade.md)
(one square origin-by-destination matrix per year and item) into the
tidy `from_code`/`to_code` long form consumed by
[`compute_footprint_balance()`](https://eduaguilera.github.io/whep/reference/compute_footprint_balance.md).
Self-trade (diagonal) entries are dropped.

## Usage

``` r
melt_bilateral_trade(bilateral_trade)
```

## Arguments

- bilateral_trade:

  Tibble from
  [`get_bilateral_trade()`](https://eduaguilera.github.io/whep/reference/get_bilateral_trade.md),
  with `year`, `item_cbs_code` and a `bilateral_trade` matrix
  list-column.

## Value

A tibble with `year`, `from_code`, `to_code`, `item_cbs_code` and
`value`.

## Examples

``` r
m <- matrix(
  c(0, 40, 0, 0),
  nrow = 2,
  dimnames = list(c("1", "2"), c("1", "2"))
)
bt <- tibble::tibble(
  year = 2010L, item_cbs_code = 10L, bilateral_trade = list(m)
)
melt_bilateral_trade(bt)
#> # A tibble: 1 × 5
#>    year from_code to_code item_cbs_code value
#>   <int>     <int>   <int>         <int> <dbl>
#> 1  2010         2       1            10    40
```
