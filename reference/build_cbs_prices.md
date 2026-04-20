# Build CBS item prices

Compute prices for all commodity balance sheet items, including
processed products and crop residues. Prices are derived from trade
data, with special handling for items without direct trade prices (palm
kernels, soy hulls, brans, etc.). Crop residue prices are estimated as a
fraction of the product price.

## Usage

``` r
build_cbs_prices(
  cbs,
  trade_prices = NULL,
  residue_price_factor = 0.1,
  example = FALSE
)
```

## Arguments

- cbs:

  A tibble of commodity balance sheets, as returned by
  [`build_commodity_balances()`](https://eduaguilera.github.io/whep/reference/build_commodity_balances.md)
  or
  [`get_wide_cbs()`](https://eduaguilera.github.io/whep/reference/get_wide_cbs.md).

- trade_prices:

  A tibble as returned by
  [`build_trade_prices()`](https://eduaguilera.github.io/whep/reference/build_trade_prices.md).
  If `NULL`, it is computed internally.

- residue_price_factor:

  Numeric. Relative price of crop residues compared to the product.
  Default `0.1`.

- example:

  Logical. If `TRUE`, return a small example tibble. Default `FALSE`.

## Value

A tibble with columns:

- `year`: Integer year.

- `element`: `"import"` or `"export"`.

- `item_cbs_code`: Numeric CBS item code.

- `price`: Price in KDollars per tonne.

## Examples

``` r
build_cbs_prices(example = TRUE)
#> # A tibble: 10 × 4
#>     year element item_cbs_code price
#>    <int> <chr>           <int> <dbl>
#>  1  2010 export           2511 0.292
#>  2  2010 import           2511 0.295
#>  3  2015 export           2807 0.422
#>  4  2015 import           2807 0.426
#>  5  2010 export           2536 0.255
#>  6  2010 import           2536 0.25 
#>  7  2018 export           2555 0.347
#>  8  2018 import           2555 0.344
#>  9  2015 export           2105 0.029
#> 10  2015 import           2105 0.03 
```
