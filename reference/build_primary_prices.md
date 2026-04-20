# Build primary item prices

Compute prices for primary production items. Export trade prices are
preferred; when unavailable, production value prices (gross production
value divided by quantity) are used as fallback. Gaps are filled via
linear interpolation.

## Usage

``` r
build_primary_prices(
  primary_prod,
  value_of_production = NULL,
  trade_prices = NULL,
  example = FALSE
)
```

## Arguments

- primary_prod:

  A tibble of primary production, as returned by
  [`build_primary_production()`](https://eduaguilera.github.io/whep/reference/build_primary_production.md)
  or
  [`get_primary_production()`](https://eduaguilera.github.io/whep/reference/get_primary_production.md).

- value_of_production:

  A data frame with FAOSTAT Value of Production data. Must contain
  columns `Item.Code` (or `item_code_prod`), `Element`, `Unit`, `Year`,
  `Value`, `Area.Code` (or `area_code`). If `NULL`, only trade prices
  are used.

- trade_prices:

  A tibble as returned by
  [`build_trade_prices()`](https://eduaguilera.github.io/whep/reference/build_trade_prices.md).
  If `NULL`, it is computed internally.

- example:

  Logical. If `TRUE`, return a small example tibble. Default `FALSE`.

## Value

A tibble with columns:

- `year`: Integer year.

- `item_prod_code`: Numeric production item code.

- `price`: Price in KDollars per tonne.

## Examples

``` r
build_primary_prices(example = TRUE)
#> # A tibble: 10 × 3
#>     year item_prod_code price
#>    <int>          <int> <dbl>
#>  1  2010             15 0.292
#>  2  2015             15 0.21 
#>  3  2010             56 0.255
#>  4  2015             56 0.185
#>  5  2010            236 0.41 
#>  6  2015            236 0.347
#>  7  2010             31 0.395
#>  8  2015             31 0.422
#>  9  2018            406 0.33 
#> 10  2018            486 0.18 
```
