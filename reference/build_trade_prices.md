# Build global trade prices

Compute global prices of traded items from FAOSTAT trade data. For each
item and element (import/export), the price is `KDollars / tonnes`
aggregated across all countries.

## Usage

``` r
build_trade_prices(raw_trade = NULL, example = FALSE)
```

## Arguments

- raw_trade:

  A data.table or tibble of FAOSTAT bilateral trade data with columns
  `year`, `item_trade`, `item_code_trade`, `unit`, `element`, and
  `value`. Must include both quantity (`"tonnes"`) and value
  (`"1000 US$"`) rows. If `NULL` (default), the data is read from the
  `"faostat-trade-bilateral"` pin.

- example:

  Logical. If `TRUE`, return a small example tibble. Default `FALSE`.

## Value

A tibble with columns:

- `year`: Integer year.

- `item_trade`: Trade item name.

- `item_code_trade`: Numeric FAOSTAT trade item code.

- `element`: `"import"` or `"export"`.

- `kdollars`: Total trade value in thousand US dollars.

- `tonnes`: Total trade quantity in tonnes.

- `price`: Price in KDollars per tonne.

## Examples

``` r
build_trade_prices(example = TRUE)
#> # A tibble: 10 × 7
#>     year item_trade item_code_trade element kdollars    tonnes price
#>    <int> <chr>                <int> <chr>      <dbl>     <dbl> <dbl>
#>  1  2010 Wheat                   15 export  35000000 120000000 0.292
#>  2  2010 Wheat                   15 import  38000000 130000000 0.292
#>  3  2015 Rice                    31 export  19000000  45000000 0.422
#>  4  2015 Rice                    31 import  20000000  47000000 0.426
#>  5  2010 Maize                   56 export  28000000 110000000 0.255
#>  6  2010 Maize                   56 import  30000000 120000000 0.25 
#>  7  2018 Soybeans               236 export  52000000 150000000 0.347
#>  8  2018 Soybeans               236 import  55000000 160000000 0.344
#>  9  2015 Sugar                  162 export  11000000  55000000 0.2  
#> 10  2015 Sugar                  162 import  12000000  58000000 0.207
```
