# CBS to trade item code mapping

Maps detailed FAOSTAT trade item codes to their corresponding CBS item
categories, enabling aggregation of bilateral trade data into the CBS
framework.

## Usage

``` r
cbs_trade_codes
```

## Format

A tibble where each row corresponds to one trade item. It contains the
following columns:

- `item_code_trade`: Numeric FAOSTAT trade item code (e.g., `15` for
  wheat).

- `item_trade`: Name of the trade item (e.g., `"Wheat"`,
  `"Flour, wheat"`, `"Bran, wheat"`).

- `item_cbs`: Name of the CBS category this trade item belongs to (e.g.,
  `"Wheat and products"`).

- `item_check`: Cross-validation column repeating the mapped CBS name;
  used to flag mapping inconsistencies during data processing.

## Source

Derived from [FAOSTAT Detailed Trade
Matrix](https://www.fao.org/faostat/en/#data/TM) and commodity balance
sheet correspondence tables.

## Examples

``` r
head(cbs_trade_codes)
#> # A tibble: 6 × 4
#>   item_code_trade item_trade   item_cbs           item_check        
#>             <dbl> <chr>        <chr>              <chr>             
#> 1              15 Wheat        Wheat and products Wheat and products
#> 2              16 Flour, wheat Wheat and products Wheat and products
#> 3              17 Bran, wheat  Wheat and products Wheat and products
#> 4              18 Macaroni     Wheat and products Wheat and products
#> 5              19 Germ, wheat  Wheat and products Wheat and products
#> 6              20 Bread        Wheat and products Wheat and products
```
