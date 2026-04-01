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
