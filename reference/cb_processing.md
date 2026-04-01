# Commodity balance sheet processing fractions

Specifies the product fractions obtained when CBS items are processed,
linking processed items to their output CBS categories.

## Usage

``` r
cb_processing
```

## Format

A tibble where each row corresponds to one processed-item /
output-category combination. It contains the following columns:

- `ProcessedItem`: Name of the CBS item being processed (e.g.,
  `"Apples and products"`, `"Barley and products"`).

- `item_cbs`: Name of the output CBS category produced by processing
  (e.g., `"Alcohol, Non-Food"`).

- `Product_fraction`: Fraction of the processed item that yields the
  output product (numeric, 0–1).

- `Value_fraction`: Economic value fraction associated with the output
  product (numeric; largely `NA` in current data).

- `Required`: Reserved column, currently all `NA`.

## Source

Derived from FAOSTAT commodity balance sheet processing assumptions.
