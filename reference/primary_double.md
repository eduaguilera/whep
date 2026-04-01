# Items with double-counting in production statistics

Identifies production items that appear both as primary crop products
and as harvested-area items, requiring special treatment to avoid
double-counting in production and biomass accounting.

## Usage

``` r
primary_double
```

## Format

A tibble where each row corresponds to one item pair with a
double-counting relationship. It contains the following columns:

- `Item_area`: Name of the item as it appears in harvested-area
  statistics (e.g., `"Seed cotton, unginned"`).

- `item_prod`: Name of the derived production item (e.g.,
  `"Cotton lint, ginned"`, `"Cotton seed"`).

- `item_prod_code`: Numeric FAOSTAT production code of the derived item.

- `Multi_type`: Classification of the double-counting type:

  - `"Primary"`: The area item is the primary crop; product is a direct
    output.

  - `"Primary_area"`: Area is recorded under a primary aggregate crop
    name.

  - `"Multi"`: Multiple products share the same harvested area.

  - `"Multi_area"`: Multiple products share a recorded area aggregate.

## Source

Derived from FAOSTAT production methodology documentation.
