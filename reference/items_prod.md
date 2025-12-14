# Primary production items

Defines name/code correspondences for production items.

## Usage

``` r
items_prod
```

## Format

A tibble where each row corresponds to one production item. It contains
the following columns:

- `item_prod_code`: A numeric code used to refer to the item.

- `item_prod_name`: A natural language name for the item.

- `item_type`: An ad-hoc grouping of items. This is a work in progress
  evolving depending on our needs, so for now it only has two possible
  values:

  - `crop_product`: The CBS item represents a crop product.

  - `other`: Not any of the previous groups.

## Source

Inspired by [FAOSTAT data](https://www.fao.org/faostat/en/#data/QCL).
