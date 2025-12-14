# Commodity balance sheet items

Defines name/code correspondences for commodity balance sheet (CBS)
items.

## Usage

``` r
items_cbs
```

## Format

A tibble where each row corresponds to one CBS item. It contains the
following columns:

- `item_cbs_code`: A numeric code used to refer to the CBS item.

- `item_cbs_name`: A natural language name for the item.

- `item_type`: An ad-hoc grouping of items. This is a work in progress
  evolving depending on our needs, so for now it only has two possible
  values:

  - `livestock`: The CBS item represents a live animal.

  - `other`: Not any of the previous groups.

## Source

Inspired by [FAOSTAT data](https://www.fao.org/faostat/en/#data/FBS).
