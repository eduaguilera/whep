# Maximum intake shares.

Per livestock category diet share caps that limit how much of a diet a
feed item or feed class may make up. Migrated from the afsetools
`Livestock_coefs.xlsx` workbook.

## Usage

``` r
max_intake_share
```

## Format

A tibble with one row per cap:

- livestock_category:

  Livestock category the cap applies to.

- var:

  Cap key type: item_cbs (per item) or Cat_feed (per feed class).

- var_value:

  Item or feed class the cap applies to.

- max_intake_share:

  Maximum share of the diet (fraction).

## Source

afsetools `Livestock_coefs.xlsx`.
