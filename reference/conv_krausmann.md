# Krausmann per head feed intake.

Annual feed intake (tonnes dry matter per head per year) for draft and
non productive species that lack a product based feed conversion ratio,
from Krausmann et al. (2013). Migrated from the afsetools
`Codes_coefs.xlsx` workbook.

## Usage

``` r
conv_krausmann
```

## Format

A tibble with one row per species:

- item_cbs_code:

  FAOSTAT commodity balance item code.

- species:

  Species name (Horses, Asses, Mules, Camels and so on).

- conversion:

  Feed intake (t DM per head per year).

## Source

Krausmann et al. (2013), via afsetools `Codes_coefs.xlsx`.

## Details

The unit is tonnes, not kilograms: the feed demand builder multiplies
FAOSTAT head counts directly by `conversion` and books the product in
the same column as the product based path (tonnes of product times a
dimensionless feed conversion ratio), which downstream is named
`demand_dm_t`. A horse at 3.65 t DM per year eats 10 kg DM a day; read
as kilograms it would be 10 g. The workbook's `original_value` column
(kept in `inst/extdata/feed/conv_krausmann.csv`) reads the same way: its
values are round daily intakes of 10, 6, 5 and 0.1 kg DM times 365 /
1000. Which table of Krausmann et al. (2013) the values come from, and
why seven of the eight `conversion` values (all but horses) differ from
`original_value`, is unverified.
