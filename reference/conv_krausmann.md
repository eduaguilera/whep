# Krausmann per head feed intake.

Annual feed intake (kilograms dry matter per head per year) for draft
and non productive species that lack a product based feed conversion
ratio, from Krausmann et al. (2013). Migrated from the afsetools
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

  Feed intake (kg DM per head per year).

## Source

Krausmann et al. (2013), via afsetools `Codes_coefs.xlsx`.
