# Eurostat crop classification codes

Maps Eurostat crop codes to their full crop category names, used when
integrating Eurostat agricultural statistics.

## Usage

``` r
crops_eurostat
```

## Format

A tibble where each row corresponds to one Eurostat crop category. It
contains the following columns:

- `Crop`: Eurostat crop code (e.g., `"G0000"`, `"G1000"`).

- `Name_Eurostat`: Full name of the crop category as used in Eurostat
  (e.g., `"Plants harvested green from arable land"`,
  `"Temporary grasses and grazings"`).

## Source

[Eurostat Agricultural
Statistics](https://ec.europa.eu/eurostat/statistics-explained/index.php/Agricultural_statistics).
