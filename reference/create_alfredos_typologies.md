# Alfredo's typology classification

Calculates typologies for provinces based on grassland, fertilizer,
imported feed, and woody/herbaceous shares.

## Usage

``` r
create_alfredos_typologies(
  soil_inputs = NULL,
  prod_destiny = NULL,
  years = 1860:2020
)
```

## Arguments

- soil_inputs:

  A data frame containing soil nitrogen inputs.

- prod_destiny:

  A data frame containing production and destiny data.

- years:

  Years between 1860 and 2020.

## Value

A data frame with the columns: Year, Province_name, grass_N,
fertilizer_N, feed_import_N, woody, herbaceous, woody_share, and
Category.
