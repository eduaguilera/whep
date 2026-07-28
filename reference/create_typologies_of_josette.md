# Typologies of Josette

Typologies of provinces in Spain based on nitrogen (N) production data
of crops and livestock, considering multiple data inputs and producing
classification maps and data frames.

## Usage

``` r
create_typologies_of_josette(
  make_map = TRUE,
  shapefile_path = NULL,
  map_year = 1980
)
```

## Arguments

- make_map:

  If TRUE a map of the typologies will be created.

- shapefile_path:

  Optional path to a Natural Earth 10m admin-1 states/provinces
  shapefile. When `NULL` (default) the layer is downloaded from
  <https://www.naturalearthdata.com> on first use and cached locally;
  set `options(whep.provinces_shapefile = )` to point at an existing
  copy instead.

- map_year:

  The year for which the typology map is created.

## Value

A tibble with the typology classification per year and province.
