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

## Examples

``` r
# Minimal stand-ins for `create_n_prov_destiny()` and
# `create_n_soil_inputs()`, carrying only the columns the classification
# reads. One province of each of three categories: Lugo is dominated by
# grassland N, Sevilla by synthetic N on herbaceous production, and Huelva
# by synthetic N where the local production is woody (acorns).
prod_destiny <- tibble::tribble(
  ~year, ~province_name, ~box, ~item, ~origin, ~destiny, ~mg_n,
  2000, "Lugo", "semi_natural_agroecosystems", "Grassland",
  "semi_natural_agroecosystems", "livestock_rum", 900,
  2000, "Sevilla", "semi_natural_agroecosystems", "Grassland",
  "semi_natural_agroecosystems", "livestock_rum", 100,
  2000, "Huelva", "semi_natural_agroecosystems", "Grassland",
  "semi_natural_agroecosystems", "livestock_rum", 30,
  2000, "Huelva", "semi_natural_agroecosystems", "Acorns",
  "semi_natural_agroecosystems", "export", 200,
  2000, "Lugo", "Cropland", "Maize and products",
  "Outside", "livestock_mono", 20,
  2000, "Sevilla", "Cropland", "Maize and products",
  "Outside", "livestock_mono", 50,
  2000, "Huelva", "Cropland", "Maize and products",
  "Outside", "livestock_mono", 10
)

soil_inputs <- tibble::tribble(
  ~year, ~province_name, ~synthetic,
  2000, "Lugo", 10,
  2000, "Sevilla", 400,
  2000, "Huelva", 150
)

create_alfredos_typologies(
  soil_inputs = soil_inputs,
  prod_destiny = prod_destiny,
  years = 2000
)
#> # A tibble: 3 × 9
#>    year province_name grass_N fertiliser_N feed_import_N woody herbaceous
#>   <dbl> <chr>           <dbl>        <dbl>         <dbl> <dbl>      <dbl>
#> 1  2000 Huelva             30          150            10   200         30
#> 2  2000 Lugo              900           10            20     0        900
#> 3  2000 Sevilla           100          400            50     0        100
#> # ℹ 2 more variables: woody_share <dbl>, Category <chr>
```
