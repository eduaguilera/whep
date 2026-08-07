# Create WHEP typologies for Spain

Calculates all decision variables for the WHEP typology using only
production and consumption data (no import/export).

## Usage

``` r
create_typologies_whep(
  prod_destiny = create_n_prov_destiny(),
  prod_n = dplyr::rename(create_n_production(), production_n = prod),
  years = 2020
)
```

## Arguments

- prod_destiny:

  Tibble with N flows from
  [`create_n_prov_destiny()`](https://eduaguilera.github.io/whep/reference/create_n_prov_destiny.md).

- prod_n:

  Tibble with per-item N production from
  [`create_n_production()`](https://eduaguilera.github.io/whep/reference/create_n_production.md)
  (column `prod`, renamed here to `production_n`).

- years:

  Numeric vector of years to include (default = 2020).

## Value

A data frame with Year, Province_name, decision variables, and Category.

## Examples

``` r
# Minimal stand-ins for the two real inputs, carrying only the columns the
# decision variables read. Lugo feeds its livestock mostly on local grass;
# Barcelona feeds them mostly on imported N.
prod_destiny <- tibble::tribble(
  ~year, ~province_name, ~box, ~item, ~origin, ~destiny, ~mg_n,
  2020, "Lugo", "Cropland", "Wheat and products",
  "Cropland", "population_food", 100,
  2020, "Lugo", "semi_natural_agroecosystems", "Grassland",
  "semi_natural_agroecosystems", "livestock_rum", 800,
  2020, "Lugo", "Cropland", "Maize and products",
  "Cropland", "livestock_rum", 200,
  2020, "Lugo", "Cropland", "Soyabean cake",
  "Outside", "livestock_mono", 50,
  2020, "Barcelona", "Cropland", "Wheat and products",
  "Cropland", "population_food", 100,
  2020, "Barcelona", "Cropland", "Soyabean cake",
  "Outside", "livestock_mono", 900,
  2020, "Barcelona", "Cropland", "Maize and products",
  "Cropland", "livestock_mono", 100
)

prod_n <- tibble::tribble(
  ~year, ~province_name, ~box, ~production_n,
  2020, "Lugo", "Cropland", 300,
  2020, "Barcelona", "Cropland", 200
)

create_typologies_whep(
  prod_destiny = prod_destiny,
  prod_n = prod_n,
  years = 2020
) |>
  dplyr::select(year, province_name, human_share, import_share, Category)
#> # A tibble: 2 × 5
#>    year province_name human_share import_share Category                         
#>   <dbl> <chr>               <dbl>        <dbl> <chr>                            
#> 1  2020 Barcelona           0.5         0.9    Imported feed-based system       
#> 2  2020 Lugo                0.333       0.0476 Local grass-based livestock syst…
```
