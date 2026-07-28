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
