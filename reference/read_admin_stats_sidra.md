# Read Brazilian state agricultural statistics from IBGE SIDRA

Read one IBGE SIDRA table of Brazilian state-level (`admin1`)
agricultural statistics through the unauthenticated SIDRA values API and
return it in the shared admin-statistics reader shape: one row per
source-native unit, item, indicator and year, in WHEP units.

Three tables are served, all annual from 1974:

- `"5457"` – Producao Agricola Municipal (PAM) crop areas and
  production. Harvested area (`indicator_used == "area_harvested"`) is
  the indicator the allocation binds on; planted-or-sown area
  (`"area_planted_or_sown"`) is a flagged proxy SIDRA publishes only
  from 1988, and production (`"production"`) is the last-resort
  indicator.

- `"3939"` – Pesquisa da Pecuaria Municipal (PPM) herd inventories, in
  head. Livestock rows carry `quantity == "heads"` and
  `indicator_used == NA`, head counts not being one of the
  area/production/yield indicators
  [`admin_shares_schema()`](https://eduaguilera.github.io/whep/reference/admin_shares_schema.md)
  closes over.

- `"94"` – milked cows, in head, the dairy split PPM table 3939 does not
  report.

Rows are **source-native**: unit and item identifiers are returned
exactly as SIDRA served them and are never mapped to `item_prod_code` or
resolved to a polity here. Values are returned in WHEP units with a
conversion factor of 1 throughout, SIDRA serving hectares, tonnes and
head directly.

Requests are paged by year block so no single query exceeds the API's
50,000-value cap; see `years_per_request`.

## Usage

``` r
read_admin_stats_sidra(
  table = c("5457", "3939", "94"),
  years = NULL,
  items = NULL,
  years_per_request = NULL,
  example = FALSE
)
```

## Source

IBGE SIDRA API (<https://apisidra.ibge.gov.br>), tables 5457 (Producao
Agricola Municipal), 3939 and 94 (Pesquisa da Pecuaria Municipal).
Unauthenticated; verified 2026-09-02.

## Arguments

- table:

  SIDRA table to read, one of `"5457"` (PAM crops), `"3939"` (PPM herds)
  or `"94"` (milked cows).

- years:

  Integer vector of calendar years. `NULL` (the default) requests 1974 –
  the first year all three tables publish – through the current year;
  years the table has not published are absent from the result rather
  than an error.

- items:

  Optional character vector of classification category codes to restrict
  the request to (`c782` codes for table `"5457"`, `c79` codes for
  `"3939"`). `NULL` requests every category and drops the `"0"`
  ("Total") category, which is the all-crops total rather than an item.
  Must be `NULL` for table `"94"`, which has no classification.

- years_per_request:

  Number of years per API request. `NULL` uses the per-table default
  that keeps a request under the 50,000-value cap.

- example:

  If `TRUE`, return a small fixture instead of reading from the API.
  Defaults to `FALSE`.

## Value

A tibble with one row per unit, item, indicator and year:

- `source`: `"IBGE_PAM"` for table 5457, `"IBGE_PPM"` for 3939 and 94.

- `source_native_unit_id`, `source_native_unit_name`: the IBGE UF code
  and name exactly as served.

- `source_native_item_code`, `source_native_item_name`: the
  classification category exactly as served; the code is `NA` for table
  94, whose item is the table itself and whose name is the served
  variable name.

- `indicator_used`: `"area_harvested"`, `"area_planted_or_sown"` or
  `"production"` for crop rows, `NA` for head counts.

- `quantity`: `"area"`, `"production"` or `"heads"`.

- `year`: calendar year.

- `value`: the served value in WHEP units, `NA` where the source gave a
  missing-value code.

- `value_unit`: `"ha"`, `"tonnes"` or `"heads"`.

- `value_flag`: the source's missing-value code verbatim (`"..."`,
  `".."` or `"X"`), `NA` when the row carries a value. An absolute zero
  (`"-"`) is a value, not a flag.

- `grain`: always `"admin1"`; SIDRA level N3 is the state.

- `nuts_version`: always `NA`, Brazil having no NUTS geography.

- `source_version`: always `NA`; SIDRA exposes no vintage stamp.

- `recorded_at`: ISO 8601 UTC fetch time of the request the row came
  from.

## Examples

``` r
read_admin_stats_sidra(example = TRUE)
#> # A tibble: 10 × 15
#>    source   source_native_unit_id source_native_unit_name source_native_item_c…¹
#>    <chr>    <chr>                 <chr>                   <chr>                 
#>  1 IBGE_PAM 17                    Tocantins               40122                 
#>  2 IBGE_PAM 11                    Rondônia                40127                 
#>  3 IBGE_PAM 35                    São Paulo               40122                 
#>  4 IBGE_PAM 35                    São Paulo               40122                 
#>  5 IBGE_PAM 35                    São Paulo               40122                 
#>  6 IBGE_PAM 41                    Paraná                  40127                 
#>  7 IBGE_PAM 43                    Rio Grande do Sul       40102                 
#>  8 IBGE_PPM 43                    Rio Grande do Sul       2677                  
#>  9 IBGE_PPM 35                    São Paulo               32796                 
#> 10 IBGE_PPM 35                    São Paulo               NA                    
#> # ℹ abbreviated name: ¹​source_native_item_code
#> # ℹ 11 more variables: source_native_item_name <chr>, indicator_used <chr>,
#> #   quantity <chr>, year <int>, value <dbl>, value_unit <chr>,
#> #   value_flag <chr>, grain <chr>, nuts_version <chr>, source_version <chr>,
#> #   recorded_at <chr>
```
