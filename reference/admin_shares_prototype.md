# The admin-shares zero-row prototype

The zero-row tibble
[`admin_shares_schema()`](https://eduaguilera.github.io/whep/reference/admin_shares_schema.md)
describes: the declared columns, in the declared order, each with the
declared type. Built with
[`empty_table_from_schema()`](https://eduaguilera.github.io/whep/reference/empty_table_from_schema.md),
so it passes
[`check_table_schema()`](https://eduaguilera.github.io/whep/reference/check_table_schema.md)
by construction and cannot drift from the schema. See
[`admin_shares_schema()`](https://eduaguilera.github.io/whep/reference/admin_shares_schema.md)
for the full column-by-column contract.

## Usage

``` r
admin_shares_prototype()
```

## Value

A zero-row tibble with the admin-shares contract's columns, in contract
order.

## Examples

``` r
admin_shares_prototype()
#> # A tibble: 0 × 22
#> # ℹ 22 variables: area_code <int>, level_polity_code <chr>, level <int>,
#> #   item_prod_code <int>, species_group <chr>, indicator_used <chr>,
#> #   year <int>, value <dbl>, share <dbl>, source <chr>, tier <int>,
#> #   grain <chr>, concept_break <lgl>, nuts_version <chr>,
#> #   source_native_id <chr>, source_native_name <chr>, source_id <chr>,
#> #   source_version <chr>, recorded_at <chr>, treatment_year <chr>,
#> #   treatment_value <chr>, value_flag <chr>
nrow(check_table_schema(admin_shares_prototype(), admin_shares_schema()))
#> [1] 0
```
