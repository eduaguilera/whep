# Build a typed zero-row table from a declarative schema.

Turn the same serializable schema
[`check_table_schema()`](https://eduaguilera.github.io/whep/reference/check_table_schema.md)
validates into the zero-row tibble it describes: the declared column
names, in the declared order, each with the declared type and no rows.
The result passes
[`check_table_schema()`](https://eduaguilera.github.io/whep/reference/check_table_schema.md)
by construction, because both functions resolve the schema through one
parser, so a prototype and the validator that judges it cannot drift
apart.

This is the missing half of
[`ensure_columns()`](https://eduaguilera.github.io/whep/reference/ensure_columns.md),
which *needs* a zero-row prototype tibble and cannot be handed a schema.
Declare the schema once as data — in a YAML file beside the artifact,
say — then `empty_table_from_schema()` for the prototype,
[`ensure_columns()`](https://eduaguilera.github.io/whep/reference/ensure_columns.md)
to coerce a table onto it, and
[`assert_table_schema()`](https://eduaguilera.github.io/whep/reference/assert_table_schema.md)
to prove the result.

## Usage

``` r
empty_table_from_schema(schema)
```

## Arguments

- schema:

  Declarative schema, as described above.

## Value

A zero-row tibble with one column per declared column, in declared
order. A schema declaring no columns yields a 0x0 tibble.

## Types

`"logical"`, `"integer"`, `"double"`, `"character"`, `"Date"` and
`"list"` produce a column of exactly that type. `"any"` declares that
the schema does not constrain the type, so there is no type to build:
the column is created as
[`logical()`](https://rdrr.io/r/base/logical.html), the type of a bare
`NA` and the one any later cast widens from. Fields other than `name`
and `type` (`min`, `allowed`, `key`, ...) constrain values, of which a
zero-row table has none, so they only have to parse.

A schema with `allow_empty = FALSE` has no valid empty table, and is
rejected rather than returning one that fails its own validation.

## Examples

``` r
schema <- list(
  columns = list(
    list(name = "year", type = "integer", min = 1961, max = 2023),
    list(name = "area_code", type = "integer"),
    list(name = "source", type = "character"),
    list(name = "value", type = "double", min = 0)
  ),
  key = c("year", "area_code")
)
prototype <- empty_table_from_schema(schema)
prototype
#> # A tibble: 0 × 4
#> # ℹ 4 variables: year <int>, area_code <int>, source <chr>, value <dbl>

# It is a prototype: `ensure_columns()` coerces a partial table onto it.
ensure_columns(tibble::tibble(year = 2020L, value = 1.5), prototype)
#> # A tibble: 1 × 4
#>    year area_code source value
#>   <int>     <int> <chr>  <dbl>
#> 1  2020        NA NA       1.5

# And it conforms to the schema it was built from.
nrow(check_table_schema(prototype, schema))
#> [1] 0
```
