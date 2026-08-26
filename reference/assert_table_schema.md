# Assert that a table conforms to a declarative schema.

Build-time gate over a tabular artifact. Aborts when
[`check_table_schema()`](https://eduaguilera.github.io/whep/reference/check_table_schema.md)
reports any `"error"` diagnostic and warns when it reports only
`"warning"` ones, so a table that has silently lost a column, changed
type or gained duplicate keys fails at the moment it is produced instead
of downstream. Returns its input, so it can sit inside a pipeline
without changing the value that flows through.

## Usage

``` r
assert_table_schema(data, schema, arg = "data")
```

## Arguments

- data:

  Table to validate. Not modified.

- schema:

  Declarative schema, as described above.

- arg:

  Name of the validated object, used in messages.

## Value

Invisibly, `data`, unchanged. Called for its side effect of aborting on
violation.

## Examples

``` r
schema <- list(
  columns = list(
    list(name = "year", type = "integer"),
    list(name = "value", type = "double", min = 0)
  ),
  key = "year"
)
data <- tibble::tibble(year = c(2000L, 2001L), value = c(1, 2))
assert_table_schema(data, schema)
```
