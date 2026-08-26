# Check a table against a declarative schema.

Validate a tibble against a serializable schema and return one row of
diagnostics per violation. The input is never modified, so this is the
read-only counterpart of
[`ensure_columns()`](https://eduaguilera.github.io/whep/reference/ensure_columns.md),
which *coerces* a table to a typed prototype. Use
[`ensure_columns()`](https://eduaguilera.github.io/whep/reference/ensure_columns.md)
to reach a schema and `check_table_schema()` to prove a table is already
there.

The schema is plain data — nested lists of scalars and atomic vectors —
so it round-trips through `yaml` or `jsonlite` unchanged and can live
next to the artifact it describes. Project vocabularies and scientific
bounds stay in the caller's schema; this function hard-codes none of
them.

## Usage

``` r
check_table_schema(data, schema)
```

## Arguments

- data:

  Table to validate. Not modified.

- schema:

  Declarative schema, as described above.

## Value

A tibble of diagnostics, empty when `data` conforms.

## Schema representation

A list with these fields, all optional except `columns`:

- `columns`: An *ordered* list of column specifications (see below). A
  list, not a named list, so the declared order survives any
  serialization. May be empty.

- `key`: Character vector of column names that must jointly be unique.
  Every name must be declared in `columns`.

- `extra_columns`: `"allow"` (default) or `"forbid"` for columns the
  schema does not declare.

- `column_order`: `"ignore"` (default) or `"strict"`. Under `"strict"`
  the declared columns that are present must appear in the declared
  relative order.

- `allow_empty`: Whether a zero-row table is acceptable. `TRUE` by
  default; set `FALSE` to flag an empty result.

Each column specification is a list with:

- `name`: Column name. Required.

- `type`: One of `"logical"`, `"integer"`, `"double"`, `"character"`,
  `"Date"`, `"list"` or `"any"`. Required. Types are compared exactly
  ([`vctrs::vec_is()`](https://vctrs.r-lib.org/reference/vec_assert.html)):
  an `integer` column does not satisfy `"double"`. `"any"` skips the
  type check.

- `required`: Whether the column must be present. `TRUE` by default.

- `allow_missing`: Whether `NA` is acceptable. `TRUE` by default.

- `min`, `max`: Inclusive bounds, for `"integer"`, `"double"` and
  `"Date"` only.

- `allowed`: Permitted values, i.e. a caller-owned vocabulary.

- `unique`: Whether values must be unique within the column. `FALSE` by
  default.

- `severity`: `"error"` (default) or `"warning"` for every diagnostic
  attributed to this column.

`min`, `max`, `allowed`, `unique` and `allow_missing = FALSE` do not
apply to `"list"` or `"any"` columns. Unknown fields, at either level,
abort rather than being ignored, so a mistyped `minimum` cannot silently
disable a bound.

## Diagnostics

One row per violation, ordered deterministically: table-scope rules
first, then each declared column in schema order (by row, then rule),
then undeclared columns in input order, then key duplicates. Columns:

- `row`: Row index in `data`, `NA` for table- and column-scope rules.

- `column`: Column name, `NA` for table-scope rules.

- `rule`: One of `"empty_table"`, `"column_order"`, `"missing_column"`,
  `"unexpected_column"`, `"type_mismatch"`, `"missing_value"`,
  `"below_min"`, `"above_max"`, `"not_allowed"`, `"duplicate_value"`,
  `"duplicate_key"`.

- `value`: The offending value, formatted, `NA` where no single value is
  at fault.

- `severity`: `"error"` or `"warning"`, from the column specification.
  Table-scope rules are always `"error"`.

- `detail`: Human-readable context.

A column whose type does not match reports `type_mismatch` and its value
rules are skipped, so a wrongly typed column yields one diagnostic
rather than one per row.

## Examples

``` r
# A keyed long table with a vocabulary and a scientific bound.
schema <- list(
  columns = list(
    list(name = "year", type = "integer", min = 1961, max = 2023),
    list(name = "area_code", type = "integer", allow_missing = FALSE),
    list(
      name = "source",
      type = "character",
      allowed = c("FAOSTAT_prod", "LUH2")
    ),
    list(name = "value", type = "double", min = 0)
  ),
  key = c("year", "area_code")
)
data <- tibble::tibble(
  year = c(2000L, 2000L, 1900L),
  area_code = c(4L, 4L, 8L),
  source = c("FAOSTAT_prod", "guess", "LUH2"),
  value = c(1, -2, 3)
)
check_table_schema(data, schema)
#> # A tibble: 5 × 6
#>     row column rule          value    severity detail                           
#>   <int> <chr>  <chr>         <chr>    <chr>    <chr>                            
#> 1     3 year   below_min     1900     error    min is 1961                      
#> 2     2 source not_allowed   guess    error    allowed: FAOSTAT_prod, LUH2      
#> 3     2 value  below_min     -2       error    min is 0                         
#> 4     1 NA     duplicate_key 2000 | 4 error    key (year, area_code) must be un…
#> 5     2 NA     duplicate_key 2000 | 4 error    key (year, area_code) must be un…

# The schema is data: it survives a YAML round trip unchanged.
identical(
  check_table_schema(data, yaml::yaml.load(yaml::as.yaml(schema))),
  check_table_schema(data, schema)
)
#> [1] TRUE

# A structurally different schema: closed column set, strict order,
# no key, a non-empty requirement and a list column.
manifest_schema <- list(
  columns = list(
    list(name = "built_at", type = "Date"),
    list(name = "inputs", type = "list")
  ),
  extra_columns = "forbid",
  column_order = "strict",
  allow_empty = FALSE
)
check_table_schema(tibble::tibble(inputs = list()), manifest_schema)
#> # A tibble: 2 × 6
#>     row column   rule           value severity detail                           
#>   <int> <chr>    <chr>          <chr> <chr>    <chr>                            
#> 1    NA NA       empty_table    NA    error    the schema requires at least one…
#> 2    NA built_at missing_column NA    error    the schema requires a Date column
```
