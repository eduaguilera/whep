# The declarative schema of a row-evidence table.

Return the `"whep-row-evidence/1"` contract as a declarative schema,
ready for
[`check_table_schema()`](https://eduaguilera.github.io/whep/reference/check_table_schema.md)
or
[`assert_table_schema()`](https://eduaguilera.github.io/whep/reference/assert_table_schema.md).
It is plain data, so it can be serialized next to an evidence artifact
and used by a consumer that does not load this package.

The schema is closed (`extra_columns = "forbid"`) and ordered
(`column_order = "strict"`): an evidence table with an extra column or a
permuted column order is not this format, because a consumer rebuilding
`row_key` from `key_columns` relies on both.

## Usage

``` r
row_evidence_schema()
```

## Value

A schema list, as documented in
[`check_table_schema()`](https://eduaguilera.github.io/whep/reference/check_table_schema.md).

## Examples

``` r
row_evidence_schema()
#> $columns
#> $columns[[1]]
#> $columns[[1]]$name
#> [1] "row_key"
#> 
#> $columns[[1]]$type
#> [1] "character"
#> 
#> $columns[[1]]$allow_missing
#> [1] FALSE
#> 
#> 
#> $columns[[2]]
#> $columns[[2]]$name
#> [1] "key_columns"
#> 
#> $columns[[2]]$type
#> [1] "character"
#> 
#> $columns[[2]]$allow_missing
#> [1] FALSE
#> 
#> 
#> $columns[[3]]
#> $columns[[3]]$name
#> [1] "source_id"
#> 
#> $columns[[3]]$type
#> [1] "character"
#> 
#> $columns[[3]]$allow_missing
#> [1] FALSE
#> 
#> 
#> $columns[[4]]
#> $columns[[4]]$name
#> [1] "source_version"
#> 
#> $columns[[4]]$type
#> [1] "character"
#> 
#> 
#> $columns[[5]]
#> $columns[[5]]$name
#> [1] "recorded_at"
#> 
#> $columns[[5]]$type
#> [1] "character"
#> 
#> $columns[[5]]$allow_missing
#> [1] FALSE
#> 
#> 
#> $columns[[6]]
#> $columns[[6]]$name
#> [1] "field"
#> 
#> $columns[[6]]$type
#> [1] "character"
#> 
#> $columns[[6]]$allow_missing
#> [1] FALSE
#> 
#> 
#> $columns[[7]]
#> $columns[[7]]$name
#> [1] "value"
#> 
#> $columns[[7]]$type
#> [1] "character"
#> 
#> 
#> 
#> $extra_columns
#> [1] "forbid"
#> 
#> $column_order
#> [1] "strict"
#> 

# The contract is data, so it survives serialization unchanged.
identical(
  yaml::yaml.load(yaml::as.yaml(row_evidence_schema())),
  row_evidence_schema()
)
#> [1] TRUE
```
