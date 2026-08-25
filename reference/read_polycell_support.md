# Read the polycell support table from its registered pin

Resolves the versioned `polycell_support` input (DA-17), preferring a
local parquet named by `Sys.getenv("WHEP_POLYCELL_SUPPORT_PATH")` so a
development build can be used before it is published.

A support table may carry a second, **non-partitioning** layer: the
aggregate polities of
[`build_polycell_support()`](https://eduaguilera.github.io/whep/reference/build_polycell_support.md)`(aggregates = "overlap_layer")`,
whose polygons cover their members' and therefore claim ground twice.
This returns the partition alone unless asked otherwise, so a consumer
that never heard of the layer cannot pick a row of it up by accident.

## Usage

``` r
read_polycell_support(
  path = NULL,
  version = NULL,
  role = c("partition", "overlap", "all")
)
```

## Arguments

- path:

  Optional path to a local parquet, overriding the environment variable
  and the pin.

- version:

  Pin version, passed to
  [`whep_read_file()`](https://eduaguilera.github.io/whep/reference/whep_read_file.md).
  `NULL` takes the version frozen in
  [whep_inputs](https://eduaguilera.github.io/whep/reference/whep_inputs.md).

- role:

  Which layer to return. `"partition"` (default) is the rows that
  partition each cell – every row of a table built with the default
  `aggregates = "exclude"`, and every row of any table published before
  whep#803. `"overlap"` is the aggregate layer alone, for a consumer
  that needs the territory of a reporting bucket whose only polity is an
  aggregate; it aborts rather than returning nothing when the table
  carries no such layer. `"all"` returns both and is only correct where
  the two are kept apart afterwards – summing across them double-counts
  every member an aggregate covers.

## Value

A `tibble` in the
[`build_polycell_support()`](https://eduaguilera.github.io/whep/reference/build_polycell_support.md)
grain.

## Examples

``` r
# Requires WHEP_POLYCELL_SUPPORT_PATH or a published pin; not run without it.
if (nzchar(Sys.getenv("WHEP_POLYCELL_SUPPORT_PATH"))) {
  read_polycell_support()
}
```
