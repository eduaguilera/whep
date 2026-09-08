# Read the polycell support table from its registered pin

Resolves the versioned `polycell_support` input (DA-17), preferring a
local parquet named by `Sys.getenv("WHEP_POLYCELL_SUPPORT_PATH")` so a
development build can be used before it is published.

## Usage

``` r
read_polycell_support(
  path = NULL,
  version = NULL,
  require_layers = TRUE,
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

- require_layers:

  Whether to refuse a support built without its inland water and ice
  layers. `TRUE` (default) aborts with class
  `whep_polycell_absent_layers`; see *Refusing a zero-filled support*.
  `FALSE` returns the table anyway, for a caller that needs the
  territory and not the land/water/ice split and says so.

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

## Refusing a zero-filled support

`water` and `ice` are optional arguments of
[`build_polycell_support()`](https://eduaguilera.github.io/whep/reference/build_polycell_support.md)
and zero-fill when absent, so a support can be published with every
lake, river and glacier inside a polity booked as land. Nothing in the
table's own arithmetic can see that: the identity
`polity_area_ha == land_area_ha + inland_water_ha + ice_area_ha` holds
to `max |residual| = 0 ha` either way, because zero satisfies it. It has
happened twice – `20260818T105426Z-a0330` (whep#885) and
`20260827T190201Z-f82a2` (whep#1010) – and on the second, 2015 land was
534.9 Mha (+4.1%) too high across 94.3% of the `(cell, area_code)`
groups the gridded carbon path reads.

So this reader checks, and **aborts** rather than warning. Both times
the defect reached a pin there was a warning to see: whep#885 added
`cli_warn()` to the producer's zero-fill branch, and
`verify_polycell_support.R` prints one when the layer's environment
variable is unset. A warning inside a build that runs for hours is not a
gate. Every consumer of this table divides a nutrient or carbon mass by
land area, so a 4% error in the denominator is not a caveat to carry
forward.

What is checked is whether the layers were **supplied**, never whether
the totals reconcile. A table built since whep#1010 carries
`layers_supplied`, and the check is an equality on that label. On one
published before it, the fallback is that neither `inland_water_ha` nor
`ice_area_ha` is identically zero – scale-free, so it holds on a
single-country development build as well as on the global pin, and a
global zero is not a plausible measurement of either quantity.

A support table may carry a second, **non-partitioning** layer: the
aggregate polities of
[`build_polycell_support()`](https://eduaguilera.github.io/whep/reference/build_polycell_support.md)`(aggregates = "overlap_layer")`,
whose polygons cover their members' and therefore claim ground twice.
This returns the partition alone unless asked otherwise, so a consumer
that never heard of the layer cannot pick a row of it up by accident.

## Examples

``` r
# Requires WHEP_POLYCELL_SUPPORT_PATH or a published pin; not run without it.
if (nzchar(Sys.getenv("WHEP_POLYCELL_SUPPORT_PATH"))) {
  read_polycell_support()
}
```
