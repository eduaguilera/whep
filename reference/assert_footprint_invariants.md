# Assert that footprint conservation invariants hold.

Build-time gate over a footprint result, intended for use in a pipeline
or regression test. Aborts when the footprint over-traces any origin
sector (embodied pressure exceeds the source, which should never happen)
or when the global share of untraced pressure exceeds `max_rel_loss`. A
regression that silently loses or inflates pressure then fails loudly
instead of shipping.

## Usage

``` r
assert_footprint_invariants(
  footprint,
  extensions,
  labels,
  x_vec,
  max_rel_loss = 0.05
)
```

## Arguments

- footprint:

  Footprint tibble from
  [`compute_footprint()`](https://eduaguilera.github.io/whep/reference/compute_footprint.md),
  with `origin_area`, `origin_item` and `value` columns.

- extensions:

  Numeric vector of environmental extensions per sector, as passed to
  [`compute_footprint()`](https://eduaguilera.github.io/whep/reference/compute_footprint.md).

- labels:

  Tibble with `area_code` and `item_cbs_code` mapping each sector to its
  meaning, as passed to
  [`compute_footprint()`](https://eduaguilera.github.io/whep/reference/compute_footprint.md).

- x_vec:

  Numeric vector of total output per sector.

- max_rel_loss:

  Maximum tolerated global relative under-tracing (pressure that never
  reaches final demand). The engine's negative-zeroing and column
  capping cause a small amount, so this is non-zero by default.

## Value

Invisibly, the one-row summary from
[`summarise_conservation()`](https://eduaguilera.github.io/whep/reference/summarise_conservation.md).
Called for its side effect of aborting on violation.

## Examples

``` r
z_mat <- matrix(c(0, 5, 10, 0), nrow = 2)
x_vec <- c(100, 200)
y_mat <- matrix(c(85, 195), ncol = 1)
extensions <- c(50, 30)
labels <- tibble::tibble(
  area_code = c(1L, 1L),
  item_cbs_code = c(1L, 2L)
)
fp <- compute_footprint(
  x_vec = x_vec, y_mat = y_mat, extensions = extensions,
  labels = labels, z_mat = z_mat
)
#> ℹ Computing footprint for 2 sectors.
#>   2 sectors have non-zero extensions.
#>   Final demand: 1 column.
#> Sparse solve path (no dense Leontief inverse).
#> Computing footprints...
#> ✔ Footprint complete: 2 non-zero flows.
assert_footprint_invariants(fp, extensions, labels, x_vec)
#> ✔ Footprint invariants hold (1/2 sectors ok).
```
