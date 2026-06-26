# Check footprint conservation against direct extensions.

Verify the master input-output accounting identity for a footprint: the
environmental pressure embodied across all final demand and traced back
to an origin sector should equal the direct extension (the source
pressure) of that sector.

The footprint engine zeroes negative coefficients, caps column sums, and
drops near-zero-output sectors (the FABIO conventions), so the identity
holds only approximately. This check quantifies the discrepancy per
origin sector instead of asserting exact equality. Crucially it detects
*under-tracing* (pressure that silently disappears and never reaches
final demand), which
[`compute_footprint()`](https://eduaguilera.github.io/whep/reference/compute_footprint.md)'s
`conserve_extensions` bounding never reports because it only rescales
results downward.

Only the positive side of the extensions is traced, matching the engine,
which traces `pmax(extensions, 0)` and ignores sectors with output
`<= output_tol`.

## Usage

``` r
check_footprint_conservation(
  footprint,
  extensions,
  labels,
  x_vec,
  output_tol = 1e-08,
  tol = 0.01
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

- output_tol:

  Minimum output for a sector to be traceable. Sectors with
  `x_vec <= output_tol` contribute zero direct pressure, matching
  [`compute_footprint()`](https://eduaguilera.github.io/whep/reference/compute_footprint.md).

- tol:

  Relative tolerance for the conservation status. Discrepancies within
  `tol` of the direct pressure are `"ok"`.

## Value

A tibble with one row per origin sector:

- `origin_area`: Country where the pressure occurs.

- `origin_item`: Item causing the pressure.

- `direct`: Direct (source) extension for the sector.

- `embodied`: Footprint traced back to the sector.

- `discrepancy`: `embodied - direct`.

- `rel_discrepancy`: `discrepancy / direct` (`NA` when `direct` is
  zero).

- `status`: One of `"ok"`, `"under_traced"`, `"dropped"` (embodied is
  zero while direct is positive) or `"over_traced"`. Rows are ordered by
  descending absolute relative discrepancy.

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
check_footprint_conservation(fp, extensions, labels, x_vec)
#> # A tibble: 2 × 7
#>   origin_area origin_item direct embodied discrepancy rel_discrepancy status    
#>         <int>       <int>  <dbl>    <dbl>       <dbl>           <dbl> <chr>     
#> 1           1           1     50     47.5     -2.51          -0.0501  under_tra…
#> 2           1           2     30     30.0     -0.0376        -0.00125 ok        
```
