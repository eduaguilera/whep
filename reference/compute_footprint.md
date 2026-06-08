# Compute environmental footprints.

Trace environmental extensions through the supply chain using the
Leontief inverse, following the FABIO methodology (Bruckner et al.,
2019). The footprint shows how much of an environmental pressure (e.g.
land use, water, emissions) is embodied in the final consumption of each
product in each country.

The multiplier matrix is computed as \\MP\_{ij} = (e_i / X_i) \cdot
L\_{ij}\\, where \\e_i\\ is the extension for sector \\i\\. For each
demand category, the footprint is decomposed per target item using the
FABIO diagonal approach: \\FP = MP \cdot \text{diag}(y)\\, aggregated by
item.

For large systems, pass `z_mat` and `x_vec` instead of `l_inv`. This
solves \\(I - A) x = Y\\ directly using a sparse LU factorisation,
avoiding the dense Leontief inverse entirely and reducing memory from
\\O(n^2)\\ to \\O(nnz)\\.

## Usage

``` r
compute_footprint(
  l_inv = NULL,
  x_vec,
  y_mat,
  extensions,
  labels,
  z_mat = NULL,
  fd_labels = NULL,
  output_tol = 1e-08,
  value_added_floor = 0.001,
  conserve_extensions = TRUE
)
```

## Arguments

- l_inv:

  Leontief inverse matrix from
  [`compute_leontief_inverse()`](https://eduaguilera.github.io/whep/reference/compute_leontief_inverse.md).
  Ignored when `z_mat` is provided.

- x_vec:

  Numeric vector of total output per sector.

- y_mat:

  Final demand matrix from
  [`build_io_model()`](https://eduaguilera.github.io/whep/reference/build_io_model.md).

- extensions:

  Numeric vector of environmental extensions (e.g. hectares of land use)
  per sector. Must have the same length as `x_vec`.

- labels:

  Tibble with `area_code` and `item_cbs_code` mapping row/column indices
  to their meaning. From
  [`build_io_model()`](https://eduaguilera.github.io/whep/reference/build_io_model.md).

- z_mat:

  Optional inter-industry flow matrix from
  [`build_io_model()`](https://eduaguilera.github.io/whep/reference/build_io_model.md).
  When provided, the system is solved directly (sparse LU), and `l_inv`
  is not needed.

- fd_labels:

  Optional tibble labelling Y columns. Pass `fd_labels[[i]]` from
  [`build_io_model()`](https://eduaguilera.github.io/whep/reference/build_io_model.md)
  output. When provided, footprints are decomposed per target item using
  the FABIO diagonal approach, and the result includes a `target_fd`
  column. When omitted, columns of Y are treated as sectors (appropriate
  only when Y is square).

- output_tol:

  Minimum output considered valid when computing extension intensities.
  Sectors with `x_vec <= output_tol` get zero intensity to avoid
  infinite or numerically explosive footprints from zero-output
  residuals.

- value_added_floor:

  Minimum share of each sector's output that is treated as
  non-intermediate leakage when constructing A from `z_mat`. Column sums
  larger than `1 - value_added_floor` are rescaled to that maximum.
  Ignored when a precomputed `l_inv` is supplied without `z_mat`.

- conserve_extensions:

  If `TRUE`, rescale positive footprint flows within each origin
  area/item so their sum does not exceed the corresponding positive
  extension total. This keeps footprint outputs conservative when capped
  coefficients or negative final demand columns would otherwise make
  positive-only paths larger than the source extension.

## Value

A tibble with footprint results containing:

- `origin_area`: Country where the pressure occurs.

- `origin_item`: Item causing the pressure.

- `target_area`: Country consuming the product.

- `target_item`: Item consumed.

- `target_fd`: Demand category (e.g. `"food"`). Only present when
  `fd_labels` is provided.

- `value`: Footprint value in extension units.

## Examples

``` r
z_mat <- matrix(c(0, 5, 10, 0), nrow = 2)
x_vec <- c(100, 200)
l_inv <- compute_leontief_inverse(z_mat, x_vec)
#> Computing Leontief inverse (2x2 matrix)...
#> Inverting (I - A)...
#> ✔ Leontief inverse computed.
y_mat <- matrix(c(85, 195), ncol = 1)
extensions <- c(50, 30)
labels <- tibble::tibble(
  area_code = c(1L, 1L),
  item_cbs_code = c(1L, 2L)
)

# Small system: pass pre-computed L
compute_footprint(l_inv, x_vec, y_mat, extensions, labels)
#> ℹ Computing footprint for 2 sectors.
#>   2 sectors have non-zero extensions.
#>   Final demand: 1 column.
#> Computing multiplier matrix...
#> Computing footprints...
#> ✔ Footprint complete: 2 non-zero flows.
#> # A tibble: 2 × 5
#>   origin_area origin_item target_area target_item value
#>         <int>       <int>       <int>       <int> <dbl>
#> 1           1           1           1          NA  47.5
#> 2           1           2           1          NA  30.0

# Using Z directly (computes L internally)
compute_footprint(
  x_vec = x_vec, y_mat = y_mat,
  extensions = extensions, labels = labels,
  z_mat = z_mat
)
#> ℹ Computing footprint for 2 sectors.
#>   2 sectors have non-zero extensions.
#>   Final demand: 1 column.
#> Sparse solve path (no dense Leontief inverse).
#> Computing footprints...
#> ✔ Footprint complete: 2 non-zero flows.
#> # A tibble: 2 × 5
#>   origin_area origin_item target_area target_item value
#>         <int>       <int>       <int>       <int> <dbl>
#> 1           1           1           1          NA  47.5
#> 2           1           2           1          NA  30.0
```
