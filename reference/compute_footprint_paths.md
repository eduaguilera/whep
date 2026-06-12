# Compute first-use footprint paths.

Decompose an origin footprint into the first sector that directly uses
the origin product before the footprint reaches final demand. This is
useful for Sankey views that show paths such as origin product -\>
first-use area -\> first-use product -\> final-demand area.

The decomposition uses the IO identity \\x = d + A x\\. For each
selected origin sector \\i\\ and final-demand target, the origin
requirement \\x_i\\ is split into direct final demand \\d_i\\ and direct
intermediate use \\A\_{ij} x_j\\. Values are multiplied by the origin
extension intensity \\e_i / X_i\\.

## Usage

``` r
compute_footprint_paths(
  z_mat,
  x_vec,
  y_mat,
  extensions,
  labels,
  fd_labels,
  origin_area = NULL,
  origin_item = NULL,
  output_tol = 1e-08,
  value_added_floor = 0.001,
  conserve_extensions = TRUE,
  min_value = 0
)
```

## Arguments

- z_mat:

  Inter-industry flow matrix from
  [`build_io_model()`](https://eduaguilera.github.io/whep/reference/build_io_model.md).

- x_vec:

  Numeric vector of total output per sector.

- y_mat:

  Final demand matrix from
  [`build_io_model()`](https://eduaguilera.github.io/whep/reference/build_io_model.md).

- extensions:

  Numeric vector of environmental extensions per sector.

- labels:

  Tibble with `area_code` and `item_cbs_code` mapping sectors.

- fd_labels:

  Tibble labelling Y columns, from
  [`build_io_model()`](https://eduaguilera.github.io/whep/reference/build_io_model.md).

- origin_area:

  Optional area code vector limiting origin sectors.

- origin_item:

  Optional item code vector limiting origin sectors.

- output_tol:

  Minimum output considered valid when computing extension intensities.

- value_added_floor:

  Minimum non-intermediate leakage share used when constructing
  technical coefficients from `z_mat`.

- conserve_extensions:

  If `TRUE`, rescale positive paths within each origin area/item so
  their sum does not exceed the corresponding positive extension total.

- min_value:

  Drop paths with values less than or equal to this value before
  returning.

## Value

A tibble with `origin_area`, `origin_item`, `use_area`, `use_item`,
`target_area`, `target_item`, role-specific polity metadata,
`target_fd`, `path_type`, and `value`.
