# Compute final-product footprint paths.

Decompose an origin footprint by the area and item of the product
supplied to final demand. This adds the missing FABIO-viewer style phase
between origin product and final-demand area: origin product -\>
supplied product area -\> supplied product -\> final-demand area.

Unlike
[`compute_footprint_paths()`](https://eduaguilera.github.io/whep/reference/compute_footprint_paths.md),
this does not show the first direct intermediate input. It shows the
downstream product row in `Y` whose final demand carries the origin
footprint.

## Usage

``` r
compute_fp_product_paths(
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

A tibble with `origin_area`, `origin_item`, `product_area`,
`product_item`, `target_area`, role-specific polity metadata,
`target_fd`, and `value`.
