# Add a final-demand product-area stage to footprints.

Split footprint rows by the area that supplied the final-demand product,
using shares from `y_mat`. This preserves the standard footprint totals
while adding a FABIO-viewer style phase: origin product -\>
product/supplier area -\> product -\> final-demand area.

This is a compact global-view helper. It does not recompute the full
origin-sector by product-sector Leontief cube. Instead, each existing
footprint row is allocated over the product-area shares observed in
final demand for the same `target_area`, `target_fd`, and `target_item`.

## Usage

``` r
add_footprint_product_stage(
  footprints,
  y_mat,
  labels,
  fd_labels,
  max_product_areas = 5,
  other_area_name = "Other",
  min_share = 0
)
```

## Arguments

- footprints:

  Footprint table from
  [`compute_footprint()`](https://eduaguilera.github.io/whep/reference/compute_footprint.md)
  with `target_area`, `target_item`, `target_fd`, and `value`.

- y_mat:

  Final demand matrix from
  [`build_io_model()`](https://eduaguilera.github.io/whep/reference/build_io_model.md).

- labels:

  Tibble mapping Y rows to `area_code` and `item_cbs_code`.

- fd_labels:

  Tibble mapping Y columns to `area_code` and `fd_col`.

- max_product_areas:

  Maximum number of supplier/product areas to keep separately for each
  final-demand area, item, and demand category. Smaller supplier areas
  are grouped into `other_area_name`.

- other_area_name:

  Label for grouped supplier/product areas.

- min_share:

  Drop split paths smaller than this percentage of the total input
  footprint value. Use 0 to keep all split paths.

## Value

`footprints` with `product_area`, `product_area_name`, `product_item`,
and `product_share` columns. `value` is replaced by the split path
value.
