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
  max_column_sum = 100,
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

  Optional vector limiting origin sectors. Each value is matched against
  `labels$area_code` – the LEGACY numeric area code – and a value that
  matches no `area_code` is then resolved through the polity vocabulary
  of
  [polity_area_crosswalk](https://eduaguilera.github.io/whep/reference/polity_area_crosswalk.md),
  so a `polity_area_code` (`206`, the Sudan aggregation bucket) or a
  `polity_code` (`"SDN-2011-2025"`) selects the area codes it covers.
  Legacy codes keep their legacy meaning. Values that resolve to no
  sector are dropped with a warning, and a call in which nothing
  resolves aborts instead of returning an empty table.

- origin_item:

  Optional item code vector limiting origin sectors.

- output_tol:

  Minimum output considered valid when computing extension intensities.

- value_added_floor:

  Minimum non-intermediate leakage share used when constructing
  technical coefficients from `z_mat`.

- max_column_sum:

  Maximum allowed column sum in A. Must match the value used by
  [`compute_footprint()`](https://eduaguilera.github.io/whep/reference/compute_footprint.md)
  (default `100`) so the path decomposition and the footprint it
  decomposes share an identical A cap.

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

## Examples

``` r
# A two-sector economy: sector 1 (item 10) carries the whole extension and
# supplies 50 of intermediate input to sector 2 (item 20). Its footprint
# therefore splits into a direct final-demand path and a first-use path
# through item 20.
z_mat <- matrix(c(0, 50, 0, 0), nrow = 2, byrow = TRUE)
x_vec <- c(100, 200)
y_mat <- matrix(c(10, 100), nrow = 2)
labels <- tibble::tibble(
  area_code = c(1L, 1L),
  item_cbs_code = c(10L, 20L)
)
fd_labels <- tibble::tibble(area_code = 2L, fd_col = "food")

compute_footprint_paths(
  z_mat = z_mat,
  x_vec = x_vec,
  y_mat = y_mat,
  extensions = c(200, 0),
  labels = labels,
  fd_labels = fd_labels,
  conserve_extensions = FALSE
) |>
  dplyr::select(origin_item, use_item, target_area, path_type, value)
#> ℹ Computing first-use footprint paths for 1 origin sector.
#>   Final demand: 1 column.
#> Decomposing first-use paths...
#> ✔ First-use paths complete: 2 non-zero flows.
#> # A tibble: 2 × 5
#>   origin_item use_item target_area path_type    value
#>         <int>    <int>       <int> <chr>        <dbl>
#> 1          10       10           2 final_demand  20  
#> 2          10       20           2 intermediate  50.0
```
