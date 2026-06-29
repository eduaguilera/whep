# Compute a footprint end-to-end from an extension table.

Trace a long-format environmental extension table through the supply
chain for one or more years and return a tidy footprint. This wraps the
three steps that the footprint driver scripts used to repeat inline:
build (or reuse) the input-output model with
[`build_io_model()`](https://eduaguilera.github.io/whep/reference/build_io_model.md),
align the extension to each year's sector labels with
[`align_extension()`](https://eduaguilera.github.io/whep/reference/align_extension.md),
and trace it with
[`compute_footprint()`](https://eduaguilera.github.io/whep/reference/compute_footprint.md).

The `extension` table is the output of any `build_*_extension()`
builder, such as
[`build_grassland_land_extension()`](https://eduaguilera.github.io/whep/reference/build_grassland_land_extension.md)
or
[`build_livestock_ghg_extension()`](https://eduaguilera.github.io/whep/reference/build_livestock_ghg_extension.md):
rows keyed by `year`, `area_code` and `item_cbs_code`, with the pressure
magnitude in `value_col`.

## Usage

``` r
build_footprint(
  extension,
  years = NULL,
  io = NULL,
  method = c("mass", "value"),
  value_col = "impact_u",
  ...
)
```

## Arguments

- extension:

  Long-format extension tibble with columns `year`, `area_code`,
  `item_cbs_code` and the column named by `value_col`.

- years:

  Years to compute. Defaults to the distinct years present in
  `extension`. Ignored when `io` is supplied.

- io:

  Optional pre-built
  [`build_io_model()`](https://eduaguilera.github.io/whep/reference/build_io_model.md)
  result (a tibble with one row per year). Supply it to reuse one model
  across several extensions instead of rebuilding it. When `NULL`
  (default), it is built for `years`.

- method:

  Co-product allocation method passed to
  [`build_io_model()`](https://eduaguilera.github.io/whep/reference/build_io_model.md),
  `"mass"` (default) or `"value"`. Ignored when `io` is supplied (the
  model already encodes its allocation).

- value_col:

  Name of the extension magnitude column, `"impact_u"` by default.

- ...:

  Further arguments passed to
  [`compute_footprint()`](https://eduaguilera.github.io/whep/reference/compute_footprint.md)
  (e.g. `conserve_extensions`, `report_conservation`).

## Value

A tibble of footprint flows as returned by
[`compute_footprint()`](https://eduaguilera.github.io/whep/reference/compute_footprint.md),
with an added `year` column.

## Examples

``` r
io <- tibble::tibble(
  year = 2000L,
  Z = list(matrix(c(0, 5, 10, 0), nrow = 2)),
  X = list(c(100, 200)),
  Y = list(matrix(c(85, 195), ncol = 1)),
  labels = list(tibble::tibble(
    index = 1:2,
    area_code = c(1L, 1L),
    item_cbs_code = c(1L, 2L)
  )),
  fd_labels = list(tibble::tibble(area_code = 1L, fd_col = "food"))
)
extension <- tibble::tibble(
  year = 2000L,
  area_code = 1L,
  item_cbs_code = c(1L, 2L),
  impact_u = c(50, 30)
)
build_footprint(extension, io = io)
#> ℹ Computing footprint for 2 sectors.
#>   2 sectors have non-zero extensions.
#>   Final demand: 1 column.
#> Sparse solve path (no dense Leontief inverse).
#> Computing footprints...
#> ✔ Footprint complete: 4 non-zero flows.
#> # A tibble: 4 × 13
#>   origin_area origin_polity_code origin_polity_name origin_polity_has_geometry
#>         <int> <chr>              <chr>              <lgl>                     
#> 1           1 ARM-1991-2025      Armenia            TRUE                      
#> 2           1 ARM-1991-2025      Armenia            TRUE                      
#> 3           1 ARM-1991-2025      Armenia            TRUE                      
#> 4           1 ARM-1991-2025      Armenia            TRUE                      
#> # ℹ 9 more variables: origin_item <int>, target_area <int>,
#> #   target_polity_code <chr>, target_polity_name <chr>,
#> #   target_polity_has_geometry <lgl>, target_item <int>, target_fd <chr>,
#> #   value <dbl>, year <int>
```
