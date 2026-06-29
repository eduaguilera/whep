# Align an extension table to input-output sector labels.

Turn a long-format extension table into the dense per-sector numeric
vector that
[`compute_footprint()`](https://eduaguilera.github.io/whep/reference/compute_footprint.md)
expects, ordered to match a single year's `labels` from
[`build_io_model()`](https://eduaguilera.github.io/whep/reference/build_io_model.md).
Sectors absent from the extension are filled with zero, and extension
rows outside the model are dropped. Rows sharing an
`(area_code, item_cbs_code)` are summed.

## Usage

``` r
align_extension(extension, labels, year, value_col = "impact_u")
```

## Arguments

- extension:

  Long-format extension tibble with `year`, `area_code`, `item_cbs_code`
  and the column named by `value_col`.

- labels:

  One year's `labels` tibble from
  [`build_io_model()`](https://eduaguilera.github.io/whep/reference/build_io_model.md),
  with `area_code`, `item_cbs_code` and `index` columns.

- year:

  Year to select from `extension`.

- value_col:

  Name of the extension magnitude column, `"impact_u"` by default.

## Value

A numeric vector with one entry per row of `labels`, ordered by the
label `index`.

## Examples

``` r
extension <- tibble::tibble(
  year = 2000L,
  area_code = 1L,
  item_cbs_code = 10L,
  impact_u = 5
)
labels <- tibble::tibble(
  index = 1:2,
  area_code = c(1L, 1L),
  item_cbs_code = c(10L, 20L)
)
align_extension(extension, labels, 2000L)
#> [1] 5 0
```
