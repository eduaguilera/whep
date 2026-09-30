# The cropland each unit holds, on the basis the capacity ceiling uses

Sum a gridded cropland extent onto the units of an allocation layer,
weighting every cell by `cell_area_frac`. This is the denominator
[`reconcile_admin_allocation()`](https://eduaguilera.github.io/whep/reference/reconcile_admin_allocation.md)'s
implied cropping intensity divides by, and it is deliberately the same
quantity `.capacity_bases()` multiplies by the multi-cropping factor: an
intensity taken against any other land basis would not be comparable
with the ceiling it is flagged against.

A physical cell shared by two units contributes its cropland to each in
proportion, so the units' extents inside one cell add up to that cell's
own cropland and nothing is double counted. Rows are selected per year
by the layer's own validity convention, so an interval-keyed layer
returns the unit that existed in each cropland year.

## Usage

``` r
unit_cropland_extent(allocation_layer, gridded_cropland)
```

## Arguments

- allocation_layer:

  The country grid the allocation ran on, from
  [`build_allocation_layer()`](https://eduaguilera.github.io/whep/reference/build_allocation_layer.md):
  `lon`, `lat`, `area_code`, `cell_area_frac` and, where a depth is
  granted, `level_polity_code`.

- gridded_cropland:

  Per-cell cropland extent, as
  [`build_gridded_landuse()`](https://eduaguilera.github.io/whep/reference/build_gridded_landuse.md)
  takes it: `lon`, `lat`, `year`, `cropland_ha`.

## Value

A tibble of `year`, `area_code`, `level_polity_code`, `cropland_ha` and
`n_cells`.

## See also

[`reconcile_admin_allocation()`](https://eduaguilera.github.io/whep/reference/reconcile_admin_allocation.md).

## Examples

``` r
layer <- tibble::tibble(
  lon = c(0.25, 0.75, 0.75), lat = 50.25, area_code = 1L,
  level_polity_code = c("A1", "A1", "A2"), level = 1L,
  cell_area_frac = c(1, 0.25, 0.75)
)
cropland <- tibble::tibble(
  lon = c(0.25, 0.75), lat = 50.25, year = 2000L,
  cropland_ha = c(100, 400)
)
unit_cropland_extent(layer, cropland)
#> # A tibble: 2 × 5
#>    year area_code level_polity_code cropland_ha n_cells
#>   <int>     <int> <chr>                   <dbl>   <int>
#> 1  2000         1 A1                        200       2
#> 2  2000         1 A2                        300       1
```
