# Get the per-crop physical cropland extension from spatialization inputs.

Convenience wrapper that loads the gridded land-use inputs, spatializes
crop harvested area with
[`build_gridded_landuse()`](https://eduaguilera.github.io/whep/reference/build_gridded_landuse.md)
(crop-level, no CFT aggregation), and converts it to a per-crop physical
land extension with
[`build_crop_land_extension()`](https://eduaguilera.github.io/whep/reference/build_crop_land_extension.md).
The result is keyed by `(year, area_code, item_cbs_code)` and ready to
use as `extensions` in
[`compute_footprint()`](https://eduaguilera.github.io/whep/reference/compute_footprint.md).

## Usage

``` r
get_crop_land_extension(
  input_dir = NULL,
  years = NULL,
  method = c("cropland_apportion", "intensity_divide"),
  use_type_constraint = FALSE,
  fill_missing_patterns = TRUE,
  example = FALSE
)
```

## Arguments

- input_dir:

  Directory holding the spatialization inputs (`country_areas.parquet`,
  `crop_patterns.parquet`, `gridded_cropland.parquet`,
  `country_grid.parquet`, and optionally `multicropping.parquet`).
  Typically `<l_files_dir>/whep/inputs`. Required unless
  `example = TRUE`.

- years:

  Numeric vector of years to compute, or `NULL` for all available.

- method:

  Physical-area conversion method passed to
  [`build_crop_land_extension()`](https://eduaguilera.github.io/whep/reference/build_crop_land_extension.md).

- use_type_constraint:

  If `TRUE`, restrict each crop to cells of its LUH2 type (requires
  `type_cropland.parquet`). Defaults to `FALSE`.

- fill_missing_patterns:

  If `TRUE` (default), crops that have harvested area but no
  `crop_patterns` rows (e.g. Barley, absent from the Monfreda layer) are
  placed with a uniform fallback pattern over each producing country's
  cropland, so their land is not silently dropped.

- example:

  If `TRUE`, return a small example output without reading remote/large
  data. Defaults to `FALSE`.

## Value

A tibble with columns `year`, `area_code`, `item_cbs_code`, `impact_u`
(physical land in hectares), and `method_land`.

## Examples

``` r
get_crop_land_extension(example = TRUE)
#> # A tibble: 10 × 5
#>     year area_code item_cbs_code impact_u method_land       
#>    <int>     <int>         <int>    <dbl> <chr>             
#>  1  2000        33          2511 17562678 cropland_apportion
#>  2  2000        33          2514  2159391 cropland_apportion
#>  3  2000        33          2516  3206883 cropland_apportion
#>  4  2000        33          2555  7782531 cropland_apportion
#>  5  2000       100          2511 27345112 cropland_apportion
#>  6  2000       100          2513  9810455 cropland_apportion
#>  7  2000       100          2531  1204599 cropland_apportion
#>  8  2000       100          2555  7218004 cropland_apportion
#>  9  2000       110          2511  1188233 cropland_apportion
#> 10  2000       110          2805  1503221 cropland_apportion
```
