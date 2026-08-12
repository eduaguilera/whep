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
  Typically `<l_files_dir>/whep/inputs`. If `NULL` or unset, the pinned
  WHEP spatialization inputs are used.

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
(physical land in hectares), and `method_land`, plus the polity columns
below.

## Polity columns

Every area-keyed output carries the polity its `area_code` resolves to
in that row's year:

- `polity_area_code`: The numeric key rows are AGGREGATED on, for the
  matrix workflows. It is a bucket, not an identity: use
  `reporting_polity_code` to say which territory a row belongs to.

- `reporting_polity_code`: The polity itself, e.g. `ESP-1846-1914`. It
  is year-aware, so the same `area_code` resolves to different polities
  in different years, which is the point of the crosswalk.

- `reporting_polity_name`: Its name. It can differ from the area's own
  name where the area folds into an aggregate.

- `reporting_polity_has_geometry`: Whether the polity has a polygon in
  the WHEP polity database, for callers that need to map or intersect
  it. `FALSE` is a documented gap upstream, not an error.

Rows whose `area_code` resolves to no polity keep the columns with `NA`
rather than being dropped, so a gap is visible instead of silent.

Rows before the back-cast anchor year resolve to the polity live in that
anchor year rather than to the polity live in the row's own year,
because WHEP's pre-anchor series are back-cast onto the anchor-year
territory. See
[`add_polity_code()`](https://eduaguilera.github.io/whep/reference/add_polity_code.md)
for the reasoning. Where that polity is not live in the row's own year –
41.5% of the pre-1961 `(area, year)` cells –
[`add_polity_code()`](https://eduaguilera.github.io/whep/reference/add_polity_code.md)
says so as `mapping_status == "backcast_anchor"`, and
[`polity_coverage_gaps()`](https://eduaguilera.github.io/whep/reference/polity_coverage_gaps.md)
reports it as `gap_kind == "backcast_anchor"`. These columns do not say
so either way.

A row whose year no mapped period covers is resolved to the NEAREST
period of the same area instead, so `reporting_polity_code` can name a
polity that did not exist in that row's year – FAOSTAT bucket 206 "Sudan
(former)" keeps reporting after `SUD-1956-2011` ends, and its post-2011
rows carry that code. These columns do not say so:
[`add_polity_code()`](https://eduaguilera.github.io/whep/reference/add_polity_code.md)
reports such a row as `mapping_status == "out_of_span"`, and that column
is dropped here so that adding it does not change the schema of every
area-keyed output at once.
[`polity_coverage_gaps()`](https://eduaguilera.github.io/whep/reference/polity_coverage_gaps.md)
reports the stand-in rows of a built table, and
`options(whep.polity_mapping_status = "flag")` (or `"status"`) carries
the signal on the outputs themselves. Both are opt-in; the default is no
extra column.

## Examples

``` r
get_crop_land_extension(example = TRUE)
#> # A tibble: 10 × 9
#>     year area_code polity_area_code reporting_polity_code reporting_polity_name
#>    <int>     <int>            <int> <chr>                 <chr>                
#>  1  2000        33               33 CAN-1949-2025         Canada               
#>  2  2000        33               33 CAN-1949-2025         Canada               
#>  3  2000        33               33 CAN-1949-2025         Canada               
#>  4  2000        33               33 CAN-1949-2025         Canada               
#>  5  2000       100              100 IND-1949-2025         India                
#>  6  2000       100              100 IND-1949-2025         India                
#>  7  2000       100              100 IND-1949-2025         India                
#>  8  2000       100              100 IND-1949-2025         India                
#>  9  2000       110              110 JPN-1952-2025         Japan                
#> 10  2000       110              110 JPN-1952-2025         Japan                
#> # ℹ 4 more variables: reporting_polity_has_geometry <lgl>, item_cbs_code <int>,
#> #   impact_u <dbl>, method_land <chr>
```
