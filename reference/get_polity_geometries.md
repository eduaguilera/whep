# Get WHEP polity geometries

Returns the periodized polity database, including geometry. Pass
`polity_codes` to retrieve a subset that can be joined to outputs from
[`add_polity_code()`](https://eduaguilera.github.io/whep/reference/add_polity_code.md).

## Usage

``` r
get_polity_geometries(polity_codes = NULL)
```

## Arguments

- polity_codes:

  Optional character vector of WHEP polity codes. Subsetting by code
  needs the suggested package `sf` to be installed; the whole table is
  returned without it.

## Value

An sf data frame.

## Examples

``` r
# sf is only suggested, and its methods are what make the geometry column
# printable, so guard the example on it.
if (requireNamespace("sf", quietly = TRUE)) {
  codes <- add_polity_code(
    tibble::tibble(area_code = c(203L, 68L), year = c(2000L, 2000L))
  )$polity_code
  geometries <- get_polity_geometries(codes)
  print(geometries[, c("polity_code", "polity_name", "polygon_source")])
}
#> Simple feature collection with 2 features and 3 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -18.1643 ymin: 27.6375 xmax: 9.562218 ymax: 51.09257
#> Geodetic CRS:  WGS 84
#>       polity_code polity_name polygon_source                           geom
#> 220 ESP-1800-2025       Spain    cshapes-2.0 MULTIPOLYGON (((-17.88347 2...
#> 272 FRA-1919-2025      France    cshapes-2.0 MULTIPOLYGON (((9.50739 42....
```
