# A minimal polity geometry table for examples and smoke tests

Returns one live polity holding a rectangle that spans six 0.5-degree
cells, in the shape
[`get_polity_geometries()`](https://eduaguilera.github.io/whep/reference/get_polity_geometries.md)
returns: enough to run
[`build_polycell_support()`](https://eduaguilera.github.io/whep/reference/build_polycell_support.md)
end to end in a fraction of a second, with no pins, no rasters and no
environment variables.

## Usage

``` r
polycell_example_geometries()
```

## Value

An `sf` table with `polity_code`, `polity_type`, `wiki_status`,
`polygon_status`, `start_year`, `end_year`, `area_code` and a `geom`
multipolygon in WGS84.

## Examples

``` r
if (requireNamespace("sf", quietly = TRUE)) {
  polycell_example_geometries()
}
#> Simple feature collection with 1 feature and 7 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 10.1 ymin: 44.9 xmax: 11.4 ymax: 45.4
#> Geodetic CRS:  WGS 84
#>     polity_code polity_type wiki_status polygon_status start_year end_year
#> 1 AAA-2000-2020    national    reviewed       assigned       2000     2020
#>   area_code                           geom
#> 1        11 POLYGON ((10.1 44.9, 11.4 4...
```
