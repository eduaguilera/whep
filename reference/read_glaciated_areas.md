# Read the Natural Earth glaciated-areas ice layer

Reads `ne_10m_glaciated_areas`, the ice source for
[`build_polycell_support()`](https://eduaguilera.github.io/whep/reference/build_polycell_support.md).
A few features are invalid under the spherical `s2` engine and
GEOS-level
[`sf::st_make_valid()`](https://r-spatial.github.io/sf/reference/valid.html)
does not repair them, so
[`sf::st_area()`](https://r-spatial.github.io/sf/reference/geos_measures.html)
aborts with "Loop 0 is not valid". Those features are repaired under the
planar engine, and any that remain invalid are measured with
[`terra::expanse()`](https://rspatial.github.io/terra/reference/expanse.html),
which does not go through `s2`, and reported rather than silently
dropped.

The layer is a coarse present-day snapshot, so ice area does **not**
vary historically. That is acceptable only while ice is a reporting
category rather than a driver.

## Usage

``` r
read_glaciated_areas(dir = NULL)
```

## Arguments

- dir:

  Directory holding the shapefile. Defaults to
  `Sys.getenv("WHEP_NATURALEARTH_DIR")`, under which the layer is
  expected at `ne_10m_glaciated_areas/ne_10m_glaciated_areas.shp`.

## Value

An `sf` table of glaciated polygons in WGS84, carrying a `s2_repaired`
logical column. The `"unrepaired"` attribute is a `tibble` of the
features that stayed `s2`-invalid, with their
[`terra::expanse()`](https://rspatial.github.io/terra/reference/expanse.html)
area.

## Examples

``` r
# Requires WHEP_NATURALEARTH_DIR to be set; not run without it.
if (nzchar(Sys.getenv("WHEP_NATURALEARTH_DIR"))) {
  read_glaciated_areas()
}
```
