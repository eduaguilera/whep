# Build a constant-territory time series for a reference year's boundaries

Estimates a time series of a quantity over a **fixed** set of
territorial boundaries — the polities active in `ref_year` — from data
reported under the *changing* historical boundaries of each data year.

Country borders change over time, so there is no raw constant-territory
series: a 1900 figure for "Austria-Hungary" is not a figure for
present-day Austria. This function estimates one by spatial reallocation
(dasymetric areal interpolation):

1.  For each data `year`, the value reported by each source polity is
    spread over **that polity's own extent for that year** across a
    regular grid, weighted by a `covariate` density (e.g. gridded
    cropland or population; uniform = plain areal weighting).

2.  The grid is then re-aggregated to the `ref_year` target boundaries:
    a target's estimate is the sum of grid mass falling inside it.

3.  Target territory **not covered** by any source with data in that
    year is *imputed* — its grid cells still carry covariate mass, so
    they are filled at a donor intensity (value per unit covariate)
    rather than left at zero. The fraction of a target's covariate mass
    that had to be imputed is reported as `imputed_share`, an honest
    confidence signal.

The estimate is only as good as the `covariate`: supply the same gridded
surface used elsewhere in WHEP spatialization (cropland for crop output,
population for demographic series, livestock density for animals). With
`covariate = NULL` the method reduces to area-weighted areal
interpolation.

## Usage

``` r
build_constant_territory_series(
  data,
  ref_year,
  polities = NULL,
  covariate = NULL,
  resolution = 25000,
  donor = c("regional", "none"),
  crs_equal_area = 6933,
  max_cells = 2e+06,
  verbose = TRUE
)
```

## Arguments

- data:

  A data frame of reported values with columns:

  - `year`: integer data year.

  - `polity_code`: the source polity that reported the value (must be
    active in `year` and carry a polygon).

  - `value`: numeric value (summed if a polity appears more than once).

- ref_year:

  Integer. Target boundaries are the polities active in this year
  (`start_year <= ref_year <= end_year`).

- polities:

  An `sf` of polity polygons with `polity_code`, `start_year`,
  `end_year` and geometry. Defaults to
  [`get_polity_geometries()`](https://eduaguilera.github.io/whep/reference/get_polity_geometries.md).

- covariate:

  `NULL` (uniform density, i.e. area weighting) or a function
  `function(centroids_sf, year) -> numeric` returning a non-negative
  density per grid-cell centroid (centroids are supplied in
  `crs_equal_area`).

- resolution:

  Grid cell size, in metres of `crs_equal_area`. Default 25000 (25 km).
  Smaller is more accurate but slower.

- donor:

  Gap-imputation rule: `"regional"` (default) fills uncovered target
  cells at the region-wide value-per-covariate intensity of the sources
  with data that year; `"none"` leaves them at zero (covered-only).

- crs_equal_area:

  EPSG code of an equal-area CRS used for gridding and areas. Default
  6933 (NSIDC EASE-Grid 2.0 Global).

- max_cells:

  Safety cap on grid cells per year (default 2e6). Aborts if the
  source/target extent would exceed it (usually a stray continent-scale
  target); restrict `polities`, coarsen `resolution`, or raise this.

- verbose:

  Logical; emit progress/warnings.

## Value

A tibble, one row per (`ref_year`-target, data `year`):

- `target_polity_code`, `year`

- `value`: constant-territory estimate (`covered + imputed`)

- `covered`: mass from cells overlapping a source with data

- `imputed`: mass added for uncovered cells

- `imputed_share`: covariate fraction imputed (0 = fully observed)

- `n_sources`: number of source polities contributing that year

## Examples

``` r
# Self-contained toy: two adjacent square polities. Only "P1" reports a
# value in 1900, so when the series is rebuilt onto the boundaries active in
# `ref_year` 2000 (both polities), "P2" is imputed from "P1"'s intensity.
make_square <- function(xmin, ymin, side) {
  sf::st_polygon(list(rbind(
    c(xmin, ymin),
    c(xmin + side, ymin),
    c(xmin + side, ymin + side),
    c(xmin, ymin + side),
    c(xmin, ymin)
  )))
}
polities <- sf::st_sf(
  polity_code = c("P1", "P2"),
  start_year = c(1800L, 1800L),
  end_year = c(2025L, 2025L),
  geometry = sf::st_sfc(
    make_square(0, 0, 2),
    make_square(2, 0, 2),
    crs = 4326
  )
)
reported <- tibble::tibble(
  year = 1900L,
  polity_code = "P1",
  value = 100
)
build_constant_territory_series(
  reported,
  ref_year = 2000,
  polities = polities,
  resolution = 50000,
  verbose = FALSE
)
#> # A tibble: 2 × 7
#>   target_polity_code  year value covered imputed imputed_share n_sources
#>   <chr>              <int> <dbl>   <dbl>   <dbl>         <dbl>     <int>
#> 1 P1                  1900   100     100       0             0         1
#> 2 P2                  1900   100       0     100             1         1
```
