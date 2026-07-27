# Read gridded yearly LUH2 land-use-class fractions and areas.

Read the LUH2 v2h gridded land-use "states" product and aggregate its 12
subgrid states into the four carbon-balance classes (cropland,
grassland, natural, urban). Per cell-year-class the `fraction` is the
sum of the member states' grid-cell fractions (0..1); `area_ha` is that
fraction times the spherical 0.5-degree cell area. At
`resolution = "polity"` the areas are summed to each overlapping polity
via the country grid; a border cell keeps every polity it overlaps.

## Usage

``` r
read_luh2_landuse(
  resolution = c("grid", "polity"),
  years = NULL,
  data = NULL,
  example = FALSE
)
```

## Source

LUH2 v2h, Hurtt, G. C. et al. (2020). Harmonization of global land use
change and management for the period 850-2100 (LUH2) for CMIP6.
Geoscientific Model Development 13, 5425-5464.
[doi:10.5194/gmd-13-5425-2020](https://doi.org/10.5194/gmd-13-5425-2020)
.

## Arguments

- resolution:

  `"grid"` (default, per cell and class) or `"polity"` (aggregated to
  `area_code` per year and class).

- years:

  Optional integer vector of calendar years to keep. `NULL` keeps every
  year present in the source.

- data:

  Named list of pre-loaded inputs bypassing the pin read: `states` (raw
  per-cell-year-state fractions with `lon`, `lat`, `year`, `land_use`,
  `fraction`) and `country_grid` (`lon`, `lat`, `area_code`,
  `cell_area_frac`). Each falls back to its reader when absent.

- example:

  If `TRUE`, return a small fixture instead of reading remote data.
  Defaults to `FALSE`.

## Value

A tibble with columns `lon`, `lat`, `area_code`, `year`, `land_use`,
`fraction` and `area_ha` at `"grid"` resolution; at `"polity"`
resolution `lon` and `lat` are dropped and `area_ha` is summed per
`(area_code, year, land_use)`.

## Examples

``` r
read_luh2_landuse(example = TRUE)
#> # A tibble: 12 × 7
#>      lon   lat area_code  year land_use  fraction area_ha
#>    <dbl> <dbl>     <int> <int> <chr>        <dbl>   <dbl>
#>  1 -3.25 40.2        203  2000 cropland      0.4   94368.
#>  2 -3.25 40.2        203  2000 grassland     0.2   47184.
#>  3 -3.25 40.2        203  2000 natural       0.35  82572.
#>  4 -3.25 40.2        203  2000 urban         0.05  11796.
#>  5 35.2  -1.25        79  2000 cropland      0.3   92710.
#>  6 35.2  -1.25        79  2000 grassland     0.25  77258.
#>  7 35.2  -1.25        79  2000 natural       0.4  123613.
#>  8 35.2  -1.25        79  2000 urban         0.05  15452.
#>  9  9.25 47.8         11  2000 cropland      0.25  51958.
#> 10  9.25 47.8         11  2000 grassland     0.2   41567.
#> 11  9.25 47.8         11  2000 natural       0.5  103917.
#> 12  9.25 47.8         11  2000 urban         0.05  10392.
```
