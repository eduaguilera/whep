# Build the gridded agricultural land support.

Assembles the physical agricultural land support that
[`build_n_inputs()`](https://eduaguilera.github.io/whep/reference/build_n_inputs.md)
allocates its non-crop-specific nitrogen terms over: per grid cell,
polity, year and CBS item, the hectares of agricultural land available
to receive nitrogen. Cropland hectares come from the LUH2-derived
`type_cropland` surface, split among crops by the static `crop_patterns`
composition (normalised within each cell, so the cell's physical
cropland area is apportioned rather than inflated by multicropping).
Grassland hectares come from
[`read_luh2_landuse()`](https://eduaguilera.github.io/whep/reference/read_luh2_landuse.md)'s
gridded grassland class and are all carried on CBS 3000, with no
intensive/extensive split inferred. Both sides are split across border
polities by the same `cell_polity` crosswalk.

Years with cropland but no grassland coverage (a grassland source that
runs short of the cropland surface, as `"luh2"` does after 2015) keep
their cropland support and raise a warning naming the affected years;
supply `data$grassland_ha` to cover them.

## Usage

``` r
build_ag_land_support(
  years = NULL,
  grassland = c("gridded_pasture", "luh2", "none"),
  data = list(),
  example = FALSE
)
```

## Arguments

- years:

  Optional integer vector of calendar years to keep. `NULL` (default)
  keeps every year the cropland surface covers.

- grassland:

  Grassland-support source. `"gridded_pasture"` (default) is the
  prepared per-cell `pasture_ha` + `rangeland_ha` surface, which shares
  the cropland surface's grid and 1851-2023 span. `"luh2"` reads the
  same LUH2 classes through
  [`read_luh2_landuse()`](https://eduaguilera.github.io/whep/reference/read_luh2_landuse.md)
  and agrees with it where they overlap, but stops at 2015. `"none"`
  returns cropland-only support, an explicit choice rather than a silent
  gap.

- data:

  Optional named list of pre-loaded inputs to avoid remote reads:
  `cell_polity` (the
  [`build_cell_polity()`](https://eduaguilera.github.io/whep/reference/build_cell_polity.md)
  crosswalk), `type_cropland` (`lon`, `lat`, `year`, `luh2_type`,
  `type_ha`), `crop_patterns` (`lon`, `lat`, `item_prod_code`,
  `harvest_fraction`), `gridded_pasture` (`lon`, `lat`, `year`,
  `pasture_ha`, `rangeland_ha`), `states`
  ([`read_luh2_landuse()`](https://eduaguilera.github.io/whep/reference/read_luh2_landuse.md)'s
  raw LUH2 states) and `grassland_ha` (`lon`, `lat`, `area_code`,
  `year`, `area_ha`, bypassing the grassland read entirely). Each falls
  back to its reader when absent.

- example:

  If `TRUE`, return a small fixture instead of reading remote data.
  Defaults to `FALSE`.

## Value

A tibble with `lon`, `lat`, `area_code`, `item_cbs_code`, `year`,
`land_use` (`"cropland"` or `"grassland"`) and positive `area_ha`.

## Examples

``` r
build_ag_land_support(example = TRUE)
#> # A tibble: 7 × 7
#>     lon   lat area_code item_cbs_code  year land_use  area_ha
#>   <dbl> <dbl>     <int>         <int> <int> <chr>       <dbl>
#> 1  0.25  50.2        10          2511  2010 cropland     750 
#> 2  0.25  50.2        10          2513  2010 cropland     250 
#> 3  0.25  50.2        10          3000  2010 grassland   1977.
#> 4  0.75  50.2        10          2511  2010 cropland     300 
#> 5  0.75  50.2        10          3000  2010 grassland   2372.
#> 6  0.75  50.2        20          2511  2010 cropland     200 
#> 7  0.75  50.2        20          3000  2010 grassland   1581.
```
