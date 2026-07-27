# Read gridded soil hydraulic properties from HWSD onto WHEP's grid.

Reads the HWSD (Harmonized World Soil Database) soil map unit attribute
table and raster, resolves each map unit's dominant USDA texture class,
looks up that class's volumetric field capacity, wilting point and
porosity from
[soil_hydraulic_by_texture](https://eduaguilera.github.io/whep/reference/soil_hydraulic_by_texture.md)
(via the
[hwsd_texture_usda](https://eduaguilera.github.io/whep/reference/hwsd_texture_usda.md)
code crosswalk), and aggregates each property to WHEP's 0.5-degree grid
by averaging the native HWSD cells inside each 0.5-degree block. These
are the per-cell soil hydraulic drivers the ICBM soil-carbon moisture
modifier consumes. Soil texture is a static HWSD property: the result
has no `year` column. Cropping to `data$cell_polity` follows the same
regional-crop path as
[`read_soil_ph()`](https://eduaguilera.github.io/whep/reference/read_soil_ph.md);
missing cells are gap-filled from the nearest available neighbour when a
target grid is supplied.

## Usage

``` r
read_soil_hydraulic(hwsd_dir = NULL, data = list(), example = FALSE)
```

## Arguments

- hwsd_dir:

  Path to the directory holding `hwsd_data.csv` and `hwsd.bil`. Defaults
  to `Sys.getenv("WHEP_HWSD_DIR")`.

- data:

  Optional named list of pre-loaded inputs: `cell_polity` (`lon`, `lat`,
  at minimum), used both to crop the HWSD raster and as the gap-filling
  target grid.

- example:

  If `TRUE`, return a small fixture instead of reading data. Defaults to
  `FALSE`.

## Value

A tibble with `lon`, `lat`, `t_field` (volumetric field capacity),
`t_wilt` (volumetric wilting point) and `porosity`, each a fraction.

## Examples

``` r
read_soil_hydraulic(example = TRUE)
#> # A tibble: 1 × 5
#>     lon   lat t_field t_wilt porosity
#>   <dbl> <dbl>   <dbl>  <dbl>    <dbl>
#> 1 -0.25 -0.25    0.29   0.14     0.43
```
