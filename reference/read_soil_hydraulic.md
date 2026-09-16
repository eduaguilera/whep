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
read_soil_hydraulic(
  hwsd_dir = NULL,
  data = list(),
  source = c("auto", "pin", "local"),
  version = NULL,
  example = FALSE
)
```

## Arguments

- hwsd_dir:

  Path to the directory holding `hwsd_data.csv` and `hwsd.bil`. Defaults
  to `Sys.getenv("WHEP_HWSD_DIR")`. Supplying it derives the grid
  locally rather than reading the pin.

- data:

  Optional named list of pre-loaded inputs: `cell_polity` (`lon`, `lat`,
  at minimum), used both to crop the HWSD raster and as the gap-filling
  target grid.

- source:

  Where the grid comes from. `"auto"` (default) reads the published pin
  unless `hwsd_dir` is given; `"pin"` always reads it; `"local"` always
  derives it from an HWSD archive. The pin is preferred so that every
  user shares one vintage: HWSD exists in two incompatible versions
  (v1.2 topsoil 0-30 cm, HWSD2 D1 0-20 cm) and the carbon balance
  reports 0-30 cm.

- version:

  Pin version, passed to
  [`whep_read_file()`](https://eduaguilera.github.io/whep/reference/whep_read_file.md).
  `NULL` takes the version frozen in
  [`whep_inputs`](https://eduaguilera.github.io/whep/reference/whep_inputs.md).

- example:

  If `TRUE`, return a small fixture instead of reading data. Defaults to
  `FALSE`.

## Value

A tibble with `lon`, `lat`, `t_field` (volumetric field capacity),
`t_wilt` (volumetric wilting point) and `porosity`, each a fraction.

## Caching

Aggregating the HWSD raster to the 0.5-degree grid takes about an hour
per pass, and its result depends only on the archive and the target
grid, so it is cached under `rappdirs::user_cache_dir("whep")`. The
cache key covers the archive's raster and header (size and modification
time), the resolution, the requested columns, the target grid's cells
and the derived map-unit values, plus an algorithm version that is
bumped whenever a change would move the numbers. Set
`WHEP_HWSD_CACHE_DIR` to relocate it; the test suite points it at a
temporary directory so a fixture-derived grid can never reach a real
cache.

## Examples

``` r
read_soil_hydraulic(example = TRUE)
#> # A tibble: 1 × 5
#>     lon   lat t_field t_wilt porosity
#>   <dbl> <dbl>   <dbl>  <dbl>    <dbl>
#> 1 -0.25 -0.25    0.29   0.14     0.43
```
