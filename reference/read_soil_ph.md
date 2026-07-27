# Read gridded soil pH onto WHEP's grid.

Reads the HWSD (Harmonized World Soil Database) soil map unit attribute
table and raster, derives each map unit's pH from its dominant USDA
texture class, and aggregates the result to WHEP's 0.5-degree grid by
averaging the native HWSD cells inside each 0.5-degree block. Soil pH is
a static HWSD property: the result has no `year` column. When
`data$cell_polity` is supplied, the native HWSD raster is first cropped
to that grid's extent before reclassification (so a regional caller
never materialises or reclassifies the full-resolution global raster),
and cells present in that target grid but missing from the aggregated
HWSD grid are gap-filled from the nearest available neighbour; otherwise
cropping and gap-filling are both skipped and the returned grid covers
every cell where HWSD itself has data.

## Usage

``` r
read_soil_ph(hwsd_dir = NULL, data = list(), example = FALSE)
```

## Arguments

- hwsd_dir:

  Path to the directory holding `hwsd_data.csv` and `hwsd.bil`. Defaults
  to `Sys.getenv("WHEP_HWSD_DIR")`.

- data:

  Optional named list of pre-loaded inputs: `cell_polity` (`lon`, `lat`,
  at minimum), used both to crop the HWSD raster to the region of
  interest before reclassification and as the target grid for
  gap-filling. When absent, cropping and gap-filling are both skipped
  (documented fallback above).

- example:

  If `TRUE`, return a small fixture instead of reading data. Defaults to
  `FALSE`.

## Value

A tibble with `lon`, `lat`, `soil_ph`.

## Examples

``` r
read_soil_ph(example = TRUE)
#> # A tibble: 1 × 3
#>     lon   lat soil_ph
#>   <dbl> <dbl>   <dbl>
#> 1 -0.25 -0.25     6.8
```
