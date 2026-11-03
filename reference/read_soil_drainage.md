# Read gridded soil drainage class from HWSD onto WHEP's grid.

Reads the drainage class of HWSD (Harmonized World Soil Database)
version 1.2 and returns, for each 0.5-degree cell, the class most of its
native HWSD pixels fall in, named as the drainage classes of
[image_gnm_leaching_factors](https://eduaguilera.github.io/whep/reference/image_gnm_leaching_factors.md)
(Table 2 of Beusen et al. 2015), the soil drainage factor of the
IMAGE-GNM root-zone denitrification. HWSD drainage is a reference
condition on flat terrain (0-0.5% slope), not a measured water table.
Soil drainage is a static HWSD property: the result has no `year`
column.

## Usage

``` r
read_soil_drainage(
  hwsd_dir = NULL,
  data = list(),
  source = c("auto", "pin", "local"),
  version = NULL,
  example = FALSE
)
```

## Source

FAO/IIASA/ISRIC/ISSCAS/JRC (2012). *Harmonized World Soil Database
version 1.2*. FAO, Rome and IIASA, Laxenburg. Drainage classes: the
field `DRAINAGE` and its lookup `D_DRAINAGE` in `HWSD.mdb`;
documentation p. 10 (drainage classes after FAO 1995, flat terrain).
Class names: Beusen, A. H. W. et al. (2015). Geosci. Model Dev. 8,
4045-4067, Table 2.

## Arguments

- hwsd_dir:

  Path to the directory holding `hwsd_data.csv` and `hwsd.bil`. Defaults
  to `Sys.getenv("WHEP_HWSD_DIR")`. Supplying it derives the grid
  locally rather than reading the pin.

- data:

  Optional named list of pre-loaded inputs: `cell_polity` (`lon`, `lat`,
  at minimum), used to crop the HWSD raster and a pinned grid to the
  region of interest, and `drainage_counts`, a per-class pixel count
  grid that replaces both routes.

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

A tibble with `lon`, `lat`, `soil_drainage_class` (one of
`"very_poorly_drained"`, `"poorly_drained"`, `"imperfectly_drained"`,
`"moderately_well_drained"`, `"well_drained"`,
`"somewhat_excessively_drained"`, `"excessively_drained"`) and
`method_soil_drainage`.

## Details

Each HWSD map unit takes the drainage class with the largest summed
`share` over its soil components, and each 0.5-degree cell the class
with the most pixels. Ties go to the wetter class (the lower HWSD code),
and a tied cell is marked in `method_soil_drainage`. The published pin
holds the per-class pixel counts, not the class, as the pin of
[`read_soil_hydraulic()`](https://eduaguilera.github.io/whep/reference/read_soil_hydraulic.md)
does.

The HWSD v1.2 codes map as follows, each read from the database's own
`D_DRAINAGE` lookup (the documentation gives the basis of the classes,
p. 10, but not the codes): 1 Very Poor to very poorly drained, 2 Poor to
poorly drained, 3 Imperfectly to imperfectly drained, 4 Moderately Well
to moderately well drained, 5 Well to well drained, 6 Somewhat Excessive
to somewhat excessively drained and 7 Excessive to excessively drained.
No HWSD v1.2 soil unit carries code 7. A code outside 1-7 aborts, and so
does an extract that is not HWSD v1.2, whose coding differs.

Cells whose HWSD units carry no drainage class (open water, ice and
other non-soil units) are absent from the result. They are not
gap-filled, because carrying a neighbour's class there would be a method
choice.

## Caching

Aggregating the HWSD raster to the 0.5-degree grid takes about an hour
per pass, and its result depends only on the archive and the target
grid, so it is cached under `rappdirs::user_cache_dir("whep")`. The
cache key covers the archive's raster and header (size and modification
time), the resolution, the requested columns, the target grid's cells
and the derived map-unit values, plus an algorithm version that is
bumped whenever a change would move the numbers. Set
`WHEP_HWSD_CACHE_DIR` to relocate it.

## Examples

``` r
read_soil_drainage(example = TRUE)
#> # A tibble: 1 × 4
#>     lon   lat soil_drainage_class method_soil_drainage   
#>   <dbl> <dbl> <chr>               <chr>                  
#> 1 -0.25 -0.25 well_drained        hwsd_v1.2_drainage_code
```
