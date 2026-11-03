# Read gridded FAO soil texture class from HWSD onto WHEP's grid.

Derives the FAO soil texture class of HWSD (Harmonized World Soil
Database) version 1.2 from the topsoil clay and sand fractions and
returns, for each 0.5-degree cell, the class most of its native HWSD
pixels fall in, named as the texture classes of
[image_gnm_leaching_factors](https://eduaguilera.github.io/whep/reference/image_gnm_leaching_factors.md)
(Table 2 of Beusen et al. 2015), the soil texture factor of the
IMAGE-GNM root-zone denitrification. Soil texture is a static HWSD
property: the result has no `year` column.

## Usage

``` r
read_soil_texture_fao(
  hwsd_dir = NULL,
  data = list(),
  source = c("auto", "pin", "local"),
  version = NULL,
  example = FALSE
)
```

## Source

FAO/IIASA/ISRIC/ISSCAS/JRC (2012). *Harmonized World Soil Database
version 1.2*. FAO, Rome and IIASA, Laxenburg. Texture classes:
documentation p. 10 (`T_TEXTURE`); Histosols: pp. 29-31.

## Arguments

- hwsd_dir:

  Path to the directory holding `hwsd_data.csv` and `hwsd.bil`. Defaults
  to `Sys.getenv("WHEP_HWSD_DIR")`. Supplying it derives the grid
  locally rather than reading the pin.

- data:

  Optional named list of pre-loaded inputs: `cell_polity` (`lon`, `lat`,
  at minimum), used to crop the HWSD raster and a pinned grid to the
  region of interest, and `texture_fao_counts`, a per-class pixel count
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

A tibble with `lon`, `lat`, `soil_texture_fao` (one of `"coarse"`,
`"medium"`, `"fine"`, `"organic"`) and `method_soil_texture_fao`
(`hwsd_v1.2_fao3_clay_sand` or `hwsd_v1.2_histosol_unit`, followed by
`;tied_dominant_class` where the cell is tied).

## Details

Each soil component takes the class of the HWSD documentation's
definition of the simplified FAO textural classes of the Soil Map of the
World (field `T_TEXTURE`, p. 10): coarse is less than 18% clay and more
than 65% sand, fine is more than 35% clay, and medium is the rest of the
soils with up to 35% clay. The limits are read as printed, so a
component with exactly 35% clay, or exactly 65% sand and under 18% clay,
is medium; the database's own `T_TEXTURE` also calls all 726 such
components medium. A component with no USDA texture class, which HWSD
reports as zero clay, silt and sand, has no mineral texture and is
skipped. A component whose soil unit is a Histosol is organic whatever
its fractions are. The documentation defines Histosols as "soils which
are composed of organic materials" (p. 30) and lists their units in the
FAO-74 (`O`, `Oe`, `Od`, `Ox`, p. 31) and FAO-90 (`HS`, `HSl`, `HSs`,
`HSf`, `HSt`, `HSi`, p. 29) legends. It gives no organic carbon limit
for organic soils.

The documentation defines three texture classes and no very fine class
(it simplifies the five classes of its SOTER source to three, p. 7), so
very fine soils fall in `"fine"` and the class `"very_fine"` of
[image_gnm_leaching_factors](https://eduaguilera.github.io/whep/reference/image_gnm_leaching_factors.md)
is never returned. The stamp `method_soil_texture_fao` says so on every
row.

A map unit takes the class with the largest summed `share` over its
components and a cell the class with the most pixels; ties go to the
first of coarse, medium, fine and organic, and a tied cell is marked in
`method_soil_texture_fao`. The published pin holds the per-class pixel
counts, not the class. Cells with no classified pixel are absent from
the result and are not gap-filled. An extract that is not HWSD v1.2, or
that holds a clay or sand fraction outside 0-100%, aborts.

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
read_soil_texture_fao(example = TRUE)
#> # A tibble: 1 × 4
#>     lon   lat soil_texture_fao method_soil_texture_fao 
#>   <dbl> <dbl> <chr>            <chr>                   
#> 1 -0.25 -0.25 medium           hwsd_v1.2_fao3_clay_sand
```
