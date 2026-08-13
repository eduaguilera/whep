# Derive the lake-and-river fraction of each 0.5-degree cell from GLWD

Aggregates the Global Lakes and Wetlands Database rasters to the
0.5-degree grid as a fraction of the whole cell. This is the one
implementation of that derivation in WHEP:
[`read_glwd_water()`](https://eduaguilera.github.io/whep/reference/read_glwd_water.md)
calls it for
[`build_polycell_support()`](https://eduaguilera.github.io/whep/reference/build_polycell_support.md),
and `inst/scripts/prepare_spatialize_all.R` calls it to write LPJmL's
`lakes_rivers` input. It used to live only in that script, so the
polycell producer read a hand-made `.clm` artefact of an LPJmL run
instead and the two answers were free to diverge.

## Usage

``` r
glwd_water_fraction(glwd_dir, cells = NULL)
```

## Arguments

- glwd_dir:

  Directory holding the GLWD rasters, as
  `inst/scripts/download/download_hydrology.R` lays them out: `GLWD_v2/`
  for v2, or `glwd_3/hdr.adf` / `glwd_3.tif` for v1.

- cells:

  Optional `tibble` of `lon`/`lat` cell centres to sample at. `NULL`
  (default) returns every cell of the 0.5-degree grid the rasters cover.

## Value

A `tibble` with `lon`, `lat` and `water_frac`, carrying a
`"glwd_version"` attribute of `"v1"` or `"v2"`.

## Which classes count as inland water

GLWD v2 is a **33-class wetland map**, not a water fraction, so a subset
has to be chosen and the choice is a judgement rather than a lookup.
Taken here: lakes as classes 1-3 (freshwater lake, saline lake,
reservoir) and rivers as class 7 (small streams). Everything else –
palustrine and riverine wetland, peatland, mangrove, saltmarsh, rice
paddies – is **excluded**: those are land that is wet, not surface
water, and
[`build_polycell_support()`](https://eduaguilera.github.io/whep/reference/build_polycell_support.md)
books them under `land_area_ha`.

Under GLWD v1 the equivalent classes are 1 (lakes) and 3 (rivers). The
two vintages are not interchangeable and give totals about 20% apart;
see
[`read_glwd_water()`](https://eduaguilera.github.io/whep/reference/read_glwd_water.md).

Class membership is multiplied by the companion `area_pct` raster where
one is present, so a partially covered source pixel contributes its own
fraction rather than counting whole.

## Examples

``` r
# Requires the GLWD download; not run without it.
if (nzchar(Sys.getenv("WHEP_LPJML_INPUT_DIR"))) {
  glwd_water_fraction(
    file.path(Sys.getenv("WHEP_LPJML_INPUT_DIR"), "GLWD")
  )
}
```
