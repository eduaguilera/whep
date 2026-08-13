# Read the GLWD inland-water fraction on the 0.5-degree grid

Returns the Global Lakes and Wetlands Database surface-water fraction of
each 0.5-degree cell. `water_frac` is a fraction of the **whole** cell,
so the water area of a cell is `water_frac * cell_area_ha`.

This is the inland-water source for
[`build_polycell_support()`](https://eduaguilera.github.io/whep/reference/build_polycell_support.md).
It is preferred over `ne_10m_lakes`, which carries roughly half of
global inland water and omits the Caspian Sea entirely.

The fraction is derived from the GLWD rasters
`inst/scripts/download/download_hydrology.R` fetches from the published
figshare DOI, through
[`glwd_water_fraction()`](https://eduaguilera.github.io/whep/reference/glwd_water_fraction.md)
– the same derivation `inst/scripts/prepare_spatialize_all.R` uses to
write LPJmL's own `lakes_rivers` input, so the two cannot drift.

Until WHEP settled on GLWD v2 this read LPJmL's
`glwd_lakes_and_rivers_30arcmin.clm` and a companion `grid.clm` instead.
That pair is derived from GLWD **v1**, no script in this repository
produces it, and it gives 2.4759 Mkm2 of inland water over the
67,420-cell CRU land mask against v2's 3.2480 Mkm2. Any figure quoted
against the old layer has to be re-measured rather than carried across.

## Usage

``` r
read_glwd_water(dir = NULL)
```

## Source

Lehner, B., Anand, M., Fluet-Chouinard, E. et al. (2025). Mapping the
world's inland surface waters: an update to the Global Lakes and
Wetlands Database (GLWD v2). *Earth System Science Data* 17, 2277-2329.
[doi:10.5194/essd-17-2277-2025](https://doi.org/10.5194/essd-17-2277-2025)

## Arguments

- dir:

  Directory holding `GLWD/`, as `download_hydrology.R` lays it out.
  Defaults to `Sys.getenv("WHEP_LPJML_INPUT_DIR")`.

## Value

A `tibble` with `lon`, `lat` and `water_frac`, one row per cell the
rasters cover.

## Examples

``` r
# Requires WHEP_LPJML_INPUT_DIR to be set; not run without it.
if (nzchar(Sys.getenv("WHEP_LPJML_INPUT_DIR"))) {
  read_glwd_water()
}
```
