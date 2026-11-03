# Read the topsoil soil mass from HWSD onto WHEP's grid.

Read the mass of the HWSD (Harmonized World Soil Database) version 1.2
topsoil (0-30 cm) per 0.5-degree cell: per map unit the share-weighted
mean over its soil components of
`bulk_density x topsoil_depth_cm x 100 x (1 - t_gravel / 100)` Mg of
fine earth per ha, then averaged over the native HWSD cells inside each
0.5-degree block. It is the denominator that turns a 0-30 cm soil
organic carbon stock (Mg C per ha, as
[`build_carbon_balance()`](https://eduaguilera.github.io/whep/reference/build_carbon_balance.md)
reports it) into a mass fraction of the soil, as the soil organic matter
share of the Meisinger leaching drivers needs. The 0-30 cm carbon stock
of the same components,
[`read_hwsd_topsoil_soc()`](https://eduaguilera.github.io/whep/reference/read_hwsd_topsoil_soc.md)'s
`soc_obs_mgc_ha`, is aggregated in the same pass, so the observed HWSD
carbon fraction is `soc_obs_mgc_ha / soil_mass_mg_ha`.

A component with a density but no organic carbon counts towards the mass
and not towards the carbon stock; a component without a density counts
towards neither. A cell is returned only where both values exist.

## Usage

``` r
read_hwsd_topsoil_mass(
  hwsd_dir = NULL,
  bulk_density = c("measured", "reference"),
  data = list(),
  example = FALSE
)
```

## Source

FAO/IIASA/ISRIC/ISSCAS/JRC (2012). *Harmonized World Soil Database
version 1.2*. FAO, Rome and IIASA, Laxenburg – topsoil defined as 0-30
cm; fields `T_BULK_DENSITY`, `T_REF_BULK_DENSITY`, `T_GRAVEL`. Stock
equation: Hiederer, R. & Koechy, M. (2011). *Global Soil Organic Carbon
Estimates and the Harmonized World Soil Database*. EUR 25225 EN,
Publications Office of the European Union, 79 pp.

## Arguments

- hwsd_dir:

  Path to the directory holding `hwsd_data.csv` and `hwsd.bil`. Defaults
  to `Sys.getenv("WHEP_HWSD_DIR")`.

- bulk_density:

  Which HWSD bulk density to use. `"measured"` (default) takes
  `t_bulk_density`, falling back to `t_ref_bulk_density` where it is
  absent; `"reference"` takes `t_ref_bulk_density` alone. The default is
  not cosmetic: `t_ref_bulk_density` is derived from texture and so
  knows nothing about organic matter, and over the 1,375 map units with
  `t_oc` above 6% it averages 1.33 against a measured 0.37, which would
  inflate a peat soil's carbon stock roughly 3.6-fold. Recorded in
  `method_soc_obs`.

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

A tibble with `lon`, `lat`, `soil_mass_mg_ha` (0-30 cm fine-earth mass,
Mg per ha), `soc_obs_mgc_ha` (0-30 cm organic carbon, Mg C per ha) and
`method_soil_mass` (the bulk-density method).

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
read_hwsd_topsoil_mass(example = TRUE)
#> # A tibble: 1 × 5
#>     lon   lat soil_mass_mg_ha soc_obs_mgc_ha method_soil_mass
#>   <dbl> <dbl>           <dbl>          <dbl> <chr>           
#> 1 -0.25 -0.25            3900           41.6 measured        
```
