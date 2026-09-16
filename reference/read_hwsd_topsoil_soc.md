# Read observed topsoil organic carbon from HWSD onto WHEP's grid.

Read the observed **0-30 cm** soil organic carbon stock per 0.5-degree
cell from the HWSD (Harmonized World Soil Database) map-unit attribute
table and raster. Each map unit's stock is the share-weighted mean over
its soil components of
`t_oc * bulk_density * 30 * (1 - t_gravel / 100)`, and the map units are
then averaged over the native HWSD cells inside each 0.5-degree block by
the same aggregation
[`read_soil_ph()`](https://eduaguilera.github.io/whep/reference/read_soil_ph.md)
uses.

This exists to be a **benchmark**, not a model input: nothing in the
carbon pipeline consumes it. HWSD version 1.2's topsoil is 0-30 cm,
exactly the layer
[`build_carbon_balance()`](https://eduaguilera.github.io/whep/reference/build_carbon_balance.md)
reports (see its Soil depth section), so it is the only observational
anchor available at WHEP's own modelled depth – and it comes out of the
archive the carbon balance already reads for clay, so it needs no new
download. Note that HWSD**2**, which
`inst/scripts/download/download_hwsd.R` fetches, layers its topsoil as
D1 = 0-20 cm instead; the two are not interchangeable (whep#851).

## Usage

``` r
read_hwsd_topsoil_soc(
  hwsd_dir = NULL,
  bulk_density = c("measured", "reference"),
  data = list(),
  example = FALSE
)
```

## Source

FAO/IIASA/ISRIC/ISSCAS/JRC (2012). *Harmonized World Soil Database
version 1.2*. FAO, Rome and IIASA, Laxenburg – topsoil defined as 0-30
cm. Stock equation and the bulk-density caveat: Hiederer, R. & Koechy,
M. (2011). *Global Soil Organic Carbon Estimates and the Harmonized
World Soil Database*. EUR 25225 EN, Publications Office of the European
Union, 79 pp.

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

A tibble with `lon`, `lat`, `soc_obs_mgc_ha` (0-30 cm soil organic
carbon, Mg C per ha) and `method_soc_obs`.

## Examples

``` r
read_hwsd_topsoil_soc(example = TRUE)
#> # A tibble: 1 × 4
#>     lon   lat soc_obs_mgc_ha method_soc_obs
#>   <dbl> <dbl>          <dbl> <chr>         
#> 1 -0.25 -0.25           41.6 measured      
```
