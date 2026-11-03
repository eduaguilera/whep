# IMAGE-GNM surface runoff and erosion factors.

The factors of IMAGE-GNM's surface runoff nitrogen (Beusen et al. 2015,
eqs. 8-9, p. 4052), read by
[`calculate_n_leaching()`](https://eduaguilera.github.io/whep/reference/calculate_n_leaching.md)
with `method = "image_gnm"`:

- `runoff_land_use`: `f_qsro(landuse)` of eq. 3 (p. 4049), the land-use
  factor of the surface runoff fraction, as listed with its standard
  values in Table 3 (p. 4056).

- `erosion_texture`: the texture factor of the erodibility, from the
  authors' `fix_input/texture_erosion.vat`.

- `soil_loss`: the soil loss per km2 and year per unit erodibility of
  each land use, from the authors' parameter file. The paper prints only
  the European means these were calibrated to (360, 40 and 15 t per km2
  for arable land, grassland and natural vegetation, p. 4052).

- `erosion_cn`: the C:N ratio of eroded soil (p. 4052).

The paper sets the soil loss of peat soils equal to that of fine texture
(p. 4052); the authors' table gives peat 0.5 and fine 1.5, and the
table, the one their results came from, is used.

## Usage

``` r
image_gnm_runoff_factors
```

## Format

A tibble with columns:

- factor:

  `"runoff_land_use"`, `"erosion_texture"`, `"soil_loss"` or
  `"erosion_cn"`.

- class:

  Land use `"Cropland"`, `"Grassland"` or `"Natural"`, or the texture
  classes of
  [image_gnm_leaching_factors](https://eduaguilera.github.io/whep/reference/image_gnm_leaching_factors.md)
  (`"coarse"`, `"medium"`, `"fine"`, `"very_fine"`, `"organic"`).

- value:

  The factor.

- unit:

  Its unit.

- source:

  Locator of the value.

## Source

Beusen, A. H. W., Van Beek, L. P. H., Bouwman, A. F. et al. (2015).
Coupling global models for hydrology and nutrient loading to simulate
nitrogen and phosphorus retention in surface water - description of
IMAGE-GNM and analysis of performance. Geosci. Model Dev. 8, 4045-4067.
[doi:10.5194/gmd-8-4045-2015](https://doi.org/10.5194/gmd-8-4045-2015) .
Table 3 (p. 4056) for `runoff_land_use` and p. 4052 for `erosion_cn`.
The authors' model code and inputs published with the paper,
[doi:10.5194/gmd-8-4045-2015-supplement](https://doi.org/10.5194/gmd-8-4045-2015-supplement)
: `N_model/trunk/tools/parameters_history_100.ini` lines 112-121 and
213-215, and `fix_input/texture_erosion.vat`.

## Examples

``` r
image_gnm_runoff_factors
#> # A tibble: 14 × 5
#>    factor          class        value unit                                source
#>    <chr>           <chr>        <dbl> <chr>                               <chr> 
#>  1 runoff_land_use Cropland     1     dimensionless                       Beuse…
#>  2 runoff_land_use Grassland    0.25  dimensionless                       Beuse…
#>  3 runoff_land_use Natural      0.125 dimensionless                       Beuse…
#>  4 erosion_texture coarse       0.5   dimensionless                       IMAGE…
#>  5 erosion_texture medium       1.5   dimensionless                       IMAGE…
#>  6 erosion_texture fine         1.5   dimensionless                       IMAGE…
#>  7 erosion_texture very_fine    0.5   dimensionless                       IMAGE…
#>  8 erosion_texture organic      0.5   dimensionless                       IMAGE…
#>  9 soil_loss       Cropland  1151     t soil km-2 yr-1 per unit erodibil… IMAGE…
#> 10 soil_loss       Grassland   99     t soil km-2 yr-1 per unit erodibil… IMAGE…
#> 11 soil_loss       Natural     29     t soil km-2 yr-1 per unit erodibil… IMAGE…
#> 12 erosion_cn      Cropland    12     g C g-1 N                           Beuse…
#> 13 erosion_cn      Grassland   14     g C g-1 N                           Beuse…
#> 14 erosion_cn      Natural     14     g C g-1 N                           Beuse…
```
