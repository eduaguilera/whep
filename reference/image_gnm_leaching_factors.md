# IMAGE-GNM soil denitrification and land-use leaching factors.

The class factors of IMAGE-GNM's root-zone leaching fraction (Beusen et
al. 2015, eq. 11, p. 4052),
`f_leach = [1 - min(f_climate + f_text + f_drain + f_soc, 1)] f_landuse`,
read by
[`calculate_n_leaching()`](https://eduaguilera.github.io/whep/reference/calculate_n_leaching.md)
with `method = "image_gnm"`: the denitrification fractions for soil
texture (`f_text`), soil drainage (`f_drain`) and soil organic carbon
content (`f_soc`) of Table 2 (p. 4053), and the land-use factor
`f_landuse` (p. 4052). Table 2 is transcribed as printed. Its drainage
column names five classes; the FAO classes it does not name, well
drained and somewhat excessively drained, take 0 from the authors' model
code, which books every well-drained class at 0. The organic-carbon
classes are half-open, `soc_min_pct <= SOC < soc_max_pct`, as in the
authors' class code; the table gives no numeric bound for its Organic
class, which here starts where the 6-50\\

## Usage

``` r
image_gnm_leaching_factors
```

## Format

A tibble with columns:

- factor:

  Which factor the row gives: `"texture"`, `"drainage"`, `"soc"` or
  `"land_use"`.

- class:

  The class key
  [`calculate_n_leaching()`](https://eduaguilera.github.io/whep/reference/calculate_n_leaching.md)
  matches: texture `"coarse"`, `"medium"`, `"fine"`, `"very_fine"`,
  `"organic"`; drainage `"excessively_drained"`,
  `"somewhat_excessively_drained"`, `"well_drained"`,
  `"moderately_well_drained"`, `"imperfectly_drained"`,
  `"poorly_drained"`, `"very_poorly_drained"`; organic carbon
  `"below_1_pct"`, `"1_to_3_pct"`, `"3_to_6_pct"`, `"6_to_50_pct"`,
  `"organic"`; land use `"Cropland"`, `"Grassland"`, `"Natural"`.

- label:

  The class as the source prints it (`NA` for the two drainage classes
  taken from the authors' code).

- soc_min_pct, soc_max_pct:

  Bounds of an organic-carbon class, \\ by weight (`NA` on other rows).

- value:

  The factor (dimensionless).

- source:

  Locator of the value.

## Source

Beusen, A. H. W., Van Beek, L. P. H., Bouwman, A. F. et al. (2015).
Coupling global models for hydrology and nutrient loading to simulate
nitrogen and phosphorus retention in surface water - description of
IMAGE-GNM and analysis of performance. Geosci. Model Dev. 8, 4045-4067.
[doi:10.5194/gmd-8-4045-2015](https://doi.org/10.5194/gmd-8-4045-2015) .
Table 2 (p. 4053, after Van Drecht et al. 2003) for `f_text`, `f_drain`
and `f_soc`; p. 4052 for `f_landuse` (1 for arable land, 0.36 for
grassland and natural vegetation, citing Keuskamp, J. A., Van Drecht, G.
& Bouwman, A. F. 2012, Environ. Pollut. 165, 67-76,
[doi:10.1016/j.envpol.2012.02.008](https://doi.org/10.1016/j.envpol.2012.02.008)
). The well-drained and somewhat excessively drained classes: the
authors' model code published with the paper,
`n_soil_denitrification.py` (in `N_model/trunk/tools/`),
`fr_Ndenitrification_drainage()`, lines 163-165.

## Examples

``` r
image_gnm_leaching_factors
#> # A tibble: 20 × 7
#>    factor   class                     label soc_min_pct soc_max_pct value source
#>    <chr>    <chr>                     <chr>       <dbl>       <dbl> <dbl> <chr> 
#>  1 texture  coarse                    Coar…          NA          NA  0    Beuse…
#>  2 texture  medium                    Medi…          NA          NA  0.1  Beuse…
#>  3 texture  fine                      Fine           NA          NA  0.2  Beuse…
#>  4 texture  very_fine                 Very…          NA          NA  0.3  Beuse…
#>  5 texture  organic                   Orga…          NA          NA  0    Beuse…
#>  6 drainage excessively_drained       Exce…          NA          NA  0    Beuse…
#>  7 drainage moderately_well_drained   Mode…          NA          NA  0.1  Beuse…
#>  8 drainage imperfectly_drained       Impe…          NA          NA  0.2  Beuse…
#>  9 drainage poorly_drained            Poor…          NA          NA  0.3  Beuse…
#> 10 drainage very_poorly_drained       Very…          NA          NA  0.4  Beuse…
#> 11 drainage well_drained              NA             NA          NA  0    IMAGE…
#> 12 drainage somewhat_excessively_dra… NA             NA          NA  0    IMAGE…
#> 13 soc      below_1_pct               <1%             0           1  0    Beuse…
#> 14 soc      1_to_3_pct                1-3%            1           3  0.1  Beuse…
#> 15 soc      3_to_6_pct                3-6%            3           6  0.2  Beuse…
#> 16 soc      6_to_50_pct               6-50%           6          50  0.3  Beuse…
#> 17 soc      organic                   Orga…          50         Inf  0.3  Beuse…
#> 18 land_use Cropland                  arab…          NA          NA  1    Beuse…
#> 19 land_use Grassland                 gras…          NA          NA  0.36 Beuse…
#> 20 land_use Natural                   natu…          NA          NA  0.36 Beuse…
```
