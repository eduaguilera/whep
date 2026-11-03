# Read IMAGE-GNM's surface runoff fraction and slope layers.

Reads the two 0.5-degree layers IMAGE-GNM's surface runoff nitrogen
needs, as its authors published them with Beusen et al. (2015)
(doi:10.5194/gmd-8-4045-2015-supplement):

- `surface_runoff_fraction`: `f_qsro`, the share of total runoff that
  runs off the surface, the product `f_qsro(slope) f_qsro(texture)` of
  eqs. 3-4 (p. 4049), the median over the 90 m cells of each 0.5-degree
  cell. Their model reads it from `water_input/pcrglobwb_100/fQsro.map`
  rather than computing eq. 4, and it is read here the same way; the
  land-use factor of eq. 3 is applied by
  [`calculate_n_leaching()`](https://eduaguilera.github.io/whep/reference/calculate_n_leaching.md).

- `slope_m_per_km`: the median slope (m/km) of each cell, from
  `fix_input/slope.map`, which their erosion term reads.

Only cells where `fQsro.map` holds a value (its land mask) are returned.
A balance row on a cell outside it gets no value from a join and
[`calculate_n_leaching()`](https://eduaguilera.github.io/whep/reference/calculate_n_leaching.md)
aborts there rather than run nothing off.

The supplement is resolved from `dir`, else the `WHEP_IMAGE_GNM_DIR`
environment variable, else a local cache populated on first use: the
59.7 MB archive is downloaded from its DOI, verified against its SHA-256
and the two layers unpacked. The archive's publisher prints no checksum,
so the SHA-256 is the one WHEP recorded on 2026-10-12;
`inst/scripts/download/download_image_gnm.R` fetches the same archive
into a directory for `WHEP_IMAGE_GNM_DIR`.

## Usage

``` r
read_image_gnm_runoff(dir = NULL, verify = TRUE, example = FALSE)
```

## Source

Beusen, A. H. W., Van Beek, L. P. H., Bouwman, A. F., Mogollon, J. M.
and Middelburg, J. J. (2015). Coupling global models for hydrology and
nutrient loading to simulate nitrogen and phosphorus retention in
surface water - description of IMAGE-GNM and analysis of performance.
Geosci. Model Dev. 8, 4045-4067.
[doi:10.5194/gmd-8-4045-2015](https://doi.org/10.5194/gmd-8-4045-2015) ;
supplement
[doi:10.5194/gmd-8-4045-2015-supplement](https://doi.org/10.5194/gmd-8-4045-2015-supplement)
.

## Arguments

- dir:

  Optional directory holding the unpacked supplement (the folder that
  contains `GMD_beusen_supplement/`), overriding `WHEP_IMAGE_GNM_DIR`.
  Defaults to `NULL`.

- verify:

  If `TRUE` (default), each layer is checked against its recorded size
  and SHA-256 before it is read.

- example:

  If `TRUE`, return a small fixture instead of reading data. Defaults to
  `FALSE`.

## Value

A tibble with `lon`, `lat` (0.5-degree cell centres),
`surface_runoff_fraction` (0-1), `slope_m_per_km` and
`source_image_gnm`, the supplement's DOI.

## Examples

``` r
read_image_gnm_runoff(example = TRUE)
#> # A tibble: 8 × 5
#>      lon   lat surface_runoff_fraction slope_m_per_km source_image_gnm          
#>    <dbl> <dbl>                   <dbl>          <dbl> <chr>                     
#> 1   5.25 52.2                   0.0136           1.21 doi:10.5194/gmd-8-4045-20…
#> 2  10.2  46.2                   0.525          221.   doi:10.5194/gmd-8-4045-20…
#> 3 -93.2  41.8                   0.0532           6.02 doi:10.5194/gmd-8-4045-20…
#> 4  -3.75 40.2                   0.112           11.1  doi:10.5194/gmd-8-4045-20…
#> 5  31.2  30.2                   0.0339           3.15 doi:10.5194/gmd-8-4045-20…
#> 6 100.   30.2                   0.522          115.   doi:10.5194/gmd-8-4045-20…
#> 7  78.2  22.2                   0.289           26.1  doi:10.5194/gmd-8-4045-20…
#> 8 -60.2  -3.25                  0.0486           3.66 doi:10.5194/gmd-8-4045-20…
```
