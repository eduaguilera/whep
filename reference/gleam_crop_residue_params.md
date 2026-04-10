# GLEAM crop residue parameters.

Dry matter content and parameters for calculating crop residue yield by
crop type.

## Usage

``` r
gleam_crop_residue_params
```

## Format

A tibble with columns:

- crop:

  Crop name.

- dry_matter_pct:

  Dry matter content (percent).

- slope:

  Slope for residue yield calculation.

- intercept:

  Intercept for residue yield calculation.

## Source

MacLeod et al. (2018) GLEAM 3.0 Supplement S1, Table S.3.1.
[doi:10.1088/1748-9326/aad4d8](https://doi.org/10.1088/1748-9326/aad4d8)

## Examples

``` r
gleam_crop_residue_params
#> # A tibble: 19 × 4
#>    crop                                           dry_matter_pct slope intercept
#>    <chr>                                                   <dbl> <dbl>     <dbl>
#>  1 Grass                                                    90    0.25      0   
#>  2 Banana fruits                                            20    1         0   
#>  3 Barley                                                   89    0.98    590   
#>  4 Cotton                                                   93.5  0.93   1350   
#>  5 Fodder                                                   89    0.3       0   
#>  6 Millet                                                   90    1.43    140   
#>  7 Maize                                                    87    1.03    610   
#>  8 Other cereals (excluding wheat, rice, maize, …           89    0.98    590   
#>  9 Oil palm fruit                                           53    0         0   
#> 10 Pulses                                                   91    1.13    850   
#> 11 Rice                                                     89    0.95   2460   
#> 12 Rapeseed                                                 92.3  0.28      0   
#> 13 Cassava                                                  33    0.1    1060   
#> 14 Sunflower                                                92.3  0.95   1350   
#> 15 Soybean                                                  91    0.93   1350   
#> 16 Sorghum                                                  89    0.88   1330   
#> 17 Sugar beet                                               25    0.1       1.06
#> 18 Sugarcane                                                32    0.28      0   
#> 19 Wheat                                                    89    1.51    520   
```
