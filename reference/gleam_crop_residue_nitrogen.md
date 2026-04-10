# Nitrogen parameters for crop residues of feed materials.

Nitrogen content of above- and below-ground residues and root-to-shoot
ratios for feed materials.

## Usage

``` r
gleam_crop_residue_nitrogen
```

## Format

A tibble with columns:

- material_number:

  Sequential material identifier.

- material:

  Feed material code.

- n_ag:

  Nitrogen content of above-ground residues.

- rbg_bio:

  Ratio of below-ground residues to above-ground biomass.

- n_bg:

  Nitrogen content of below-ground residues.

- species_group:

  `"ruminant"` or `"monogastric"`.

## Source

MacLeod et al. (2018) GLEAM 3.0 Supplement S1, Tables S.6.7 and S.6.8.

## Examples

``` r
gleam_crop_residue_nitrogen
#> # A tibble: 68 × 6
#>    material_number material   n_ag rbg_bio  n_bg species_group
#>              <int> <chr>     <dbl>   <dbl> <dbl> <chr>        
#>  1               1 GRASSF    0.015    0.8  0.012 ruminant     
#>  2               2 GRASSH    0.015    0.8  0.012 ruminant     
#>  3               3 GRASSH2   0.015    0.8  0.012 ruminant     
#>  4               4 GRASSLEGF 0.027    0.4  0.022 ruminant     
#>  5               5 GRASSLEGH 0.027    0.4  0.022 ruminant     
#>  6               6 FDDRSIL   0.027    0.1  0.019 ruminant     
#>  7               7 RSTRAW    0.007    0.16 0.007 ruminant     
#>  8               8 WSTRAW    0.006    0.24 0.009 ruminant     
#>  9               9 BSTRAW    0.007    0.22 0.014 ruminant     
#> 10              10 ZSTOVER   0.006    0.22 0.007 ruminant     
#> # ℹ 58 more rows
```
