# GLEAM feed digestibility for ruminants.

Nutritional values for feed materials of ruminant species, including
gross energy, nitrogen content, and digestibility.

## Usage

``` r
gleam_feed_digestibility
```

## Format

A tibble with columns:

- number:

  Feed material number.

- material:

  Feed material code.

- gross_energy_mj_kg:

  Gross energy (MJ per kg DM).

- n_content_g_kg:

  Nitrogen content (g per kg DM).

- digestibility_pct:

  Digestibility (percent).

## Source

MacLeod et al. (2018) GLEAM 3.0 Supplement S1, Table S.3.3.

## Examples

``` r
gleam_feed_digestibility
#> # A tibble: 27 × 5
#>    number material  gross_energy_mj_kg n_content_g_kg digestibility_pct
#>     <int> <chr>                  <dbl>          <dbl>             <dbl>
#>  1      1 GRASSF                  17.8          22                 66  
#>  2      2 GRASSH                  17.8          17                 58  
#>  3      3 GRASSH2                 17.8          17                 58  
#>  4      4 GRASSLEGF               18.1          31                 67  
#>  5      5 GRASSLEGH               18.1          26                 58  
#>  6      6 FDDRSIL                 18.2          21                 63  
#>  7      7 RSTRAW                  15.5           6                 46.5
#>  8      8 WSTRAW                  18.5           6                 45.2
#>  9      9 BSTRAW                  18.3           6.67              44.4
#> 10     10 ZSTOVER                 18            10                 57.6
#> # ℹ 17 more rows
```
