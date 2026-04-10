# Processing and transport emission factors for feeds.

Emission factors for processing and transport of feed materials, for
ruminant and monogastric species.

## Usage

``` r
gleam_processing_transport_ef
```

## Format

A tibble with columns:

- material_number:

  Sequential material identifier.

- material:

  Feed material code.

- processing_g_co2eq_kg_dm:

  Processing emission factor in g CO2-eq per kg dry matter.

- transport_g_co2eq_kg_dm:

  Transport emission factor in g CO2-eq per kg dry matter.

- species_group:

  `"ruminant"` or `"monogastric"`.

## Source

MacLeod et al. (2018) GLEAM 3.0 Supplement S1, Tables S.6.5 and S.6.6.

## Examples

``` r
gleam_processing_transport_ef
#> # A tibble: 68 × 5
#>    material_number material  processing_g_co2eq_kg_dm transport_g_co2eq_kg_dm
#>              <int> <chr>                        <dbl>                   <dbl>
#>  1               1 GRASSF                         0                         0
#>  2               2 GRASSH                        15.4                       0
#>  3               3 GRASSH2                       15.4                       0
#>  4               4 GRASSLEGF                      0                         0
#>  5               5 GRASSLEGH                     15.4                       0
#>  6               6 FDDRSIL                       15.4                       0
#>  7               7 RSTRAW                         0                         0
#>  8               8 WSTRAW                         0                         0
#>  9               9 BSTRAW                         0                         0
#> 10              10 ZSTOVER                        0                         0
#> # ℹ 58 more rows
#> # ℹ 1 more variable: species_group <chr>
```
