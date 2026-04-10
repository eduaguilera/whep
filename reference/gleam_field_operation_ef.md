# Emission factors for field operations on feed materials.

CO2-equivalent emissions per hectare from field operations for ruminant
and monogastric feed materials.

## Usage

``` r
gleam_field_operation_ef
```

## Format

A tibble with columns:

- material_number:

  Sequential material identifier.

- material:

  Feed material code (e.g. `"GRASSF"`, `"WHEAT"`).

- emission_factor_kg_co2eq_ha:

  Emission factor in kg CO2-eq per hectare.

- species_group:

  `"ruminant"` or `"monogastric"`.

## Source

MacLeod et al. (2018) GLEAM 3.0 Supplement S1, Tables S.6.1 and S.6.2.

## Examples

``` r
gleam_field_operation_ef
#> # A tibble: 68 × 4
#>    material_number material  emission_factor_kg_co2eq_ha species_group
#>              <int> <chr>                           <dbl> <chr>        
#>  1               1 GRASSF                             96 ruminant     
#>  2               2 GRASSH                            353 ruminant     
#>  3               3 GRASSH2                           353 ruminant     
#>  4               4 GRASSLEGF                         204 ruminant     
#>  5               5 GRASSLEGH                         340 ruminant     
#>  6               6 FDDRSIL                           413 ruminant     
#>  7               7 RSTRAW                           9200 ruminant     
#>  8               8 WSTRAW                            213 ruminant     
#>  9               9 BSTRAW                            278 ruminant     
#> 10              10 ZSTOVER                           505 ruminant     
#> # ℹ 58 more rows
```
