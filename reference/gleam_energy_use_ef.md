# Energy use emission factors by species and system.

Emission factors for embedded and direct energy use in livestock
production, by species, production system, and climate zone.

## Usage

``` r
gleam_energy_use_ef
```

## Format

A tibble in long format with columns:

- grouping:

  Country or country group (e.g. `"OECD"`, `"EU 27"`).

- species:

  Animal species or group (e.g. `"dairy_cattle"`, `"pigs"`).

- system:

  Production system (e.g. `"Grassland based"`, `"industrial"`). `NA`
  when not applicable.

- climate:

  Climate zone (`"arid"`, `"humid"`, `"temperate"`). `NA` when not
  applicable.

- energy_type:

  `"embedded"` or `"direct"`.

- emission_factor:

  Emission factor in kg CO2-eq per unit product.

## Source

MacLeod et al. (2018) GLEAM 3.0 Supplement S1, Tables S.7.1 through
S.7.7.

## Examples

``` r
gleam_energy_use_ef
#> # A tibble: 211 × 6
#>    grouping                  species  system climate energy_type emission_factor
#>    <chr>                     <chr>    <chr>  <chr>   <chr>                 <dbl>
#>  1 OECD                      dairy_c… Grass… arid    embedded              0.182
#>  2 OECD                      dairy_c… Grass… humid   embedded              0.209
#>  3 OECD                      dairy_c… Grass… temper… embedded              0.371
#>  4 OECD                      dairy_c… Mixed… arid    embedded              0.211
#>  5 OECD                      dairy_c… Mixed… humid   embedded              0.241
#>  6 OECD                      dairy_c… Mixed… temper… embedded              0.398
#>  7 Least developed countries dairy_c… Grass… arid    embedded              0.02 
#>  8 Least developed countries dairy_c… Grass… humid   embedded              0.02 
#>  9 Least developed countries dairy_c… Grass… temper… embedded              0.02 
#> 10 Least developed countries dairy_c… Mixed… arid    embedded              0.033
#> # ℹ 201 more rows
```
