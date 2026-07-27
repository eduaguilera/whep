# HWSD topsoil USDA texture code to texture-class crosswalk.

Maps the HWSD (Harmonized World Soil Database) topsoil USDA texture code
`t_usda_tex` (the `TEXTURE_USDA` field, integers 1 to 13) to the
canonical USDA texture-class name keying
[soil_hydraulic_by_texture](https://eduaguilera.github.io/whep/reference/soil_hydraulic_by_texture.md).
[`read_soil_hydraulic()`](https://eduaguilera.github.io/whep/reference/read_soil_hydraulic.md)
uses it to translate each grid cell's dominant HWSD texture code into
its hydraulic properties. HWSD splits clay into a heavy-clay (code 1)
and a light-clay (code 3) class; both map to the single USDA `"clay"`
class of the standard 12-class system, matching how the WHEP HWSD
preparation pipeline collapses them.

## Usage

``` r
hwsd_texture_usda
```

## Format

A tibble with columns:

- t_usda_tex:

  HWSD topsoil USDA texture code (integer 1 to 13).

- usda_texture_class:

  Canonical USDA texture class (snake_case), one of the classes in
  [soil_hydraulic_by_texture](https://eduaguilera.github.io/whep/reference/soil_hydraulic_by_texture.md).

## Source

HWSD topsoil USDA texture-code legend as documented for the local HWSD
extract in the WHEP spatial-input preparation pipeline
(`inst/scripts/prepare_spatialize_all.R`), following the Harmonized
World Soil Database version 2.0 `D_TEXTURE_USDA` class ordering. FAO &
IIASA (2023). *Harmonized World Soil Database version 2.0*. Rome and
Laxenburg. [doi:10.4060/cc3823en](https://doi.org/10.4060/cc3823en) .

## Examples

``` r
hwsd_texture_usda
#> # A tibble: 13 × 2
#>    t_usda_tex usda_texture_class
#>         <int> <chr>             
#>  1          1 clay              
#>  2          2 silty_clay        
#>  3          3 clay              
#>  4          4 silty_clay_loam   
#>  5          5 clay_loam         
#>  6          6 silt              
#>  7          7 silt_loam         
#>  8          8 sandy_clay        
#>  9          9 loam              
#> 10         10 sandy_clay_loam   
#> 11         11 sandy_loam        
#> 12         12 loamy_sand        
#> 13         13 sand              
```
