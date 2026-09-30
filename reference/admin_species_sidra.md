# IBGE SIDRA herd types and their WHEP species groups

The ten herd types of classification 79 of SIDRA table 3939 (Pesquisa da
Pecuaria Municipal), plus the milked-cow series of table 94, which is
the only dairy split IBGE publishes for that herd. See
[admin_source_vocabularies](https://eduaguilera.github.io/whep/reference/admin_source_vocabularies.md)
for the columns, the mapping kinds and the rules encoded here.

## Usage

``` r
admin_species_sidra
```

## Format

A tibble with one row per SIDRA herd type, plus one for table 94, and
the nine `admin_species_*` columns of
[admin_source_vocabularies](https://eduaguilera.github.io/whep/reference/admin_source_vocabularies.md).

## Source

Eurostat dissemination API
(<https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1>), dataflows
`apro_cpshr`, `apro_cpnhr_h`, `apro_mt_ls_r` and `ef_lsk_poultry`; IBGE
SIDRA aggregate metadata
(<https://servicodados.ibge.gov.br/api/v3/agregados>), tables 5457, 3939
and 94; Ronchetti, G. et al. (2024). Harmonized European Union
subnational crop statistics reveal climate impacts and crop cultivation
shifts. *Earth System Science Data*, 16, 1623-1649.
[doi:10.5194/essd-16-1623-2024](https://doi.org/10.5194/essd-16-1623-2024)
; USDA National Agricultural Statistics Service, Quick Stats bulk
downloads (<https://www.nass.usda.gov/datasets/>).

## See also

[admin_source_vocabularies](https://eduaguilera.github.io/whep/reference/admin_source_vocabularies.md)

## Examples

``` r
head(admin_species_sidra)
#> # A tibble: 6 × 9
#>   source   source_table class_key source_class_code source_class_name         
#>   <chr>    <chr>        <chr>     <chr>             <chr>                     
#> 1 IBGE_PPM 3939         2670      2670              Bovino                    
#> 2 IBGE_PPM 3939         2675      2675              Bubalino                  
#> 3 IBGE_PPM 3939         2672      2672              Equino                    
#> 4 IBGE_PPM 3939         32794     32794             Suíno - total             
#> 5 IBGE_PPM 3939         32795     32795             Suíno - matrizes de suínos
#> 6 IBGE_PPM 3939         2681      2681              Caprino                   
#> # ℹ 4 more variables: species_group <chr>, constrains <chr>,
#> #   mapping_kind <chr>, mapping_reason <chr>
```
