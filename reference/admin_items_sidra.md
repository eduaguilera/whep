# IBGE SIDRA crop classes and their WHEP items

The 72 categories of classification 782 of SIDRA table 5457 (Producao
Agricola Municipal), each mapped to its WHEP `item_prod_code` or
recorded as dropped. Category `"0"` is the all-crops total and is
dropped as such. See
[admin_source_vocabularies](https://eduaguilera.github.io/whep/reference/admin_source_vocabularies.md)
for the columns, the mapping kinds and the rules encoded here.

## Usage

``` r
admin_items_sidra
```

## Format

A tibble with one row per SIDRA crop category and the eight
`admin_items_*` columns of
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
head(admin_items_sidra)
#> # A tibble: 6 × 8
#>   source   source_table class_key source_class_code source_class_name          
#>   <chr>    <chr>        <chr>     <chr>             <chr>                      
#> 1 IBGE_PAM 5457         0         0                 Total                      
#> 2 IBGE_PAM 5457         40129     40129             Abacate                    
#> 3 IBGE_PAM 5457         40092     40092             Abacaxi*                   
#> 4 IBGE_PAM 5457         45982     45982             Açaí                       
#> 5 IBGE_PAM 5457         40329     40329             Alfafa fenada              
#> 6 IBGE_PAM 5457         40130     40130             Algodão arbóreo (em caroço)
#> # ℹ 3 more variables: item_prod_code <chr>, mapping_kind <chr>,
#> #   mapping_reason <chr>
```
