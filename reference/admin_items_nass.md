# USDA NASS crop series and their WHEP items

The Quick Stats `SHORT_DESC` crop series the subnational spatialization
reads, each mapped to its WHEP `item_prod_code`. See
[admin_source_vocabularies](https://eduaguilera.github.io/whep/reference/admin_source_vocabularies.md)
for the columns, the mapping kinds and the rules encoded here.

## Usage

``` r
admin_items_nass
```

## Format

A tibble with one row per NASS series and the eight `admin_items_*`
columns of
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
head(admin_items_nass)
#> # A tibble: 6 × 8
#>   source    source_table class_key           source_class_code source_class_name
#>   <chr>     <chr>        <chr>               <chr>             <chr>            
#> 1 USDA_NASS qs.crops     CORN, GRAIN - ACRE… NA                CORN, GRAIN - AC…
#> 2 USDA_NASS qs.crops     CORN, SILAGE - ACR… NA                CORN, SILAGE - A…
#> 3 USDA_NASS qs.crops     WHEAT - ACRES HARV… NA                WHEAT - ACRES HA…
#> 4 USDA_NASS qs.crops     BARLEY - ACRES HAR… NA                BARLEY - ACRES H…
#> 5 USDA_NASS qs.crops     OATS - ACRES HARVE… NA                OATS - ACRES HAR…
#> 6 USDA_NASS qs.crops     SOYBEANS - ACRES H… NA                SOYBEANS - ACRES…
#> # ℹ 3 more variables: item_prod_code <chr>, mapping_kind <chr>,
#> #   mapping_reason <chr>
```
