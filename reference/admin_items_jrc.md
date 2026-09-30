# JRC subnational crop classes and their WHEP items

The nine crop classes of release 2025.01 of the JRC harmonised EU
subnational crop statistics. Two of them, `"Total wheat"` and
`"Total barley"`, are aggregates of members the same release also
publishes, so summing all nine would double-count wheat and barley; the
aggregates bind and their members are recorded but never summed
alongside them. See
[admin_source_vocabularies](https://eduaguilera.github.io/whep/reference/admin_source_vocabularies.md)
for the columns, the mapping kinds and the rules encoded here.

## Usage

``` r
admin_items_jrc
```

## Format

A tibble with one row per JRC crop class and the eight `admin_items_*`
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
head(admin_items_jrc)
#> # A tibble: 6 × 8
#>   source              source_table class_key source_class_code source_class_name
#>   <chr>               <chr>        <chr>     <chr>             <chr>            
#> 1 JRC_subnational_cr… EU27_CROP_S… Total wh… NA                Total wheat      
#> 2 JRC_subnational_cr… EU27_CROP_S… Soft whe… NA                Soft wheat       
#> 3 JRC_subnational_cr… EU27_CROP_S… Durum wh… NA                Durum wheat      
#> 4 JRC_subnational_cr… EU27_CROP_S… Total ba… NA                Total barley     
#> 5 JRC_subnational_cr… EU27_CROP_S… Winter b… NA                Winter barley    
#> 6 JRC_subnational_cr… EU27_CROP_S… Spring b… NA                Spring barley    
#> # ℹ 3 more variables: item_prod_code <chr>, mapping_kind <chr>,
#> #   mapping_reason <chr>
```
