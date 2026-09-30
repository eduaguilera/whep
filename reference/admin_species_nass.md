# USDA NASS livestock series and their WHEP species groups

The Quick Stats inventory series the subnational spatialization reads,
each mapped to its WHEP `species_group` or to the set of groups whose
sum it constrains. See
[admin_source_vocabularies](https://eduaguilera.github.io/whep/reference/admin_source_vocabularies.md)
for the columns, the mapping kinds and the rules encoded here.

## Usage

``` r
admin_species_nass
```

## Format

A tibble with one row per NASS series and the nine `admin_species_*`
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
head(admin_species_nass)
#> # A tibble: 4 × 9
#>   source    source_table        class_key    source_class_code source_class_name
#>   <chr>     <chr>               <chr>        <chr>             <chr>            
#> 1 USDA_NASS qs.animals_products CATTLE, INC… NA                CATTLE, INCL CAL…
#> 2 USDA_NASS qs.animals_products CATTLE, COW… NA                CATTLE, COWS, MI…
#> 3 USDA_NASS qs.animals_products HOGS - INVE… NA                HOGS - INVENTORY 
#> 4 USDA_NASS qs.animals_products SHEEP, INCL… NA                SHEEP, INCL LAMB…
#> # ℹ 4 more variables: species_group <chr>, constrains <chr>,
#> #   mapping_kind <chr>, mapping_reason <chr>
```
