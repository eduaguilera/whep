# Eurostat livestock classes and their WHEP species groups

The 47 classes of the `animals` dimension of `apro_mt_ls_r` and the nine
of `ef_lsk_poultry`, each mapped to its WHEP `species_group`, to the set
of groups whose sum it constrains, or recorded as a member of a binding
aggregate. See
[admin_source_vocabularies](https://eduaguilera.github.io/whep/reference/admin_source_vocabularies.md)
for the columns, the mapping kinds and the rules encoded here.

## Usage

``` r
admin_species_eurostat
```

## Format

A tibble with one row per Eurostat animal class and the nine
`admin_species_*` columns of
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
head(admin_species_eurostat)
#> # A tibble: 6 × 9
#>   source   source_table class_key source_class_code source_class_name           
#>   <chr>    <chr>        <chr>     <chr>             <chr>                       
#> 1 Eurostat apro_mt_ls_r A2000     A2000             Live bovine animals         
#> 2 Eurostat apro_mt_ls_r A2010     A2010             Bovine animals, less than 1…
#> 3 Eurostat apro_mt_ls_r A2010B    A2010B            Bovine animals, less than 1…
#> 4 Eurostat apro_mt_ls_r A2010C    A2010C            Bovine animals, less than 1…
#> 5 Eurostat apro_mt_ls_r A2020     A2020             Bovine animals, 1 to less t…
#> 6 Eurostat apro_mt_ls_r A2030     A2030             Bovine animals, 2 years old…
#> # ℹ 4 more variables: species_group <chr>, constrains <chr>,
#> #   mapping_kind <chr>, mapping_reason <chr>
```
