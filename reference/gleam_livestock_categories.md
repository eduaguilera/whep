# GLEAM livestock categories.

Species, production systems, and cohort definitions from GLEAM 3.0.

## Usage

``` r
gleam_livestock_categories
```

## Format

A tibble with columns:

- species:

  Animal species.

- production_system:

  Dairy, Beef, Meat, etc.

- cohort:

  Age/sex cohort.

- description:

  Cohort description.

## Source

Partly traced (whep#881). The cohort vocabulary corresponds to FAO.
2022. *Global Livestock Environmental Assessment Model, Model
Description, Version 3.0*. Rome, FAO, Table 2.1 "Summary of cohorts in
GLEAM", p. 10 (document code `cd8425en`, served from `www.fao.org/3/`
and `openknowledge.fao.org`), identical to Table 2.1 p. 9 of the Version
2.0 Revision 5 description
(<https://www.fao.org/fileadmin/user_upload/gleam/docs/GLEAM_2.0_Model_description.pdf>).
The shipped table is **not** a transcription of it: GLEAM names cohorts
AF/RF/AM/RM/MF/MM (plus MFr/MMr/MFf/MMf for feedlots) within one herd,
whereas this table renames them, crosses them with a
Dairy/Beef/Meat/Other production system GLEAM's Table 2.1 does not have,
and supplies its own `description` strings. GLEAM publishes no table of
cohort shares; it derives herd structure from the replacement,
fertility, mortality and age-at-first-calving rates in Supplement S1 of
the Version 2.0 description.
[`calculate_cohorts_systems()`](https://eduaguilera.github.io/whep/reference/calculate_cohorts_systems.md)
instead splits a herd equally, `1 / n` per row of this table, so the row
COUNT per (species, production system) is a result-affecting, unsourced
assumption: Cattle Dairy 6 cohorts (16.7% each), Cattle Beef 5 (20%
each). Treat the layout and the implied equal split as unverified.

## Examples

``` r
gleam_livestock_categories
#> # A tibble: 27 × 4
#>    species production_system cohort             description    
#>    <chr>   <chr>             <chr>              <chr>          
#>  1 Cattle  Dairy             Adult Female       Milking cows   
#>  2 Cattle  Dairy             Adult Male         Bulls          
#>  3 Cattle  Dairy             Replacement Female Heifers        
#>  4 Cattle  Dairy             Replacement Male   Young bulls    
#>  5 Cattle  Dairy             Surplus Female     Culled heifers 
#>  6 Cattle  Dairy             Surplus Male       Calves for meat
#>  7 Cattle  Beef              Adult Female       Breeding cows  
#>  8 Cattle  Beef              Adult Male         Bulls          
#>  9 Cattle  Beef              Replacement Female Heifers        
#> 10 Cattle  Beef              Replacement Male   Young bulls    
#> # ℹ 17 more rows
```
