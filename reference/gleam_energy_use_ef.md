# Energy use emission factors for livestock production.

Emission factors for embedded (feed-production) and direct (on-farm)
energy use in livestock production, from GLEAM 3.0 tables S.7.1 through
S.7.7. Note that the factors are expressed per kilogram of *live
weight*, *milk* or *egg* depending on the species and herd: see the
`denominator` column. The GLEAM footnotes are materialised as derived
rows: embedded energy for meat (non-dairy) cattle and all buffalo is
half of dairy cattle (S.7.1 note a); embedded energy for non-dairy small
ruminants is half of the listed values (S.7.2 note a); direct energy for
dairy small ruminants is double the dairy cattle values (S.7.5 note a).

## Usage

``` r
gleam_energy_use_ef
```

## Format

A tibble in long format with columns:

- species:

  Animal species or group (`"cattle"`, `"buffalo"`, `"large_ruminants"`,
  `"small_ruminants"`, `"pigs"`, `"chickens"`).

- herd:

  Herd or product line (`"dairy"`, `"non_dairy"`, `"broilers"`,
  `"layers"`, `"all"`). `NA` for pigs.

- grouping:

  Country or country group as reported by GLEAM (e.g. `"OECD"`,
  `"EU 27"`, `"Least developed countries"`).

- grouping_scheme:

  Which country grouping the `grouping` belongs to: `"development3"`
  (OECD / least developed / others), `"region5"` (OECD / four non-OECD
  regions) or `"detailed15"` (individual OECD members plus world
  regions).

- system:

  Production system (e.g. `"grassland_based"`, `"industrial"`). `NA`
  when not applicable.

- climate:

  Climate zone (`"arid"`, `"humid"`, `"temperate"`). `NA` when not
  applicable.

- energy_type:

  `"embedded"` or `"direct"`.

- denominator:

  Reporting basis of the factor: `"lw"` (live weight), `"milk"` or
  `"egg"`.

- emission_factor:

  Emission factor in kg CO2-eq per kg of the `denominator`.

## Source

FAO (2022) GLEAM version 3.0, Supplement S1 (an FAO workbook; no DOI is
issued for it), Tables S.7.1 through S.7.7:
<https://www.fao.org/fileadmin/user_upload/gleam/docs/GLEAM_3.0_Supplement_S1.xlsx>

## Examples

``` r
gleam_energy_use_ef
#> # A tibble: 361 × 9
#>    species herd  grouping grouping_scheme system climate energy_type denominator
#>    <chr>   <chr> <chr>    <chr>           <chr>  <chr>   <chr>       <chr>      
#>  1 buffalo all   Least d… development3    grass… arid    embedded    lw         
#>  2 buffalo all   Least d… development3    grass… humid   embedded    lw         
#>  3 buffalo all   Least d… development3    grass… temper… embedded    lw         
#>  4 buffalo all   Least d… development3    mixed… arid    embedded    lw         
#>  5 buffalo all   Least d… development3    mixed… humid   embedded    lw         
#>  6 buffalo all   Least d… development3    mixed… temper… embedded    lw         
#>  7 buffalo all   OECD     development3    grass… arid    embedded    lw         
#>  8 buffalo all   OECD     development3    grass… humid   embedded    lw         
#>  9 buffalo all   OECD     development3    grass… temper… embedded    lw         
#> 10 buffalo all   OECD     development3    mixed… arid    embedded    lw         
#> # ℹ 351 more rows
#> # ℹ 1 more variable: emission_factor <dbl>
```
