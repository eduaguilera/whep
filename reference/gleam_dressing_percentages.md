# GLEAM dressing percentages.

Carcass weight as percentage of live weight by species, production
system, cohort, and GLEAM region. Includes country-specific overrides
for industrial pig systems in Western Europe.

## Usage

``` r
gleam_dressing_percentages
```

## Format

A tibble with columns:

- species:

  Animal species (Cattle, Buffaloes, Sheep, Goats, Pigs, Chicken).

- production_system:

  Production system (Dairy, Beef, Backyard, Intermediate, Industrial,
  Layers, Broilers). NA for species without system breakdown.

- cohort:

  Cohort (e.g. Adult and replacement female). NA for species without
  cohort breakdown.

- country:

  Country name for country-specific values. NA for regional values.

- gleam_region:

  GLEAM region abbreviation (NA, RUS, WE, EE, NENA, ESEA, OCE, SA, LAC,
  SSA).

- dressing_percent:

  Dressing percentage.

## Source

FAO (2022) GLEAM version 3.0, Supplement S1 (an FAO workbook; no DOI is
issued for it), Table S.9.1:
<https://www.fao.org/fileadmin/user_upload/gleam/docs/GLEAM_3.0_Supplement_S1.xlsx>

## Examples

``` r
gleam_dressing_percentages
#> # A tibble: 193 × 6
#>    species production_system cohort        country gleam_region dressing_percent
#>    <chr>   <chr>             <chr>         <chr>   <chr>                   <dbl>
#>  1 Cattle  Dairy             Adult and re… NA      NA                         50
#>  2 Cattle  Dairy             Adult and re… NA      RUS                        50
#>  3 Cattle  Dairy             Adult and re… NA      WE                         50
#>  4 Cattle  Dairy             Adult and re… NA      EE                         50
#>  5 Cattle  Dairy             Adult and re… NA      NENA                       48
#>  6 Cattle  Dairy             Adult and re… NA      ESEA                       50
#>  7 Cattle  Dairy             Adult and re… NA      OCE                        50
#>  8 Cattle  Dairy             Adult and re… NA      SA                         50
#>  9 Cattle  Dairy             Adult and re… NA      LAC                        50
#> 10 Cattle  Dairy             Adult and re… NA      SSA                        47
#> # ℹ 183 more rows
```
