# GLEAM milk production.

Average annual milk yields and lactation lengths by region.

## Usage

``` r
gleam_milk_production
```

## Format

A tibble with `region`, `species`, `system`, `milk_kg_head_yr`,
`lactation_days`.

## Source

Not traced to any GLEAM document, and GLEAM appears to publish no such
table (whep#881). Searched and ruled out: the GLEAM 3.0 Supplement S1
workbook; the herd-parameter tables of Supplement S1 of the Version 2.0
Revision 5 description (Tables 2.4-2.21 give live weights and
replacement, fertility, mortality and age-at-first-calving rates, but no
milk yield and no lactation length); and both model descriptions, in
which `MILKyield` (Equation 9.1) and the lactation period are
country-level inputs taken from FAOSTAT and national data rather than
defaults – Table 1.2 of the Version 2.0 description lists them under
national/sub-national resolution. GLEAM-i likewise asks the user for
"annual average milk yield per milking cow". Treat the values as
unverified placeholders. No function in `R/` reads this object: the Tier
2 lactation energy term takes `milk_yield_kg_day` from the caller, and
`.build_demand_energy()` derives it from the FAOSTAT `t_head` rows.

## Examples

``` r
gleam_milk_production
#> # A tibble: 9 × 5
#>   region             species system milk_kg_head_yr lactation_days
#>   <chr>              <chr>   <chr>            <dbl>          <dbl>
#> 1 Western Europe     Cattle  Dairy             7500            305
#> 2 North America      Cattle  Dairy             9500            305
#> 3 Oceania            Cattle  Dairy             5500            270
#> 4 Latin America      Cattle  Dairy             2500            240
#> 5 Sub-Saharan Africa Cattle  Dairy              800            180
#> 6 South Asia         Cattle  Dairy             1500            240
#> 7 South Asia         Buffalo Dairy             1800            270
#> 8 Western Europe     Sheep   Dairy              200            180
#> 9 Western Europe     Goats   Dairy              450            240
```
