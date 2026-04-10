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

MacLeod et al. (2018) GLEAM 3.0.

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
