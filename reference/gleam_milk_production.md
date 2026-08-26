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

Not traced to a GLEAM document. These values are hardcoded in
`generate_gleam_pdf_tables()` in `data-raw/livestock_coefficients.R`,
not read from the GLEAM 3.0 Supplement S1 workbook, and no table of that
workbook contains them. The attribution to MacLeod et al. (2018) they
carried was wrong: that is the *Animal* position paper on GLEAM
([doi:10.1017/S1751731117001847](https://doi.org/10.1017/S1751731117001847)
), which publishes no such table. Treat the values as unverified
placeholders; tracked in whep#881.

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
