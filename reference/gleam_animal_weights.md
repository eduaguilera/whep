# GLEAM animal weights.

Typical live weights by region, species, system, and cohort.

## Usage

``` r
gleam_animal_weights
```

## Format

A tibble with `region`, `species`, `system`, `cohort`, `weight_kg`.

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
gleam_animal_weights
#> # A tibble: 32 × 5
#>    region             species system cohort       weight_kg
#>    <chr>              <chr>   <chr>  <chr>            <dbl>
#>  1 Western Europe     Cattle  Dairy  Adult Female       650
#>  2 Western Europe     Cattle  Dairy  Adult Male        1000
#>  3 Western Europe     Cattle  Beef   Adult Female       600
#>  4 Western Europe     Cattle  Beef   Fattening          400
#>  5 North America      Cattle  Dairy  Adult Female       680
#>  6 North America      Cattle  Dairy  Adult Male        1000
#>  7 North America      Cattle  Beef   Adult Female       550
#>  8 North America      Cattle  Beef   Fattening          450
#>  9 Sub-Saharan Africa Cattle  All    Adult Female       250
#> 10 Sub-Saharan Africa Cattle  All    Adult Male         350
#> # ℹ 22 more rows
```
