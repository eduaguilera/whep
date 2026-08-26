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
