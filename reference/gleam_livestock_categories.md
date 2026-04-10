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

MacLeod et al. (2018) GLEAM 3.0 Model Description.

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
