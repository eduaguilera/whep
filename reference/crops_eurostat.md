# Eurostat crop classification codes

Maps Eurostat crop codes to their full crop category names, used when
integrating Eurostat agricultural statistics.

## Usage

``` r
crops_eurostat
```

## Format

A tibble where each row corresponds to one Eurostat crop category. It
contains the following columns:

- `Crop`: Eurostat crop code (e.g., `"G0000"`, `"G1000"`).

- `Name_Eurostat`: Full name of the crop category as used in Eurostat
  (e.g., `"Plants harvested green from arable land"`,
  `"Temporary grasses and grazings"`).

## Source

[Eurostat Agricultural
Statistics](https://ec.europa.eu/eurostat/statistics-explained/index.php/Agricultural_statistics).

## Examples

``` r
head(crops_eurostat)
#> # A tibble: 6 × 2
#>   Crop  Name_Eurostat                                 
#>   <chr> <chr>                                         
#> 1 G0000 Plants harvested green from arable land       
#> 2 G1000 Temporary grasses and grazings                
#> 3 G2000 Leguminous plants harvested green             
#> 4 G2100 Alfalfa                                       
#> 5 G2900 Other leguminous plants harvested green n.e.c.
#> 6 G3000 Green maize                                   
```
