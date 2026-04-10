# Country-level fraction of crop residues removed.

Countries whose FracReMove value differs from the GLEAM default.

## Usage

``` r
gleam_fracremove
```

## Format

A tibble with columns:

- country:

  Country name.

- continent:

  Continent.

- region:

  GLEAM region.

- fracremove:

  Fraction of crop residues removed (0 to 1).

## Source

MacLeod et al. (2018) GLEAM 3.0 Supplement S1, Table S.6.9.

## Examples

``` r
gleam_fracremove
#> # A tibble: 13 × 4
#>    country                                    continent region        fracremove
#>    <chr>                                      <chr>     <chr>              <dbl>
#>  1 Austria                                    Europe    Western Euro…      0.341
#>  2 Belgium                                    Europe    Western Euro…      0.500
#>  3 Canada                                     Americas  Northern Ame…      0.471
#>  4 Germany                                    Europe    Western Euro…      0.63 
#>  5 Greece                                     Europe    Southern Eur…      0.521
#>  6 Liechtenstein                              Europe    Western Euro…      0.727
#>  7 Norway                                     Europe    Northern Eur…      0.653
#>  8 Poland                                     Europe    Eastern Euro…      0.53 
#>  9 Portugal                                   Europe    Southern Eur…      0.709
#> 10 Slovenia                                   Europe    Southern Eur…      0.467
#> 11 Sweden                                     Europe    Northern Eur…      0.636
#> 12 Switzerland                                Europe    Western Euro…      0.717
#> 13 U.K. of Great Britain and Northern Ireland Europe    Northern Eur…      0.524
```
