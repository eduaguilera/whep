# Read a WHEP coefficient table.

Reads one of the coefficient tables shipped as CSV under
`inst/extdata/coefs`. These tables are small and versioned inside the
package (read at runtime, not stored remotely), so no download is
needed.

## Usage

``` r
whep_coef_table(name)
```

## Arguments

- name:

  Coefficient table name (the file stem), for example `"bio_coefs"` or
  `"ipcc_residue_coefs"`.

## Value

A tibble with the coefficient table.

## Examples

``` r
whep_coef_table("residue_feed_fraction")
#> # A tibble: 18 × 3
#>    region_hanpp                    feed_use_fraction source                   
#>    <chr>                                       <dbl> <chr>                    
#>  1 Sub-Saharan Africa                           0.4  Lal 2005; McIntire 1992  
#>  2 Northern Africa                              0.3  Smil 1999; Lal 2005      
#>  3 Western Asia                                 0.3  Smil 1999                
#>  4 Southern Asia                                0.45 Erenstein 2014; Smil 1999
#>  5 Eastern Asia                                 0.3  Smil 1999                
#>  6 South-Eastern Asia                           0.3  Smil 1999                
#>  7 Central Asia                                 0.25 Lal 2005                 
#>  8 Western Europe                               0.15 Krausmann 2008; Smil 1999
#>  9 Eastern Europe                               0.2  Krausmann 2008           
#> 10 Northern Europe                              0.15 Krausmann 2008           
#> 11 Southern Europe                              0.2  Krausmann 2008           
#> 12 Northern America                             0.05 Smil 1999                
#> 13 Latin America and the Caribbean              0.2  Smil 1999                
#> 14 Caribbean                                    0.2  Smil 1999                
#> 15 Oceania                                      0.05 Smil 1999                
#> 16 Australia and New Zealand                    0.05 Smil 1999                
#> 17 Melanesia                                    0.1  Smil 1999                
#> 18 Global                                       0.2  default fallback         
```
