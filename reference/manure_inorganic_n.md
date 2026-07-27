# Inorganic (mineral) nitrogen fraction of excreted manure by species.

Fraction of total excreted nitrogen that is in inorganic (ammoniacal)
form and therefore available for ammonia volatilisation, by livestock
species and manure stream (whole excreta, or after separation into a
liquid and a solid fraction). Used by
[`calculate_manner_nh3()`](https://eduaguilera.github.io/whep/reference/calculate_manner_nh3.md)'s
organic-manure path to scale the realised emission factor down to the
ammoniacal nitrogen actually applied. The mapping from
[`calculate_manner_nh3()`](https://eduaguilera.github.io/whep/reference/calculate_manner_nh3.md)'s
`manure_type` argument to this table's `species`/`manure_stream` keys
(`cattle_slurry`/`pig_slurry` to the `"Liquid"` stream, `FYM` to Cattle
`"Solid"`, `poultry_manure` to Poultry `"Solid"`) is a documented
modelling choice made when porting this table, not a literal Spain_Hist
crosswalk; see
[`calculate_manner_nh3()`](https://eduaguilera.github.io/whep/reference/calculate_manner_nh3.md)'s
Details.

## Usage

``` r
manure_inorganic_n
```

## Format

A tibble with columns:

- species:

  Livestock species: `"Sheep"`, `"Goats"`, `"Poultry"`, `"Cattle"`,
  `"Pigs"`, `"Horses"`, `"Donkeys_mules"` or `"Rabbits"`.

- manure_stream:

  Manure stream: `"Excreta"` (whole, unseparated excreta), `"Liquid"` or
  `"Solid"` (after mechanical separation); not every species has a
  `"Liquid"` row.

- inorganic_n_fraction:

  Fraction of the stream's total nitrogen that is inorganic
  (ammoniacal).

- source:

  Short author-year provenance string as cited in the Spain_Hist
  `Livestock.xlsx` `Manure_inorganic_N` sheet for that coefficient.
  These are secondary citations transcribed from that workbook, not
  independently DOI-verified full bibliographic entries.

## Source

WHEP project-internal coefficient workbook (not a public DOI): Spain
historical livestock coefficient workbook, `Livestock.xlsx`, sheet
`Manure_inorganic_N`. That sheet in turn cites: Van Soest, P. J. (1994);
Nahm, K. H. (2003); Nahm, K. H. (2005); Smith, K. A. & Frost, J. P.
(2000); Chambers, B. J. et al. (1999); Chambers, B. J. et al. (2000);
Nicholson, F. A. et al. (1996); Canh, T. T. et al. (1997); Sommer, S. G.
et al. (2004); Burton, C. H. & Turner, C. (2003); Martinez, J. & Burton,
C. H. (2003); Rotz, C. A. (2004); Wheeler, E. F. et al. (2011);
Gungor-Demirci, G. & Demirer, G. N. (2004); Lebas, F. (1975); Lebas, F.
(2004). These secondary citations are transcribed as recorded in the
workbook and have not been independently verified against the primary
sources.

## Examples

``` r
manure_inorganic_n
#> # A tibble: 19 × 4
#>    species       manure_stream inorganic_n_fraction source                      
#>    <chr>         <chr>                        <dbl> <chr>                       
#>  1 Sheep         Excreta                      0.55  Van Soest (1994)            
#>  2 Sheep         Solid                        0.2   Rotz (2004)                 
#>  3 Goats         Excreta                      0.55  Van Soest (1994)            
#>  4 Goats         Solid                        0.2   Rotz (2004)                 
#>  5 Poultry       Excreta                      0.7   Nahm (2003)                 
#>  6 Poultry       Liquid                       0.6   Smith and Frost (2000); Cha…
#>  7 Poultry       Solid                        0.325 Nicholson et al. (1996); Na…
#>  8 Cattle        Excreta                      0.65  Van Soest (1994)            
#>  9 Cattle        Liquid                       0.6   Nahm (2005); Martinez and B…
#> 10 Cattle        Solid                        0.225 Chambers et al. (2000); Nic…
#> 11 Pigs          Excreta                      0.6   Canh et al. (1997)          
#> 12 Pigs          Liquid                       0.7   Sommer et al. (2004); Burto…
#> 13 Pigs          Solid                        0.275 Burton and Turner (2003); S…
#> 14 Horses        Excreta                      0.4   Wheeler et al. (2011)       
#> 15 Horses        Solid                        0.15  Wheeler et al. (2011); Gung…
#> 16 Donkeys_mules Excreta                      0.4   Wheeler et al. (2011)       
#> 17 Donkeys_mules Solid                        0.15  Wheeler et al. (2011); Gung…
#> 18 Rabbits       Excreta                      0.35  Lebas (1975)                
#> 19 Rabbits       Solid                        0.15  Lebas (2004)                
```
