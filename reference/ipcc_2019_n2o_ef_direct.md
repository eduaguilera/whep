# IPCC 2019 direct N2O emission factors.

Table 10.21: EF3 values (kg N2O-N/kg N) by manure management system.

## Usage

``` r
ipcc_2019_n2o_ef_direct
```

## Format

A tibble with `mms_type`, `ef3_kg_n2on_kg_n`, `source`.

## Source

IPCC 2019 Refinement, Vol 4, Ch 10, Table 10.21.

## Examples

``` r
ipcc_2019_n2o_ef_direct
#> # A tibble: 18 × 2
#>    system                       ef_kg_n2o_n_per_kg_n
#>    <chr>                                       <dbl>
#>  1 Uncovered Anaerobic Lagoon                  0.001
#>  2 Liquid/Slurry - No Crust                    0.002
#>  3 Liquid/Slurry - With Crust                  0.005
#>  4 Liquid/Slurry                               0.002
#>  5 Solid Storage and Dry Lot                   0.005
#>  6 Solid Storage                               0.005
#>  7 Dry Lot                                     0.005
#>  8 Pasture/Range/Paddock                       0.01 
#>  9 Daily Spread                                0.01 
#> 10 Anaerobic Digester                          0    
#> 11 Burned for Fuel                             0    
#> 12 Composting - In-vessel                      0.006
#> 13 Composting - Static Pile                    0.006
#> 14 Composting - Intensive                      0.006
#> 15 Composting - Passive                        0.01 
#> 16 Poultry Manure - High Rise                  0.001
#> 17 Poultry Manure - Deep Litter                0.001
#> 18 Other                                       0.005
```
