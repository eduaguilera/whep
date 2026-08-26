# IPCC 2019 direct N2O emission factors.

Table 10.21: EF3 values (kg N2O-N per kg N excreted) by manure
management system.

## Usage

``` r
ipcc_2019_n2o_ef_direct
```

## Format

A tibble with `system`, `ef_kg_n2o_n_per_kg_n`.

## Source

Mixed, and not consistently the 2019 Refinement's Table 10.21. Verified
against both editions of Vol 4, Ch 10, Table 10.21:

- Matching both editions: liquid/slurry with crust 0.005, in-vessel
  composting 0.006, poultry with and without litter 0.001.

- Matching the 2006 Guidelines but not the 2019 Refinement: solid
  storage 0.005 (2019: 0.010), static-pile composting 0.006 (2019:
  0.010), passive-windrow composting 0.01 (2019: 0.005), anaerobic
  digester 0 (2019: 0.0006).

- Matching neither edition: daily spread 0.01 and liquid/slurry without
  crust 0.002 and uncovered anaerobic lagoon 0.001 (all three are 0 in
  both editions), dry lot 0.005 (0.02 in both), intensive-windrow
  composting 0.006 (2019: 0.005; 2006: 0.1). Pasture/range/paddock is
  not in Table 10.21 in either edition, which defers it to Ch 11. Its
  stored 0.01 is the 2006 Ch 11 Table 11.1 EF3PRP,SO for sheep and other
  animals; the 2019 Refinement's Table 11.1 (Updated) gives 0.004 for
  cattle, poultry and pigs and 0.003 for sheep and other animals.
  Tracked in whep#601.

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
