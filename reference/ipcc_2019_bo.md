# IPCC 2019 Bo values (Table 10.16).

Maximum CH4 producing capacity of manure (m3 CH4/kg VS). Dairy cattle
(0.24) differs from other cattle (0.18).

## Usage

``` r
ipcc_2019_bo
```

## Format

A tibble with `category`, `bo_m3_kg_vs`.

## Source

IPCC 2019 Refinement, Vol 4, Ch 10, Table 10.16.

## Examples

``` r
ipcc_2019_bo
#> # A tibble: 12 × 2
#>    category           bo_m3_kg_vs
#>    <chr>                    <dbl>
#>  1 Dairy Cattle              0.24
#>  2 Other Cattle              0.18
#>  3 Buffalo                   0.1 
#>  4 Swine - Market            0.45
#>  5 Swine - Breeding          0.27
#>  6 Sheep                     0.19
#>  7 Goats                     0.18
#>  8 Horses                    0.33
#>  9 Mules and Asses           0.33
#> 10 Camels                    0.1 
#> 11 Poultry - Layers          0.39
#> 12 Poultry - Broilers        0.24
```
