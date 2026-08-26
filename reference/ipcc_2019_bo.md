# IPCC 2019 Bo values (Table 10.16A).

Maximum CH4 producing capacity of manure (m3 CH4/kg VS). Dairy cattle
(0.24) differs from other cattle (0.18).

## Usage

``` r
ipcc_2019_bo
```

## Format

A tibble with `category`, `bo_m3_kg_vs`.

## Source

IPCC 2019 Refinement, Vol 4, Ch 10, Table 10.16A (Updated) – Table 10.16
in that edition is the manure CH4 factors for deer and similar species.
Every row matches its high-productivity column except
`"Swine - Breeding"` 0.27: Table 10.16A publishes one swine Bo (0.48
North America, 0.45 other high-productivity regions, 0.29 low
productivity) and the 2006 Annex 10A.2 derivation tables give breeding
swine the same Bo as market swine, so 0.27 appears in neither edition;
it coincides with the North American *market swine volatile-solids rate*
of 0.27 kg VS head-1 day-1 in 2006 Annex 10A.2. `"Other Cattle"` 0.18 is
the Western European non-dairy column (North America is 0.19, Eastern
Europe and Oceania 0.17). Tracked in whep#601.

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
#>  8 Horses                    0.3 
#>  9 Mules and Asses           0.33
#> 10 Camels                    0.26
#> 11 Poultry - Layers          0.39
#> 12 Poultry - Broilers        0.36
```
