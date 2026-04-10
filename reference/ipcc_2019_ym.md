# IPCC 2019 Ym values (Table 10.12).

Methane conversion rate (% GE) by species and feed situation. The 2019
Refinement differentiates:

- Cattle feedlot (\>90% concentrate): 3.0%.

- Sheep \>= 75 kg body weight: 6.7%.

- Sheep \< 75 kg body weight: 4.7%.

## Usage

``` r
ipcc_2019_ym
```

## Format

A tibble with `category`, `feed_situation`, `ym_percent`.

## Source

IPCC 2019 Refinement, Vol 4, Ch 10, Table 10.12.

## Examples

``` r
ipcc_2019_ym
#> # A tibble: 9 × 4
#>   category feed_situation       ym_percent ym_uncertainty
#>   <chr>    <chr>                     <dbl>          <dbl>
#> 1 Cattle   Pasture/Range               6.5              1
#> 2 Cattle   Mixed                       6.5              1
#> 3 Cattle   Feedlot (>90% conc.)        3                1
#> 4 Buffalo  Pasture/Range               6.5              1
#> 5 Buffalo  Mixed                       6.5              1
#> 6 Sheep    Large body (>=75kg)         6.7              1
#> 7 Sheep    Small body (<75kg)          4.7              1
#> 8 Goats    All                         5.5              1
#> 9 Camels   All                         5                1
```
