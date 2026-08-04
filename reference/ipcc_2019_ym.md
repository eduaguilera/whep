# IPCC 2019 Ym values (Table 10.13).

Methane conversion rate (% GE) by species and feed situation. The 2019
Refinement differentiates:

- Cattle feedlot (\>90% concentrate): 3.0%.

- Sheep: a single 6.7%, irrespective of feed quality (no body-weight
  split).

## Usage

``` r
ipcc_2019_ym
```

## Format

A tibble with `category`, `feed_situation`, `ym_percent`.

## Source

IPCC 2019 Refinement, Vol 4, Ch 10, Table 10.13.

## Examples

``` r
ipcc_2019_ym
#> # A tibble: 8 × 4
#>   category feed_situation       ym_percent ym_uncertainty
#>   <chr>    <chr>                     <dbl>          <dbl>
#> 1 Cattle   Pasture/Range               6.5              1
#> 2 Cattle   Mixed                       6.5              1
#> 3 Cattle   Feedlot (>90% conc.)        3                1
#> 4 Buffalo  Pasture/Range               6.5              1
#> 5 Buffalo  Mixed                       6.5              1
#> 6 Sheep    All                         6.7              1
#> 7 Goats    All                         5.5              1
#> 8 Camels   All                         5                1
```
