# Build commodity balance sheets

Construct commodity balance sheets (CBS) from raw FAOSTAT data. This is
a convenience wrapper that chains the three pipeline steps:

1.  `.read_cbs()` — read & reformat FAOSTAT CBS data.

2.  `.fix_cbs()` — processing calibration, trade imputation, destiny
    filling, and final balancing.

3.  `.qc_cbs()` — flag data-quality anomalies.

## Usage

``` r
build_commodity_balances(
  primary_all,
  start_year = 1850,
  end_year = 2023,
  smooth_carry_forward = FALSE,
  example = FALSE
)
```

## Arguments

- primary_all:

  A tibble of primary production, as returned by
  [`build_primary_production()`](https://eduaguilera.github.io/whep/reference/build_primary_production.md).

- start_year:

  Integer. First year to include. Default `1850`.

- end_year:

  Integer. Last year to include. Default `2023`.

- smooth_carry_forward:

  Logical. If `TRUE`, carry-forward tails are replaced with a linear
  trend. Default `FALSE`.

- example:

  Logical. If `TRUE`, return a small hardcoded example tibble instead of
  reading remote data. Default `FALSE`.

## Value

A tibble in long format with columns: `year`, `area_code`,
`item_cbs_code`, `element` (e.g. `"production"`, `"import"`, `"food"`),
`value`, `source`, `fao_flag`.

## Examples

``` r
build_commodity_balances(example = TRUE)
#> # A tibble: 10 × 7
#>     year area_code item_cbs_code element             value source       fao_flag
#>    <dbl>     <dbl>         <dbl> <chr>               <dbl> <chr>        <chr>   
#>  1  2010       120          2731 import           1.76e+ 3 FAOSTAT_FBS… NA      
#>  2  1981       222          2734 domestic_supply  4.1 e+ 4 FAOSTAT_FBS… NA      
#>  3  1906       203          2655 processing       6.35e+ 4 historical_… NA      
#>  4  1899       175          2744 food             7.26e+ 1 historical_… NA      
#>  5  2018        48          2562 domestic_supply  1.20e+ 5 FAOSTAT_FBS… NA      
#>  6  1871        10          2746 stock_variation -7.28e-12 NA           NA      
#>  7  1938       226          2848 production       1.51e+ 5 historical_… NA      
#>  8  1924        11          2557 production       1.61e+ 2 historical_… NA      
#>  9  1928        96          2625 domestic_supply  1.85e+ 4 NA           NA      
#> 10  1879       236          2547 seed             3.83e- 8 historical_… NA      
```
