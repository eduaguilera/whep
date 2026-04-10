# GLEAM animal weights.

Typical live weights by region, species, system, and cohort.

## Usage

``` r
gleam_animal_weights
```

## Format

A tibble with `region`, `species`, `system`, `cohort`, `weight_kg`.

## Source

MacLeod et al. (2018) GLEAM 3.0.

## Examples

``` r
gleam_animal_weights
#> # A tibble: 21 × 5
#>    region             species system cohort       weight_kg
#>    <chr>              <chr>   <chr>  <chr>            <dbl>
#>  1 Western Europe     Cattle  Dairy  Adult Female       650
#>  2 Western Europe     Cattle  Dairy  Adult Male        1000
#>  3 Western Europe     Cattle  Beef   Adult Female       600
#>  4 Western Europe     Cattle  Beef   Fattening          400
#>  5 North America      Cattle  Dairy  Adult Female       680
#>  6 North America      Cattle  Dairy  Adult Male        1000
#>  7 North America      Cattle  Beef   Adult Female       550
#>  8 North America      Cattle  Beef   Fattening          450
#>  9 Sub-Saharan Africa Cattle  All    Adult Female       250
#> 10 Sub-Saharan Africa Cattle  All    Adult Male         350
#> # ℹ 11 more rows
```
