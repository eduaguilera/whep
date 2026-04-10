# Default production parameters.

Default values for fat%, protein%, lactose%, weight gain, work hours,
and pregnancy fraction by species.

## Usage

``` r
livestock_production_defaults
```

## Format

A tibble with columns:

- category:

  Species or animal class.

- fat_percent:

  Milk fat content (percent).

- protein_percent:

  Milk protein content (percent).

- lactose_percent:

  Milk lactose content (percent).

- weight_gain_kg_day:

  Average daily weight gain (kg/day).

- work_hours_day:

  Hours of draft work per day.

- pregnant_fraction:

  Fraction of females pregnant.

## Source

NRC 2001; IPCC 2019, Vol 4, Ch 10.

## Examples

``` r
livestock_production_defaults
#> # A tibble: 10 × 7
#>    category       fat_percent protein_percent lactose_percent weight_gain_kg_day
#>    <chr>                <dbl>           <dbl>           <dbl>              <dbl>
#>  1 Dairy Cattle             4             3.2            4.85               0   
#>  2 Other Cattle             0             0              0                  0.5 
#>  3 Buffalo                  7             4.5            4.9                0.2 
#>  4 Sheep                    7             5.5            4.8                0.1 
#>  5 Goats                    4             3.5            4.5                0.05
#>  6 Swine                    0             0              0                  0.6 
#>  7 Poultry                  0             0              0                  0.05
#>  8 Horses                   0             0              0                  0   
#>  9 Camels                   4             3.7            5                  0   
#> 10 Mules and Ass…           0             0              0                  0   
#> # ℹ 2 more variables: work_hours_day <dbl>, pregnant_fraction <dbl>
```
