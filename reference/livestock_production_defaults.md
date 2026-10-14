# Default production parameters.

Default values for fat%, protein%, lactose%, weight gain, work hours,
and pregnancy fraction by species.

[`estimate_energy_demand()`](https://eduaguilera.github.io/whep/reference/estimate_energy_demand.md)
applies the weight gain to growing cohorts only and the pregnancy
fraction to mature-female cohorts only: following IPCC 2019 Vol 4 Ch 10
(pp. 10.18, 10.28), mature animals have no net weight gain over a year
and only mature females gestate. Beef breeding cows take the regional
IPCC 2019 Table 10A.2 rate instead of the `"Other Cattle"` value of 0.
The `cohort_life_stage` and `method_pregnancy` columns of its output
record which rule applied.

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

  Average daily weight gain of a growing animal (kg/day).

- work_hours_day:

  Hours of draft work per day.

- pregnant_fraction:

  Fraction of mature females giving birth in a year.

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
