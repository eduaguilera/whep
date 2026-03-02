# NUE for Livestock

Calculates Nitrogen Use Efficiency (NUE) for livestock categories
(excluding pets).

The livestock NUE is defined as the percentage of nitrogen in livestock
products relative to the nitrogen in feed intake: nue = prod_n / feed_n
\* 100

Additionally, a mass balance is calculated to check the recovery of N in
products and excretion relative to feed intake: mass_balance = (prod_n +
excretion_n) / feed_n

## Usage

``` r
calculate_nue_livestock(example = FALSE)
```

## Arguments

- example:

  If `TRUE`, return a small example output without downloading remote
  data. Default is `FALSE`.

## Value

A tibble containing:

- `year`: Year

- `province_name`: Spanish province

- `livestock_cat`: Livestock category

- `item`: Produced item

- `prod_n`: Nitrogen in livestock products (Mg)

- `feed_n`: Nitrogen in feed intake (Mg)

- `excretion_n`: Nitrogen excreted (Mg)

- `nue`: Nitrogen Use Efficiency (%)

- `mass_balance`: Mass balance ratio (%)

## Examples

``` r
calculate_nue_livestock(example = TRUE)
#> # A tibble: 10 × 9
#>     year province_name livestock_cat item       prod_n feed_n excretion_n    nue
#>    <dbl> <chr>         <chr>         <chr>       <dbl>  <dbl>       <dbl>  <dbl>
#>  1  1921 Lugo          Horses        Meat, Ot…   7.28    1078        1158  0.675
#>  2  1994 Huelva        Horses        Offals, …   0.921    397         423  0.232
#>  3  2001 Cuenca        Goats         Mutton &…   4.22     347         355  1.22 
#>  4  1876 Avila         Cattle_milk   Milk - E…  40.4      179         147 22.6  
#>  5  1918 Malaga        Horses        Meat, Ot…   4.3      635         684  0.678
#>  6  1902 Madrid        Cattle_meat   Fats, An…   0        654         599  0    
#>  7  1926 Zaragoza      Sheep         Hides an… 119       8965        8454  1.33 
#>  8  2017 Badajoz       Pigs          Offals, … 581      21434       15567  2.71 
#>  9  1928 Leon          Poultry       Poultry …  10.6      177         127  6    
#> 10  1861 Girona        Horses        Meat, Ot…   5.23     842         832  0.621
#> # ℹ 1 more variable: mass_balance <dbl>
```
