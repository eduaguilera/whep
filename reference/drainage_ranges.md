# Drainage-class bins keyed on annual soil drainage.

Half-open bins that map an annual soil drainage flux (mm) to a drainage
class. A drainage value `s` is assigned to the class whose interval
satisfies `s_min < s <= s_max`. The classes key the Meisinger
denitrification matrix and the subsoil NO3 reduction table.

## Usage

``` r
drainage_ranges
```

## Format

A tibble with columns:

- drainage_rate:

  Drainage class: `"Very_high"`, `"High"`, `"Medium"`, `"Low"`,
  `"Very_low"` or `"None"`.

- s_min:

  Lower bound of the drainage flux interval (mm).

- s_max:

  Upper bound of the drainage flux interval (mm).

## Source

Spain historical nitrogen coefficient workbook (`N_coefficients.xlsx`,
sheet `Drainage_ranges`), companion to the Meisinger & Randall (1991)
denitrification matrix.
[doi:10.2136/1991.managingnitrogen.c5](https://doi.org/10.2136/1991.managingnitrogen.c5)
.

## Examples

``` r
drainage_ranges
#> # A tibble: 6 × 3
#>   drainage_rate  s_min  s_max
#>   <chr>          <dbl>  <dbl>
#> 1 Very_high     1000   3000  
#> 2 High           500   1000  
#> 3 Medium         200    500  
#> 4 Low             50    200  
#> 5 Very_low         0.1   50  
#> 6 None            -0.1    0.1
```
