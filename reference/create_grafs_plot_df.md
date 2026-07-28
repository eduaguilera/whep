# Create GRAFS plot dataset.

Combines land input data and N flows from crops, livestock, imports, and
exports to generate a dataset of nitrogen (MgN) by province and year,
ready to feed the GRAFS plot package offered by Alfredo Rodriguez.

## Usage

``` r
create_grafs_plot_df(example = FALSE)
```

## Arguments

- example:

  If `TRUE`, return a small example output without downloading remote
  data. Default is `FALSE`.

## Value

A tibble with columns `province`, `year`, `label`, `data`, `align`, and
`arrowColor`. Each `label` is a GRAFS template token (e.g. `{ARAiN}`)
and `data` holds its value as a character string.

## Examples

``` r
create_grafs_plot_df(example = TRUE)
#> # A tibble: 10 × 6
#>    province  year label                data   align arrowColor
#>    <chr>    <dbl> <chr>                <chr>  <chr> <chr>     
#>  1 Huesca    2000 {ARAiN}              12.34  R     ""        
#>  2 Huesca    2000 {CROPS_TO_LIVESTOCK} 45.6   L     ""        
#>  3 Huesca    2000 {POPULATIONM}        0.22   L     ""        
#>  4 Huesca    2000 {PROVINCE_NAME}      Huesca L     ""        
#>  5 Huesca    2000 {WIDTH_MAX}          1500   L     ""        
#>  6 Huesca    2000 {YEAR}               2000   L     ""        
#>  7 Lleida    2000 {ARArN}              8.9    R     ""        
#>  8 Lleida    2000 {LVSTCKTOTN}         3.21   L     ""        
#>  9 Spain     2000 {CRPLNDTOTN}         120.5  R     ""        
#> 10 Spain     2000 {POPULATIONM}        40.1   L     ""        
```
