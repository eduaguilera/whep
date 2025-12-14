# Fill gaps using a proxy variable

Fills gaps in a variable based on changes in a proxy variable, using
ratios between the filled variable and the proxy variable, and labels
output accordingly.

## Usage

``` r
proxy_fill(df, var, proxy_var, time_index, ...)
```

## Arguments

- df:

  A tibble data frame containing one observation per row.

- var:

  The variable of df containing gaps to be filled.

- proxy_var:

  The variable to be used as proxy.

- time_index:

  The time index variable (usually year).

- ...:

  Optionally, additional arguments that will be passed to
  [`linear_fill()`](https://eduaguilera.github.io/whep/reference/linear_fill.md)
  with the ratios. See that function to know the accepted arguments.

## Value

A tibble dataframe (ungrouped) where gaps in var have been filled, a new
proxy_ratio variable has been created, and a new "source" variable has
been created indicating if the value is original or, in case it has been
estimated, the gapfilling method that has been used.

## Examples

``` r
sample_tibble <- tibble::tibble(
  category = c("a", "a", "a", "a", "a", "a", "b", "b", "b", "b", "b", "b"),
  year = c(
    "2015", "2016", "2017", "2018", "2019", "2020",
    "2015", "2016", "2017", "2018", "2019", "2020"
  ),
  value = c(NA, 3, NA, NA, 0, NA, 1, NA, NA, NA, 5, NA),
  proxy_variable = c(1, 2, 2, 2, 2, 2, 1, 2, 3, 4, 5, 6)
)
proxy_fill(sample_tibble, value, proxy_variable, year, .by = c("category"))
#> # A tibble: 12 × 7
#>    category year  value proxy_variable proxy_ratio source_proxy_ratio           
#>    <chr>    <chr> <dbl>          <dbl>       <dbl> <chr>                        
#>  1 a        2015    1.5              1         1.5 First value carried backwards
#>  2 a        2016    3                2         1.5 Original                     
#>  3 a        2017    2                2         1   Linear interpolation         
#>  4 a        2018    1                2         0.5 Linear interpolation         
#>  5 a        2019    0                2         0   Original                     
#>  6 a        2020    0                2         0   Last value carried forward   
#>  7 b        2015    1                1         1   Original                     
#>  8 b        2016    2                2         1   Linear interpolation         
#>  9 b        2017    3                3         1   Linear interpolation         
#> 10 b        2018    4                4         1   Linear interpolation         
#> 11 b        2019    5                5         1   Original                     
#> 12 b        2020    6                6         1   Last value carried forward   
#> # ℹ 1 more variable: source_value <chr>
```
