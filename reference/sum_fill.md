# Fill gaps summing the previous value of a variable to the value of another variable.

Fills gaps in a variable with the sum of its previous value and the
value of another variable. When a gap has multiple observations, the
values are accumulated along the series. When there is a gap at the
start of the series, it can either remain unfilled or assume an
invisible 0 value before the first observation and start filling with
cumulative sum.

## Usage

``` r
sum_fill(df, var, change_var, start_with_zero = TRUE, .by = NULL)
```

## Arguments

- df:

  A tibble data frame containing one observation per row.

- var:

  The variable of df containing gaps to be filled.

- change_var:

  The variable whose values will be used to fill the gaps.

- start_with_zero:

  Logical. If TRUE, assumes an invisible 0 value before the first
  observation and fills with cumulative sum starting from the first
  change_var value. If FALSE (default), starting NA values remain
  unfilled.

- .by:

  A character vector with the grouping variables (optional).

## Value

A tibble dataframe (ungrouped) where gaps in var have been filled, and a
new "source" variable has been created indicating if the value is
original or, in case it has been estimated, the gapfilling method that
has been used.

## Examples

``` r
sample_tibble <- tibble::tibble(
  category = c("a", "a", "a", "a", "a", "a", "b", "b", "b", "b", "b", "b"),
  year = c(
    "2015", "2016", "2017", "2018", "2019", "2020",
    "2015", "2016", "2017", "2018", "2019", "2020"
  ),
  value = c(NA, 3, NA, NA, 0, NA, 1, NA, NA, NA, 5, NA),
  change_variable = c(1, 2, 3, 4, 1, 1, 0, 0, 0, 0, 0, 1)
)
sum_fill(
  sample_tibble,
  value,
  change_variable,
  start_with_zero = FALSE,
  .by = c("category")
)
#> # A tibble: 12 × 5
#>    category year  value change_variable source_value   
#>    <chr>    <chr> <dbl>           <dbl> <chr>          
#>  1 a        2015     NA               1 NA             
#>  2 a        2016      3               2 Original       
#>  3 a        2017      6               3 Filled with sum
#>  4 a        2018     10               4 Filled with sum
#>  5 a        2019      0               1 Original       
#>  6 a        2020      1               1 Filled with sum
#>  7 b        2015      1               0 Original       
#>  8 b        2016      1               0 Filled with sum
#>  9 b        2017      1               0 Filled with sum
#> 10 b        2018      1               0 Filled with sum
#> 11 b        2019      5               0 Original       
#> 12 b        2020      6               1 Filled with sum
sum_fill(
  sample_tibble,
  value,
  change_variable,
  start_with_zero = TRUE,
  .by = c("category")
)
#> # A tibble: 12 × 5
#>    category year  value change_variable source_value   
#>    <chr>    <chr> <dbl>           <dbl> <chr>          
#>  1 a        2015      1               1 Filled with sum
#>  2 a        2016      3               2 Original       
#>  3 a        2017      6               3 Filled with sum
#>  4 a        2018     10               4 Filled with sum
#>  5 a        2019      0               1 Original       
#>  6 a        2020      1               1 Filled with sum
#>  7 b        2015      1               0 Original       
#>  8 b        2016      1               0 Filled with sum
#>  9 b        2017      1               0 Filled with sum
#> 10 b        2018      1               0 Filled with sum
#> 11 b        2019      5               0 Original       
#> 12 b        2020      6               1 Filled with sum
```
