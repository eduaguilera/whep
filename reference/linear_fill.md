# Fill gaps by linear interpolation, or carrying forward or backward.

Fills gaps (`NA` values) in a time-dependent variable by linear
interpolation between two points, or carrying forward or backwards the
last or initial values, respectively. It also creates a new variable
indicating the source of the filled values.

## Usage

``` r
linear_fill(
  df,
  var,
  time_index,
  interpolate = TRUE,
  fill_forward = TRUE,
  fill_backward = TRUE,
  .by = NULL
)
```

## Arguments

- df:

  A tibble data frame containing one observation per row.

- var:

  The variable of df containing gaps to be filled.

- time_index:

  The time index variable (usually year).

- interpolate:

  Logical. If `TRUE` (default), performs linear interpolation.

- fill_forward:

  Logical. If `TRUE` (default), carries last value forward.

- fill_backward:

  Logical. If `TRUE` (default), carries first value backward.

- .by:

  A character vector with the grouping variables (optional).

## Value

A tibble data frame (ungrouped) where gaps in var have been filled, and
a new "source" variable has been created indicating if the value is
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
)
linear_fill(sample_tibble, value, year, .by = c("category"))
#> # A tibble: 12 × 4
#>    category year  value source_value                 
#>    <chr>    <chr> <dbl> <chr>                        
#>  1 a        2015      3 First value carried backwards
#>  2 a        2016      3 Original                     
#>  3 a        2017      2 Linear interpolation         
#>  4 a        2018      1 Linear interpolation         
#>  5 a        2019      0 Original                     
#>  6 a        2020      0 Last value carried forward   
#>  7 b        2015      1 Original                     
#>  8 b        2016      2 Linear interpolation         
#>  9 b        2017      3 Linear interpolation         
#> 10 b        2018      4 Linear interpolation         
#> 11 b        2019      5 Original                     
#> 12 b        2020      5 Last value carried forward   
linear_fill(
  sample_tibble,
  value,
  year,
  interpolate = FALSE,
  .by = c("category"),
)
#> # A tibble: 12 × 4
#>    category year  value source_value                 
#>    <chr>    <chr> <dbl> <chr>                        
#>  1 a        2015      3 First value carried backwards
#>  2 a        2016      3 Original                     
#>  3 a        2017     NA Gap not filled               
#>  4 a        2018     NA Gap not filled               
#>  5 a        2019      0 Original                     
#>  6 a        2020      0 Last value carried forward   
#>  7 b        2015      1 Original                     
#>  8 b        2016     NA Gap not filled               
#>  9 b        2017     NA Gap not filled               
#> 10 b        2018     NA Gap not filled               
#> 11 b        2019      5 Original                     
#> 12 b        2020      5 Last value carried forward   
```
