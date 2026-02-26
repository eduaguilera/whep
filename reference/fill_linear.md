# Fill gaps by linear interpolation, or carrying forward or backward.

Fills gaps (`NA` values) in a time-dependent variable by linear
interpolation between two points, or carrying forward or backwards the
last or initial values, respectively. It also creates a new variable
indicating the source of the filled values.

## Usage

``` r
fill_linear(
  data,
  value_col,
  time_col = year,
  interpolate = TRUE,
  fill_forward = TRUE,
  fill_backward = TRUE,
  value_smooth_window = NULL,
  .by = NULL
)
```

## Arguments

- data:

  A data frame containing one observation per row.

- value_col:

  The column containing gaps to be filled.

- time_col:

  The column containing time values. Default: `year`.

- interpolate:

  Logical. If `TRUE` (default), performs linear interpolation.

- fill_forward:

  Logical. If `TRUE` (default), carries last value forward.

- fill_backward:

  Logical. If `TRUE` (default), carries first value backward.

- value_smooth_window:

  An integer specifying the window size for a centered moving average
  applied to the variable before gap-filling. Useful for variables with
  high inter-annual variability. If `NULL` (default), no smoothing is
  applied.

- .by:

  A character vector with the grouping variables (optional).

## Value

A tibble data frame (ungrouped) where gaps in value_col have been
filled, and a new "source" variable has been created indicating if the
value is original or, in case it has been estimated, the gapfilling
method that has been used.

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
fill_linear(sample_tibble, value, .by = c("category"))
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
fill_linear(
  sample_tibble,
  value,
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
