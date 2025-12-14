# Get area codes from area names

Add a new column to an existing tibble with the corresponding code for
each name. The codes are assumed to be from those defined by the `FABIO`
model.

## Usage

``` r
add_area_code(table, name_column = "area_name", code_column = "area_code")
```

## Arguments

- table:

  The table that will be modified with a new column.

- name_column:

  The name of the column in `table` containing the names.

- code_column:

  The name of the output column containing the codes.

## Value

A tibble with all the contents of `table` and an extra column named
`code_column`, which contains the codes. If there is no code match, an
`NA` is included.

## Examples

``` r
table <- tibble::tibble(
  area_name = c("Armenia", "Afghanistan", "Dummy Country", "Albania")
)

add_area_code(table)
#> # A tibble: 4 × 2
#>   area_name     area_code
#>   <chr>             <dbl>
#> 1 Armenia               1
#> 2 Afghanistan           2
#> 3 Dummy Country        NA
#> 4 Albania               3

table |>
  dplyr::rename(my_area_name = area_name) |>
  add_area_code(name_column = "my_area_name")
#> # A tibble: 4 × 2
#>   my_area_name  area_code
#>   <chr>             <dbl>
#> 1 Armenia               1
#> 2 Afghanistan           2
#> 3 Dummy Country        NA
#> 4 Albania               3

add_area_code(table, code_column = "my_custom_code")
#> # A tibble: 4 × 2
#>   area_name     my_custom_code
#>   <chr>                  <dbl>
#> 1 Armenia                    1
#> 2 Afghanistan                2
#> 3 Dummy Country             NA
#> 4 Albania                    3
```
