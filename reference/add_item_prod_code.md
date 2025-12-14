# Get production item codes from item names

Add a new column to an existing tibble with the corresponding code for
each production item name. The codes are assumed to be from those
defined by FAOSTAT.

## Usage

``` r
add_item_prod_code(
  table,
  name_column = "item_prod_name",
  code_column = "item_prod_code"
)
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
  item_prod_name = c("Rice", "Cabbages", "Dummy Item")
)
add_item_prod_code(table)
#> # A tibble: 3 × 2
#>   item_prod_name item_prod_code
#>   <chr>                   <dbl>
#> 1 Rice                       27
#> 2 Cabbages                  358
#> 3 Dummy Item                 NA

table |>
  dplyr::rename(my_item_prod_name = item_prod_name) |>
  add_item_prod_code(name_column = "my_item_prod_name")
#> # A tibble: 3 × 2
#>   my_item_prod_name item_prod_code
#>   <chr>                      <dbl>
#> 1 Rice                          27
#> 2 Cabbages                     358
#> 3 Dummy Item                    NA

add_item_prod_code(table, code_column = "my_custom_code")
#> # A tibble: 3 × 2
#>   item_prod_name my_custom_code
#>   <chr>                   <dbl>
#> 1 Rice                       27
#> 2 Cabbages                  358
#> 3 Dummy Item                 NA
```
