# Get production item names from item codes

Add a new column to an existing tibble with the corresponding name for
each production item code. The codes are assumed to be from those
defined by FAOSTAT.

## Usage

``` r
add_item_prod_name(
  table,
  code_column = "item_prod_code",
  name_column = "item_prod_name"
)
```

## Arguments

- table:

  The table that will be modified with a new column.

- code_column:

  The name of the column in `table` containing the codes.

- name_column:

  The name of the output column containing the names.

## Value

A tibble with all the contents of `table` and an extra column named
`name_column`, which contains the names. If there is no name match, an
`NA` is included.

## Examples

``` r
table <- tibble::tibble(item_prod_code = c(27, 358, 12345))
add_item_prod_name(table)
#> # A tibble: 3 × 2
#>   item_prod_code item_prod_name
#>            <dbl> <chr>         
#> 1             27 Rice          
#> 2            358 Cabbages      
#> 3          12345 NA            

table |>
  dplyr::rename(my_item_prod_code = item_prod_code) |>
  add_item_prod_name(code_column = "my_item_prod_code")
#> # A tibble: 3 × 2
#>   my_item_prod_code item_prod_name
#>               <dbl> <chr>         
#> 1                27 Rice          
#> 2               358 Cabbages      
#> 3             12345 NA            

add_item_prod_name(table, name_column = "my_custom_name")
#> # A tibble: 3 × 2
#>   item_prod_code my_custom_name
#>            <dbl> <chr>         
#> 1             27 Rice          
#> 2            358 Cabbages      
#> 3          12345 NA            
```
