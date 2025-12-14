# Get commodity balance sheet item names from item codes

Add a new column to an existing tibble with the corresponding name for
each commodity balance sheet item code. The codes are assumed to be from
those defined by FAOSTAT.

## Usage

``` r
add_item_cbs_name(
  table,
  code_column = "item_cbs_code",
  name_column = "item_cbs_name"
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
table <- tibble::tibble(item_cbs_code = c(2559, 2744, 9876))
add_item_cbs_name(table)
#> # A tibble: 3 × 2
#>   item_cbs_code item_cbs_name
#>           <dbl> <chr>        
#> 1          2559 Cottonseed   
#> 2          2744 Eggs         
#> 3          9876 NA           

table |>
  dplyr::rename(my_item_cbs_code = item_cbs_code) |>
  add_item_cbs_name(code_column = "my_item_cbs_code")
#> # A tibble: 3 × 2
#>   my_item_cbs_code item_cbs_name
#>              <dbl> <chr>        
#> 1             2559 Cottonseed   
#> 2             2744 Eggs         
#> 3             9876 NA           

add_item_cbs_name(table, name_column = "my_custom_name")
#> # A tibble: 3 × 2
#>   item_cbs_code my_custom_name
#>           <dbl> <chr>         
#> 1          2559 Cottonseed    
#> 2          2744 Eggs          
#> 3          9876 NA            
```
