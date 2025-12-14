# Get area names from area codes

Add a new column to an existing tibble with the corresponding name for
each code. The codes are assumed to be from those defined by the `FABIO`
model, which them themselves come from `FAOSTAT` internal codes.
Equivalences with ISO 3166-1 numeric can be found in the *Area Codes*
CSV from the zip file that can be downloaded from
[FAOSTAT](https://www.fao.org/faostat/en/#data/FBS). TODO: Think about
this, would be nice to use ISO3 codes but won't be enough for our
periods.

## Usage

``` r
add_area_name(table, code_column = "area_code", name_column = "area_name")
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
table <- tibble::tibble(area_code = c(1, 2, 4444, 3))

add_area_name(table)
#> # A tibble: 4 × 2
#>   area_code area_name  
#>       <dbl> <chr>      
#> 1         1 Armenia    
#> 2         2 Afghanistan
#> 3      4444 NA         
#> 4         3 Albania    

table |>
  dplyr::rename(my_area_code = area_code) |>
  add_area_name(code_column = "my_area_code")
#> # A tibble: 4 × 2
#>   my_area_code area_name  
#>          <dbl> <chr>      
#> 1            1 Armenia    
#> 2            2 Afghanistan
#> 3         4444 NA         
#> 4            3 Albania    

add_area_name(table, name_column = "my_custom_name")
#> # A tibble: 4 × 2
#>   area_code my_custom_name
#>       <dbl> <chr>         
#> 1         1 Armenia       
#> 2         2 Afghanistan   
#> 3      4444 NA            
#> 4         3 Albania       
```
