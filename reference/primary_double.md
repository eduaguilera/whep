# Items with double-counting in production statistics

Identifies production items that appear both as primary crop products
and as harvested-area items, requiring special treatment to avoid
double-counting in production and biomass accounting.

## Usage

``` r
primary_double
```

## Format

A tibble where each row corresponds to one item pair with a
double-counting relationship. It contains the following columns:

- `Item_area`: Name of the item as it appears in harvested-area
  statistics (e.g., `"Seed cotton, unginned"`).

- `item_prod`: Name of the derived production item (e.g.,
  `"Cotton lint, ginned"`, `"Cotton seed"`).

- `item_prod_code`: Numeric FAOSTAT production code of the derived item.

- `Multi_type`: Classification of the double-counting type:

  - `"Primary"`: The area item is the primary crop; product is a direct
    output.

  - `"Primary_area"`: Area is recorded under a primary aggregate crop
    name.

  - `"Multi"`: Multiple products share the same harvested area.

  - `"Multi_area"`: Multiple products share a recorded area aggregate.

## Source

Derived from FAOSTAT production methodology documentation.

## Examples

``` r
head(primary_double)
#> # A tibble: 6 × 4
#>   Item_area             item_prod             item_prod_code Multi_type  
#>   <chr>                 <chr>                          <dbl> <chr>       
#> 1 Seed cotton, unginned Cotton lint, ginned              767 Primary     
#> 2 Seed cotton, unginned Cotton seed                      329 Primary     
#> 3 NA                    Seed cotton, unginned            328 Primary_area
#> 4 Oil palm fruit        Palm oil                         257 Primary     
#> 5 Oil palm fruit        Palm kernels                     256 Primary     
#> 6 NA                    Oil palm fruit                   254 Primary_area
```
