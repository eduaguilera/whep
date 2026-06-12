# Primary items production

Get amount of crops, livestock and livestock products.

## Usage

``` r
get_primary_production(example = FALSE)
```

## Arguments

- example:

  If `TRUE`, return a small example output without downloading remote
  data. Default is `FALSE`.

## Value

A tibble with the item production data. It contains the following
columns:

- `year`: The year in which the recorded event occurred.

- `area_code`: Legacy numeric reporting area code.

- `polity_area_code`: Numeric WHEP reporting polity code used for matrix
  workflows. This currently matches `area_code`.

- `reporting_polity_code`: WHEP polity code for the reporting polygon.

- `reporting_polity_name`: WHEP polity name for the reporting polygon.

- `reporting_polity_has_geometry`: Whether the reporting polity has a
  polygon in the WHEP polity database.

- `item_prod_code`: FAOSTAT internal code for each produced item.

- `item_cbs_code`: FAOSTAT internal code for each commodity balance
  sheet item. The commodity balance sheet contains an aggregated version
  of production items. This field is the code for the corresponding
  aggregated item.

- `live_anim_code`: Commodity balance sheet code for the type of
  livestock that produces the livestock product. It can be:

  - `NA`: The entry is not a livestock product.

  - Non-`NA`: The code for the livestock type. The name can also be
    retrieved by using
    [`add_item_cbs_name()`](https://eduaguilera.github.io/whep/reference/add_item_cbs_name.md).

- `unit`: Measurement unit for the data. Here, keep in mind three groups
  of items: crops (e.g. `Apples and products`, `Beans`...), livestock
  (e.g. `Cattle, dairy`, `Goats`...) and livestock products (e.g.
  `Poultry Meat`, `Offals, Edible`...). Then the unit can be one of:

  - `tonnes`: Available for crops and livestock products.

  - `ha`: Hectares, available for crops.

  - `t_ha`: Tonnes per hectare, available for crops.

  - `heads`: Number of animals (stocks), available for livestock.

  - `slaughtered_heads`: Number of animals slaughtered, available for
    livestock.

  - `LU`: Standard Livestock Unit measure, available for livestock.

  - `t_head`: tonnes per head, available for livestock products.

  - `t_LU`: tonnes per Livestock Unit, available for livestock products.

- `value`: The amount of item produced, measured in `unit`.

## Examples

``` r
get_primary_production(example = TRUE)
#> # A tibble: 10 × 11
#>     year area_code polity_area_code reporting_polity_code reporting_polity_name 
#>    <dbl>     <dbl>            <int> <chr>                 <chr>                 
#>  1  1969       216              216 THA-1909-2025         Thailand              
#>  2  2018       100              100 IND-1949-2025         India                 
#>  3  1962        16               16 BGD-1947-1971         East Pakistan (1947-1…
#>  4  1974       101              101 IDN-1969-1976         Indonesia (1969-1976) 
#>  5  1990       225              225 ARE-1892-2025         United Arab Emirates  
#>  6  2005         4                4 DZA-1962-2025         Algeria (1962-2025)   
#>  7  1988       137              137 MUS-1800-2025         Mauritius             
#>  8  1981       130              130 MWI-1964-2025         Malawi                
#>  9  1962       171              171 PHL-1800-2025         Philippines           
#> 10  1964       173              173 POL-1945-2025         Poland (1945-2025)    
#> # ℹ 6 more variables: reporting_polity_has_geometry <lgl>,
#> #   item_prod_code <dbl>, item_cbs_code <dbl>, live_anim_code <dbl>,
#> #   unit <chr>, value <dbl>
```
