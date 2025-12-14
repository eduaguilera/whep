# Primary items production

Get amount of crops, livestock and livestock products.

## Usage

``` r
get_primary_production(version = NULL)
```

## Arguments

- version:

  File version to use as input. See
  [whep_inputs](https://eduaguilera.github.io/whep/reference/whep_inputs.md)
  for details.

## Value

A tibble with the item production data. It contains the following
columns:

- `year`: The year in which the recorded event occurred.

- `area_code`: The code of the country where the data is from. For code
  details see e.g.
  [`add_area_name()`](https://eduaguilera.github.io/whep/reference/add_area_name.md).

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

  - `heads`: Number of animals, available for livestock.

  - `LU`: Standard Livestock Unit measure, available for livestock.

  - `t_head`: tonnes per head, available for livestock products.

  - `t_LU`: tonnes per Livestock Unit, available for livestock products.

- `value`: The amount of item produced, measured in `unit`.

## Examples

``` r
# Note: These are smaller samples to show outputs, not the real data.
# For all data, call the function with default version (i.e. no arguments).
get_primary_production(version = "example")
#> ℹ Fetching files for primary_prod...
#> # A tibble: 10,000 × 7
#>     year area_code item_prod_code item_cbs_code live_anim_code unit       value
#>    <dbl>     <dbl>          <dbl>         <dbl>          <dbl> <chr>      <dbl>
#>  1  1965        38            720          2645             NA tonnes 4460     
#>  2  2019       213           1062          2744           1052 t_LU    581.    
#>  3  1999        39            826          2671             NA t_ha      1.45  
#>  4  1968       114            772           772             NA t_ha      1     
#>  5  2001       236            406          2605             NA ha     1591     
#>  6  1984        50            603          2625             NA tonnes   45     
#>  7  1981       136            567          2605             NA ha      720     
#>  8  2005        40            547          2625             NA ha     4532     
#>  9  2009       215            977          2732            976 t_LU      0.0321
#> 10  1996       158            191          2549             NA ha      270     
#> # ℹ 9,990 more rows
```
