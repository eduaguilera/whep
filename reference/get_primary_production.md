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
get_primary_production(example = TRUE)
#> # A tibble: 10 × 7
#>     year area_code item_prod_code item_cbs_code live_anim_code unit        value
#>    <dbl>     <dbl>          <dbl>         <dbl>          <dbl> <chr>       <dbl>
#>  1  1969       216           1049          1049             NA heads     4.33e+6
#>  2  2018       100            265          2570             NA tonnes    1.57e+6
#>  3  1962        16            987          2746            976 t_LU      1.38e-2
#>  4  1974       101           1091          2744           1068 tonnes    3.72e+4
#>  5  1990       225            960           960             NA LU        2.39e+4
#>  6  2005         4            406          2605             NA ha        1.08e+4
#>  7  1988       137           1052          1052             NA heads     4.6 e+2
#>  8  1981       130            486          2615             NA ha        1.76e+4
#>  9  1962       171            122          2533             NA t_ha      5.15e+0
#> 10  1964       173           1037          2737           1049 t_head    1.67e-2
```
