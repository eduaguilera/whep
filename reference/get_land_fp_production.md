# Get land footprint data for local production.

Read and clean the `land_fp` input file, returning only land-use entries
from local production (`Impact == "Land"` and `Origin == "Production"`).

## Usage

``` r
get_land_fp_production(example = FALSE)
```

## Arguments

- example:

  If `TRUE`, return a small example output without downloading remote
  data. Default is `FALSE`.

## Value

A tibble with columns:

- `year`: Year.

- `area_code`: Numeric area code.

- `item_cbs_code`: Commodity balance sheet item code.

- `impact`: Impact category.

- `element`: Source element.

- `origin`: Origin type.

- `group`: Group category.

- `impact_u`: Land footprint value (TODO: find which unit).

## Examples

``` r
get_land_fp_production(example = TRUE)
#> # A tibble: 2 × 8
#>    year area_code item_cbs_code impact element  origin     group impact_u
#>   <int>     <int>         <int> <chr>  <chr>    <chr>      <chr>    <dbl>
#> 1  2020        32          2511 Land   Cropland Production Crops     1000
#> 2  2020        76          2514 Land   Cropland Production Crops      800
```
