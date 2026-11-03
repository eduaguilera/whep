# Create GRAFS plot dataset.

Combines land input data and N flows from crops, livestock, imports, and
exports to generate a dataset of nitrogen (MgN) by province and year,
ready to feed the GRAFS plot package offered by Alfredo Rodriguez.

## Usage

``` r
create_grafs_plot_df(
  example = FALSE,
  method_crop_allocation = c("processing_origin", "pooled_equal")
)
```

## Arguments

- example:

  If `TRUE`, return a small example output without downloading remote
  data. Default is `FALSE`.

- method_crop_allocation:

  Allocation of processed crop flows. `processing_origin` uses the same
  province-year parent crop and irrigation support as the flow builder,
  preserving field-residue crop origins and any known irrigation regime.
  Positive flows without support abort. `pooled_equal` explicitly
  selects legacy pooled national parent shares and assumed equal
  irrigation shares where local support is missing. Neither method
  estimates observed industrial plant locations.

## Value

A tibble with columns `province`, `year`, `label`, `data`, `align`, and
`arrowColor`, and `method_crop_allocation`. Each `label` is a GRAFS
template token (e.g. `{ARAiN}`) and `data` holds its value as a
character string.

## Examples

``` r
create_grafs_plot_df(example = TRUE)
#> # A tibble: 10 × 7
#>    province  year label            data  align arrowColor method_crop_allocation
#>    <chr>    <dbl> <chr>            <chr> <chr> <chr>      <chr>                 
#>  1 Huesca    2000 {ARAiN}          12.34 R     ""         processing_origin     
#>  2 Huesca    2000 {CROPS_TO_LIVES… 45.6  L     ""         processing_origin     
#>  3 Huesca    2000 {POPULATIONM}    0.22  L     ""         processing_origin     
#>  4 Huesca    2000 {PROVINCE_NAME}  Hues… L     ""         processing_origin     
#>  5 Huesca    2000 {WIDTH_MAX}      1500  L     ""         processing_origin     
#>  6 Huesca    2000 {YEAR}           2000  L     ""         processing_origin     
#>  7 Lleida    2000 {ARArN}          8.9   R     ""         processing_origin     
#>  8 Lleida    2000 {LVSTCKTOTN}     3.21  L     ""         processing_origin     
#>  9 Spain     2000 {CRPLNDTOTN}     120.5 R     ""         processing_origin     
#> 10 Spain     2000 {POPULATIONM}    40.1  L     ""         processing_origin     
```
