# Supply and use tables

Create a table with processes, their inputs (*use*) and their outputs
(*supply*).

## Usage

``` r
build_supply_use(
  cbs_version = NULL,
  feed_intake_version = NULL,
  primary_prod_version = NULL,
  primary_residues_version = NULL,
  processing_coefs_version = NULL
)
```

## Arguments

- cbs_version:

  File version passed to
  [`get_wide_cbs()`](https://eduaguilera.github.io/whep/reference/get_wide_cbs.md)
  call.

- feed_intake_version:

  File version passed to
  [`get_feed_intake()`](https://eduaguilera.github.io/whep/reference/get_feed_intake.md)
  call.

- primary_prod_version:

  File version passed to
  [`get_primary_production()`](https://eduaguilera.github.io/whep/reference/get_primary_production.md)
  call.

- primary_residues_version:

  File version passed to
  [`get_primary_residues()`](https://eduaguilera.github.io/whep/reference/get_primary_residues.md)
  call.

- processing_coefs_version:

  File version passed to
  [`get_processing_coefs()`](https://eduaguilera.github.io/whep/reference/get_processing_coefs.md)
  call.

## Value

A tibble with the supply and use data for processes. It contains the
following columns:

- `year`: The year in which the recorded event occurred.

- `area_code`: The code of the country where the data is from. For code
  details see e.g.
  [`add_area_name()`](https://eduaguilera.github.io/whep/reference/add_area_name.md).

- `proc_group`: The type of process taking place. It can be one of:

  - `crop_production`: Production of crops and their residues, e.g. rice
    production, coconut production, etc.

  - `husbandry`: Animal husbandry, e.g. dairy cattle husbandry,
    non-dairy cattle husbandry, layers chickens farming, etc.

  - `processing`: Derived subproducts obtained from processing other
    items. The items used as inputs are those that have a non-zero
    processing use in the commodity balance sheet. See
    [`get_wide_cbs()`](https://eduaguilera.github.io/whep/reference/get_wide_cbs.md)
    for more details. In each process there is a single input. In some
    processes like olive oil extraction or soyabean oil extraction this
    might make sense. Others like alcohol production need multiple
    inputs (e.g. multiple crops work), so in this data there would not
    be a process like alcohol production but rather a *virtual* process
    like 'Wheat and products processing', giving all its possible
    outputs. This is a constraint because of how the data was obtained
    and might be improved in the future. See
    [`get_processing_coefs()`](https://eduaguilera.github.io/whep/reference/get_processing_coefs.md)
    for more details.

- `proc_cbs_code`: The code of the main item in the process taking
  place. Together with `proc_group`, these two columns uniquely
  represent a process. The main item is predictable depending on the
  value of `proc_group`:

  - `crop_production`: The code is from the item for which seed usage
    (if any) is reported in the commodity balance sheet (see
    [`get_wide_cbs()`](https://eduaguilera.github.io/whep/reference/get_wide_cbs.md)
    for more). For example, the rice code for a rice production process
    or the cottonseed code for the cotton production one.

  - `husbandry`: The code of the farmed animal, e.g. bees for
    beekeeping, non-dairy cattle for non-dairy cattle husbandry, etc.

  - `processing`: The code of the item that is used as input, i.e., the
    one that is processed to get other derived products. This uniquely
    defines a process within the group because of the nature of the data
    that was used, which you can see in
    [`get_processing_coefs()`](https://eduaguilera.github.io/whep/reference/get_processing_coefs.md).

  For code details see e.g.
  [`add_item_cbs_name()`](https://eduaguilera.github.io/whep/reference/add_item_cbs_name.md).

- `item_cbs_code`: The code of the item produced or used in the process.
  Note that this might be the same value as `proc_cbs_code`, e.g., in
  rice production process for the row defining the amount of rice
  produced or the amount of rice seed as input, but it might also have a
  different value, e.g. for the row defining the amount of straw residue
  from rice production. For code details see e.g.
  [`add_item_cbs_name()`](https://eduaguilera.github.io/whep/reference/add_item_cbs_name.md).

- `type`: Can have two values:

  - `use`: The given item is an input of the process.

  - `supply`: The given item is an output of the process.

- `value`: Quantity in tonnes.

## Examples

``` r
# Note: These are smaller samples to show outputs, not the real data.
# For all data, call the function with default versions (i.e. no arguments).
build_supply_use(
  cbs_version = "example",
  feed_intake_version = "example",
  primary_prod_version = "example",
  primary_residues_version = "example",
  processing_coefs_version = "example"
)
#> ℹ Fetching files for primary_prod...
#> ℹ Fetching files for crop_residues...
#> ℹ Fetching files for commodity_balance_sheet...
#> ℹ Fetching files for feed_intake...
#> ℹ Fetching files for processing_coefs...
#> # A tibble: 27,914 × 7
#>     year area_code proc_group      proc_cbs_code item_cbs_code type       value
#>    <dbl>     <dbl> <chr>                   <dbl>         <dbl> <chr>      <dbl>
#>  1  1965        38 crop_production          2645          2645 supply     4460 
#>  2  1984        50 crop_production          2625          2625 supply       45 
#>  3  2003       131 crop_production          2577          2577 supply 13354800 
#>  4  1996       235 crop_production          2517          2517 supply     1600 
#>  5  2021        10 crop_production          2511          2511 supply 31922555.
#>  6  1970        41 crop_production          2605          2605 supply     7000 
#>  7  1997       222 crop_production          2605          2605 supply    12000 
#>  8  2003        59 crop_production          2662          2662 supply     2049.
#>  9  1966       236 crop_production          2549          2549 supply     5891 
#> 10  1976       105 crop_production          2533          2533 supply      800 
#> # ℹ 27,904 more rows
```
