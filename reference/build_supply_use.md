# Supply and use tables

Create a table with processes, their inputs (*use*) and their outputs
(*supply*).

## Usage

``` r
build_supply_use(example = FALSE)
```

## Arguments

- example:

  If `TRUE`, return a small example output without downloading remote
  data. Default is `FALSE`.

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
build_supply_use(example = TRUE)
#> # A tibble: 10 × 7
#>     year area_code proc_group      proc_cbs_code item_cbs_code type      value
#>    <dbl>     <dbl> <chr>                   <dbl>         <dbl> <chr>     <dbl>
#>  1  2021       255 husbandry                1053          2106 use    1.17e+ 5
#>  2  2003        84 crop_production          2511          2105 supply 1.62e+ 6
#>  3  1982         3 husbandry                 976          2737 supply 3.67e+ 2
#>  4  2000         8 crop_production          2534          2534 supply 2.98e+ 1
#>  5  2013       170 husbandry                1049          2594 use    3.21e+ 3
#>  6  1985        75 husbandry                1190          1190 supply 1.95e+ 3
#>  7  1969       144 husbandry                1052          1052 supply 2.18e+ 0
#>  8  2010        NA husbandry                 976          2807 use    3.33e-14
#>  9  1998       115 processing               2544          2543 supply 3.77e+ 3
#> 10  1967       238 husbandry                 960          2740 supply 1.05e+ 3
```
