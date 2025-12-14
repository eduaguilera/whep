# Crop residue items

Get type and amount of residue produced for each crop production item.

## Usage

``` r
get_primary_residues(version = NULL)
```

## Arguments

- version:

  File version to use as input. See
  [whep_inputs](https://eduaguilera.github.io/whep/reference/whep_inputs.md)
  for details.

## Value

A tibble with the crop residue data. It contains the following columns:

- `year`: The year in which the recorded event occurred.

- `area_code`: The code of the country where the data is from. For code
  details see e.g.
  [`add_area_name()`](https://eduaguilera.github.io/whep/reference/add_area_name.md).

- `item_cbs_code_crop`: FAOSTAT internal code for each commodity balance
  sheet item. This is the crop that is generating the residue.

- `item_cbs_code_residue`: FAOSTAT internal code for each commodity
  balance sheet item. This is the obtained residue. In the commodity
  balance sheet, this can be three different items right now:

  - `2105`: `Straw`

  - `2106`: `Other crop residues`

  - `2107`: `Firewood`

  These are actually not FAOSTAT defined items, but custom defined by
  us. When necessary, FAOSTAT codes are extended for our needs.

- `value`: The amount of residue produced, measured in tonnes.

## Examples

``` r
# Note: These are smaller samples to show outputs, not the real data.
# For all data, call the function with default version (i.e. no arguments).
get_primary_residues(version = "example")
#> ℹ Fetching files for crop_residues...
#> # A tibble: 4,504 × 5
#>     year area_code item_cbs_code_crop item_cbs_code_residue    value
#>    <dbl>     <dbl>              <dbl>                 <dbl>    <dbl>
#>  1  2003        53               2570                  2107  19903. 
#>  2  2008        NA               2551                  2107 222849. 
#>  3  2001        NA               2605                  2106   2309. 
#>  4  1981       136               2605                  2106    421. 
#>  5  2019       121               2605                  2106  19703. 
#>  6  2005        40               2625                  2107   5904. 
#>  7  1978       166               2605                  2106    828. 
#>  8  1996       158               2549                  2105     92.9
#>  9  1976        10               2612                  2107   5401. 
#> 10  2012         3               2605                  2106  12086. 
#> # ℹ 4,494 more rows
```
