# Crop residue items

Get type and amount of residue produced for each crop production item.

## Usage

``` r
get_primary_residues(example = FALSE)
```

## Arguments

- example:

  If `TRUE`, return a small example output without downloading remote
  data. Default is `FALSE`.

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
get_primary_residues(example = TRUE)
#> # A tibble: 10 × 5
#>     year area_code item_cbs_code_crop item_cbs_code_residue  value
#>    <dbl>     <dbl>              <dbl>                 <dbl>  <dbl>
#>  1  2010       174               2611                  2107  46260
#>  2  1975        54               2511                  2105 569199
#>  3  1988        53               2561                  2106   8213
#>  4  2020       178               2513                  2105 161992
#>  5  1972       131               2514                  2105  38845
#>  6  2011         4               2611                  2107 238808
#>  7  1965       144               2517                  2105  33688
#>  8  2018       167               2549                  2105  13578
#>  9  1994       109               2605                  2105   5597
#> 10  1982       194               2605                  2106 280552
```
