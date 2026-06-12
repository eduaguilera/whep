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
#> # A tibble: 10 × 9
#>     year area_code polity_area_code reporting_polity_code reporting_polity_name 
#>    <dbl>     <dbl>            <int> <chr>                 <chr>                 
#>  1  2010       174              174 PRT-1800-2025         Portugal              
#>  2  1975        54               54 DNK-1920-2025         Denmark               
#>  3  1988        53               53 BEN-1960-2025         Benin                 
#>  4  2020       178              178 ERI-1993-2025         Eritrea               
#>  5  1972       131              131 MYS-1965-2025         Malaysia              
#>  6  2011         4                4 DZA-1962-2025         Algeria (1962-2025)   
#>  7  1965       144              144 MOZ-1891-1975         Mozambique (1891-1975)
#>  8  2018       167              167 CZE-1993-2025         Czechia               
#>  9  1994       109              109 JAM-1800-2025         Jamaica               
#> 10  1982       194              194 SAU-1924-2025         Saudi Arabia          
#> # ℹ 4 more variables: reporting_polity_has_geometry <lgl>,
#> #   item_cbs_code_crop <dbl>, item_cbs_code_residue <dbl>, value <dbl>
```
