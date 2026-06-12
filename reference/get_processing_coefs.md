# Processed products share factors

Reports quantities of commodity balance sheet items used for
`processing` and quantities of their corresponding processed output
items.

## Usage

``` r
get_processing_coefs(example = FALSE)
```

## Arguments

- example:

  If `TRUE`, return a small example output without downloading remote
  data. Default is `FALSE`.

## Value

A tibble with the quantities for each processed product. It contains the
following columns:

- `year`: The year in which the recorded event occurred.

- `area_code`: The code of the country where the data is from. For code
  details see e.g.
  [`add_area_name()`](https://eduaguilera.github.io/whep/reference/add_area_name.md).

- `item_cbs_code_to_process`: FAOSTAT internal code for each one of the
  items that are being processed and will give other subproduct items.
  For code details see e.g.
  [`add_item_cbs_name()`](https://eduaguilera.github.io/whep/reference/add_item_cbs_name.md).

- `value_to_process`: tonnes of this item that are being processed. It
  matches the amount found in the `processing` column from the data
  obtained by
  [`get_wide_cbs()`](https://eduaguilera.github.io/whep/reference/get_wide_cbs.md).

- `item_cbs_code_processed`: FAOSTAT internal code for each one of the
  subproduct items that are obtained when processing. For code details
  see e.g.
  [`add_item_cbs_name()`](https://eduaguilera.github.io/whep/reference/add_item_cbs_name.md).

- `initial_conversion_factor`: estimate for the number of tonnes of
  `item_cbs_code_processed` obtained for each tonne of
  `item_cbs_code_to_process`. It will be used to compute the
  `final_conversion_factor`, which leaves everything balanced. TODO:
  explain how it's computed.

- `initial_value_processed`: first estimate for the number of tonnes of
  `item_cbs_code_processed` obtained from `item_cbs_code_to_process`. It
  is computed as `value_to_process * initial_conversion_factor`.

- `conversion_factor_scaling`: computed scaling needed to adapt
  `initial_conversion_factor` so as to get a final balanced total of
  subproduct quantities. TODO: explain how it's computed.

- `final_conversion_factor`: final used estimate for the number of
  tonnes of `item_cbs_code_processed` obtained for each tonne of
  `item_cbs_code_to_process`. It is computed as
  `initial_conversion_factor * conversion_factor_scaling`.

- `final_value_processed`: final estimate for the number of tonnes of
  `item_cbs_code_processed` obtained from `item_cbs_code_to_process`. It
  is computed as `initial_value_processed * final_conversion_factor`.

For the final data obtained, the quantities `final_value_processed` are
balanced in the following sense: the total sum of
`final_value_processed` for each unique tuple of
`(year, area_code, item_cbs_code_processed)` should be exactly the
quantity reported for that year, country and `item_cbs_code_processed`
item in the `production` column obtained from
[`get_wide_cbs()`](https://eduaguilera.github.io/whep/reference/get_wide_cbs.md).
This is because they are not primary products, so the amount from
'production' is actually the amount of subproduct obtained. TODO: Fix
few data where this doesn't hold.

## Examples

``` r
get_processing_coefs(example = TRUE)
#> # A tibble: 10 × 14
#>     year area_code polity_area_code reporting_polity_code reporting_polity_name
#>    <dbl>     <dbl>            <int> <chr>                 <chr>                
#>  1  1974       203              203 ESP-1800-2025         Spain                
#>  2  1991        28               28 MMR-1885-2025         Myanmar              
#>  3  1983        68               68 FRA-1919-2025         France               
#>  4  1999        68               68 FRA-1919-2025         France               
#>  5  2020       202              202 ZAF-1828-2025         South Africa         
#>  6  2010        20               20 BWA-1966-2025         Botswana             
#>  7  1972       226              226 UGA-1962-2025         Uganda               
#>  8  1974       103              103 IRQ-1921-2025         Iraq                 
#>  9  1995       230              230 UKR-1991-2014         Ukraine (1991-2014)  
#> 10  1970       223              223 TUR-1920-2025         Türkiye (1920-2025)  
#> # ℹ 9 more variables: reporting_polity_has_geometry <lgl>,
#> #   item_cbs_code_to_process <dbl>, value_to_process <dbl>,
#> #   item_cbs_code_processed <dbl>, initial_conversion_factor <dbl>,
#> #   initial_value_processed <dbl>, conversion_factor_scaling <dbl>,
#> #   final_conversion_factor <dbl>, final_value_processed <dbl>
```
