# Processed products share factors

Reports quantities of commodity balance sheet items used for
`processing` and quantities of their corresponding processed output
items.

## Usage

``` r
get_processing_coefs(version = NULL)
```

## Arguments

- version:

  File version to use as input. See
  [whep_inputs](https://eduaguilera.github.io/whep/reference/whep_inputs.md)
  for details.

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
# Note: These are smaller samples to show outputs, not the real data.
# For all data, call the function with default version (i.e. no arguments).
get_processing_coefs(version = "example")
#> ℹ Fetching files for processing_coefs...
#> # A tibble: 5,000 × 10
#>     year area_code item_cbs_code_to_process value_to_process
#>    <dbl>     <dbl>                    <dbl>            <dbl>
#>  1  2012        38                     2561           1097. 
#>  2  2009        28                     2537         372812. 
#>  3  1992       110                     2537        3581000  
#>  4  1965        51                     2555          25085  
#>  5  2008        52                     2537          94467. 
#>  6  2000       166                     2544          25108  
#>  7  1962       156                     2513          39031. 
#>  8  2010        28                     2537         374089. 
#>  9  1966       121                     2557             23.8
#> 10  2015       231                     2559        1142825. 
#> # ℹ 4,990 more rows
#> # ℹ 6 more variables: item_cbs_code_processed <dbl>,
#> #   initial_conversion_factor <dbl>, initial_value_processed <dbl>,
#> #   conversion_factor_scaling <dbl>, final_conversion_factor <dbl>,
#> #   final_value_processed <dbl>
```
