# Build processing coefficients

Extract the final calibrated processing coefficients from the CBS
building pipeline. These can be used independently for footprint
calculations.

## Usage

``` r
build_processing_coefs(
  cbs,
  start_year = 1850,
  end_year = 2023,
  example = FALSE
)
```

## Arguments

- cbs:

  A tibble of final CBS in wide format, as returned by
  [`build_commodity_balances()`](https://eduaguilera.github.io/whep/reference/build_commodity_balances.md).

- start_year:

  Integer. First year to include. Default `1850`.

- end_year:

  Integer. Last year to include. Default `2023`.

- example:

  Logical. If `TRUE`, return a small hardcoded dataset for illustration
  without downloading data. Default `FALSE`.

## Value

A tibble with columns: `year`, `area_code`, `item_cbs_code_to_process`,
`value_to_process`, `item_cbs_code_processed`,
`initial_conversion_factor`, `initial_value_processed`,
`conversion_factor_scaling`, `final_conversion_factor`,
`final_value_processed`.

## Examples

``` r
build_processing_coefs(example = TRUE)
#> # A tibble: 10 × 10
#>     year area_code item_cbs_code_to_process value_to_process
#>    <dbl>     <dbl>                    <dbl>            <dbl>
#>  1  2012       150                     2570         3000    
#>  2  1968       173                     2537     14230000    
#>  3  2015       150                     2558       462000    
#>  4  1885       248                     2807            0.138
#>  5  1896       191                     2544          116    
#>  6  1873        67                     2514          354    
#>  7  1987        79                     2537     25142000    
#>  8  2007        19                     2615         1044    
#>  9  1969        51                     2537      5589000    
#> 10  1984       171                     2513       104400    
#> # ℹ 6 more variables: item_cbs_code_processed <dbl>,
#> #   initial_conversion_factor <dbl>, initial_value_processed <dbl>,
#> #   conversion_factor_scaling <dbl>, final_conversion_factor <dbl>,
#> #   final_value_processed <dbl>
```
