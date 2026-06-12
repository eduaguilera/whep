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
#> # A tibble: 10 × 14
#>     year area_code polity_area_code reporting_polity_code reporting_polity_name 
#>    <dbl>     <dbl>            <int> <chr>                 <chr>                 
#>  1  2012       150              150 NLD-1830-2025         Netherlands           
#>  2  1968       173              173 POL-1945-2025         Poland (1945-2025)    
#>  3  2015       150              150 NLD-1830-2025         Netherlands           
#>  4  1885       248              248 F248-1918-1919        Yugoslavia (1918-1919)
#>  5  1896       191              191 VCT-1833-2025         Saint Vincent and the…
#>  6  1873        67               67 FIN-1809-1917         Grand Duchy of Finland
#>  7  1987        79               79 DEU-1949-1990         Germany (divided, 194…
#>  8  2007        19               19 BOL-1938-2025         Bolivia               
#>  9  1969        51               51 F51-1947-1993         Czechoslovakia (1947-…
#> 10  1984       171              171 PHL-1800-2025         Philippines           
#> # ℹ 9 more variables: reporting_polity_has_geometry <lgl>,
#> #   item_cbs_code_to_process <dbl>, value_to_process <dbl>,
#> #   item_cbs_code_processed <dbl>, initial_conversion_factor <dbl>,
#> #   initial_value_processed <dbl>, conversion_factor_scaling <dbl>,
#> #   final_conversion_factor <dbl>, final_value_processed <dbl>
```
