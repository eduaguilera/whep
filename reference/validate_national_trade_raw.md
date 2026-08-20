# Validate national net trade against raw historical FAO series

Validates the provincial GRAFS model's bottom-up national net trade
against the original historical Export/Import series for Spain contained
in `Europe_FAO_completed.xlsx` (1849-1960), the raw source data behind
the package's processed trade figures. Comparison is restricted to the
item and year combinations actually reported in the raw sheets, since
coverage there is sparser than in the processed dataset.

## Usage

``` r
validate_national_trade_raw(n_prov_destiny = NULL, example = FALSE)
```

## Arguments

- n_prov_destiny:

  Optional pre-computed output from
  [`create_n_prov_destiny()`](https://eduaguilera.github.io/whep/reference/create_n_prov_destiny.md).
  If `NULL`, calls that function internally.

- example:

  If `TRUE`, return a small hardcoded output without downloading remote
  data. Default is `FALSE`.

## Value

A tibble with columns `year`, `item`, `net_prov`, `net_fao`, and
`diff_net` (all in MgN).

## Examples

``` r
validate_national_trade_raw(example = TRUE)
#> # A tibble: 6 × 5
#>    year item              net_fao net_prov diff_net
#>   <dbl> <chr>               <dbl>    <dbl>    <dbl>
#> 1  1930 Nuts and products    820.  -19777.  -20598.
#> 2  1931 Nuts and products    964.  -19818.  -20782 
#> 3  1932 Nuts and products    850.  -19844.  -20694.
#> 4  1933 Nuts and products    927.  -18580   -19507 
#> 5  1934 Nuts and products   1049.  -17606.  -18655.
#> 6  1935 Nuts and products   1312.  -18918.  -20230.
```
