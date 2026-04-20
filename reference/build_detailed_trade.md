# Build detailed bilateral trade matrix

Construct the detailed bilateral trade matrix (DTM) from the FAOSTAT
Detailed Trade Matrix pin. Reports trade flows between pairs of
countries with their trade shares, aggregated to polity level and mapped
to CBS item codes.

Optionally extends the time series by joining with commodity balance
sheet years and gap-filling country shares via linear interpolation.

## Usage

``` r
build_detailed_trade(
  raw_trade = NULL,
  cbs = NULL,
  min_share = 1e-04,
  extend_time = FALSE,
  example = FALSE
)
```

## Arguments

- raw_trade:

  A data.table or tibble of raw FAOSTAT bilateral trade data. If `NULL`
  (default), the data is read from the `"faostat-trade-bilateral"` pin.

- cbs:

  A tibble of commodity balance sheets in wide format, as returned by
  [`build_commodity_balances()`](https://eduaguilera.github.io/whep/reference/build_commodity_balances.md)
  or
  [`get_wide_cbs()`](https://eduaguilera.github.io/whep/reference/get_wide_cbs.md).
  Required when `extend_time = TRUE`.

- min_share:

  Numeric. Partners with a country share below this threshold are
  dropped when extending time. Default `0.0001`.

- extend_time:

  Logical. If `TRUE`, extend the time series using CBS years and linear
  interpolation of country shares. Default `FALSE`.

- example:

  Logical. If `TRUE`, return a small example tibble without downloading
  remote data. Default `FALSE`.

## Value

A tibble with columns:

- `year`: Integer year.

- `area_code`: Numeric polity code of the reporter country.

- `area_code_partner`: Numeric polity code of the partner country.

- `element`: Either `"import"` or `"export"`.

- `item_cbs_code`: Numeric CBS item code.

- `unit`: Measurement unit (`"tonnes"` or `"heads"`).

- `value`: Trade quantity.

- `country_share`: Share of total trade for this partner.

## Examples

``` r
build_detailed_trade(example = TRUE)
#> # A tibble: 10 × 8
#>     year area_code area_code_partner element item_cbs_code unit    value
#>    <int>     <int>             <int> <chr>           <int> <chr>   <dbl>
#>  1  2010         4               100 import           2511 tonnes 125000
#>  2  2010         4                79 import           2511 tonnes  89000
#>  3  2015       100                 4 export           2536 tonnes  45000
#>  4  2015       100                79 export           2536 tonnes  72000
#>  5  2018        79                 4 import           2807 tonnes 310000
#>  6  2018        79               100 import           2807 tonnes 150000
#>  7  2005         4                79 export           2555 tonnes  63000
#>  8  2005         4               100 export           2555 tonnes  28000
#>  9  2012       100                 4 import           2570 tonnes  98000
#> 10  2012       100                79 import           2570 tonnes  54000
#> # ℹ 1 more variable: country_share <dbl>
```
