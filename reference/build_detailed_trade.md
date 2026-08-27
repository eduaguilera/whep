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

## Time extension is uniform across groups

With `extend_time = TRUE` the extension is driven by the **year axis**
of CBS only. Every `(area, item, partner, element, unit)` group observed
in any trade year is carried across the union of trade and CBS years,
and
[`fill_linear()`](https://eduaguilera.github.io/whep/reference/fill_linear.md)
interpolates inside a group's observed span and holds the first and last
observed share constant outside it. Whether CBS actually reports that
area/item/element in that year is **not** consulted, so shares are also
emitted for country-item-year cells CBS never reports.

The year axis this rests on is wide. The `"faostat-trade-bilateral"` pin
covers 1986-2021, while
[`build_commodity_balances()`](https://eduaguilera.github.io/whep/reference/build_commodity_balances.md)
defaults to 1850-2023, so 138 of the 174 extended years (79%) lie
outside the trade record entirely and carry the 1986 (or 2021) partner
mix held constant. On the full pin that is 1.17 million groups spread
over up to 174 years each, against 9.97 million observed rows, and half
of the emitted `(year, area, item, element)` cells are cells CBS never
reports (measured: 3.42 of 6.85 million). Scoping the extension to the
CBS coverage a group actually has is a methodological choice, not a bug
fix, and is tracked in issue \#232.

## Examples

``` r
build_detailed_trade(example = TRUE)
#> # A tibble: 10 × 16
#>     year area_code polity_area_code reporting_polity_code reporting_polity_name
#>    <int>     <int>            <int> <chr>                 <chr>                
#>  1  2010         4                4 DZA-1962-2025         Algeria (1962-2025)  
#>  2  2010         4                4 DZA-1962-2025         Algeria (1962-2025)  
#>  3  2015       100              100 IND-1949-2025         India                
#>  4  2015       100              100 IND-1949-2025         India                
#>  5  2018        79               79 DEU-1990-2025         Germany              
#>  6  2018        79               79 DEU-1990-2025         Germany              
#>  7  2005         4                4 DZA-1962-2025         Algeria (1962-2025)  
#>  8  2005         4                4 DZA-1962-2025         Algeria (1962-2025)  
#>  9  2012       100              100 IND-1949-2025         India                
#> 10  2012       100              100 IND-1949-2025         India                
#> # ℹ 11 more variables: reporting_polity_has_geometry <lgl>,
#> #   area_code_partner <int>, partner_polity_code <chr>,
#> #   partner_polity_name <chr>, partner_polity_has_geometry <lgl>,
#> #   partner_polity_area_code <int>, element <chr>, item_cbs_code <int>,
#> #   unit <chr>, value <dbl>, country_share <dbl>
```
