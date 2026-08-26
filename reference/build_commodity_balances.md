# Build commodity balance sheets

Construct commodity balance sheets (CBS) from raw FAOSTAT data. This is
a convenience wrapper that chains the three pipeline steps:

1.  `.read_cbs()` — read & reformat FAOSTAT CBS data.

2.  `.fix_cbs()` — processing calibration, trade imputation, destiny
    filling, and final balancing.

3.  `.qc_cbs()` — flag data-quality anomalies.

## Usage

``` r
build_commodity_balances(
  primary_all,
  start_year = 1850,
  end_year = 2023,
  smooth_carry_forward = FALSE,
  example = FALSE,
  historical_data = NULL,
  format = c("long", "wide"),
  trade_recovery = c("none", "net_import"),
  .fixed_data = NULL
)
```

## Arguments

- primary_all:

  A tibble of primary production, as returned by
  [`build_primary_production()`](https://eduaguilera.github.io/whep/reference/build_primary_production.md).

- start_year:

  Integer. First year to include. Default `1850`.

- end_year:

  Integer. Last year to include. Default `2023`.

- smooth_carry_forward:

  Logical. If `TRUE`, carry-forward tails are replaced with a linear
  trend. Default `FALSE`.

- example:

  Logical. If `TRUE`, return a small hardcoded example tibble instead of
  reading remote data. Default `FALSE`.

- historical_data:

  Optional harmonized historical CBS or production rows to add before
  the CBS historical extension. May be a data frame or a path to a
  parquet/csv file. CBS-shaped rows should provide `year`, `value`, one
  of `area_code` or `polity_area_code`, one of `item_cbs_code` or
  `item_prod_code`, and preferably `element`. Production-shaped rows
  without `element` are accepted as `production` when their unit is
  tonnes. **Rice supplied here is assumed to be on a paddy (rough-rice)
  basis** and is multiplied by the paddy-to-milled extraction rate,
  matching
  [`build_primary_production()`](https://eduaguilera.github.io/whep/reference/build_primary_production.md);
  pre-divide by that rate if the series is already milled. Default
  `NULL`.

- format:

  One of `"long"` (default) or `"wide"`. `"long"` returns one row per
  element. `"wide"` pivots the elements into columns, adds the
  live-animal rows that the FAO sheet omits, and checks the supply-use
  identity. Both are the same dataset; `"wide"` is what the IO model and
  the extensions consume.

- trade_recovery:

  One of `"none"` (default) or `"net_import"`, selecting what happens to
  a traded item the CBS has no row for. The trade record is joined onto
  the CBS, so it can only fill a row that already exists; `"none"` keeps
  that, and the import is dropped. `"net_import"` first creates the
  missing rows from the trade record, restricted to tonnes-denominated
  items (live-animal trade is in heads and arrives through
  [`get_livestock_cbs()`](https://eduaguilera.github.io/whep/reference/get_livestock_cbs.md)),
  to net importers, and to areas the CBS already covers in that year. It
  **moves published values**; `NEWS.md` states by how much and whep#762
  keeps the remaining decisions open.
  [`get_wide_cbs()`](https://eduaguilera.github.io/whep/reference/get_wide_cbs.md)
  always uses `"none"`; ask for `format = "wide"` here to get the wide
  table with recovery applied.

- .fixed_data:

  Optional tibble with the same structure as the output of the internal
  `.read_cbs() |> .fix_cbs()` steps. When supplied, `primary_all` is
  ignored and the pipeline skips directly to `.qc_cbs()`. Default
  `NULL`.

## Value

For `format = "long"`, a tibble with columns: `year`, legacy numeric
`area_code`, numeric `polity_area_code`, `reporting_polity_code`,
`reporting_polity_name`, `reporting_polity_has_geometry`,
`item_cbs_code`, `element` (e.g. `"production"`, `"import"`, `"food"`),
`value`, `source`, and `fao_flag`. For `format = "wide"`, the elements
become one column each, `stock_variation` is split into the non-negative
`stock_addition` and `stock_withdrawal`, and `domestic_supply` is total
use excluding `export`.

## Examples

``` r
build_commodity_balances(example = TRUE)
#> # A tibble: 10 × 11
#>     year area_code polity_area_code reporting_polity_code reporting_polity_name 
#>    <dbl>     <dbl>            <int> <chr>                 <chr>                 
#>  1  2010       120              120 LAO-1954-2025         Laos                  
#>  2  1981       222              222 TUN-1881-2025         Tunisia               
#>  3  1906       203              203 ESP-1800-2025         Spain                 
#>  4  1899       175              175 GNB-1886-1974         Guinea-Bissau (1886-1…
#>  5  2018        48               48 CRI-1800-2025         Costa Rica            
#>  6  1871        10               10 AUS-1901-2025         Australia             
#>  7  1938       226              226 UGA-1926-1962         Uganda (1926-1962)    
#>  8  1924        11               11 AUT-1919-2025         Austria               
#>  9  1928        96               96 HKG-1842-2025         Hong Kong             
#> 10  1879       236              236 VEN-1821-2025         Venezuela             
#> # ℹ 6 more variables: reporting_polity_has_geometry <lgl>, item_cbs_code <dbl>,
#> #   element <chr>, value <dbl>, source <chr>, fao_flag <chr>
```
