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
  trade_zero = .cbs_trade_zero_choices(),
  share_overflow = .cbs_share_overflow_choices(),
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
  to net importers, and to areas the CBS already covers in that year.
  Selecting it **moves published values** — at 2010 it adds 1,164 keys
  and 53.7 Mt of imports, and reclassifies three areas on the
  nourishment axis. `NEWS.md` states the rest, and whep#762 keeps the
  remaining decisions open.
  [`get_wide_cbs()`](https://eduaguilera.github.io/whep/reference/get_wide_cbs.md)
  always uses `"none"`; ask for `format = "wide"` here to get the wide
  table with recovery applied.

- trade_zero:

  One of `"prefer_record"` (default) or `"keep"`, selecting what happens
  when the CBS carries a **zero** import or export and the trade record
  for the same `(year, area_code, item_cbs_code)` carries a positive
  quantity. The trade record is filled in with
  [`dplyr::coalesce()`](https://dplyr.tidyverse.org/reference/coalesce.html),
  which replaces only a missing value, so the zero used to stand and the
  trade was discarded. `"prefer_record"` takes the positive trade
  quantity instead, because such a zero is not an observation: measured
  at 2010, every one of them carries FAO flag `"I"` (imputed) or the
  legacy `"S"` (standardized), and neither food-balance vintage carries
  a single official `"A"` on a trade row. A non-zero CBS value is never
  overwritten and a zero trade record never overwrites anything, so the
  fill can only add trade. `"keep"` restores the pre-whep#866 behaviour.
  **The default moves published values** — at 2010 it raises 4,493
  import keys by 9.70 Mt and 3,771 export keys by 10.27 Mt, moving
  26,538 published rows over 180 areas; see `NEWS.md`. The conflict
  count is reported by every build under either setting.

- share_overflow:

  One of `"report"` (default), `"clamp"`, `"drop"` or `"abort"`,
  selecting what happens when a pre-1962 destiny share exceeds 1 — a
  destiny larger than the `domestic_supply` it is apportioned from
  (whep#980). Measured on a real 1950–1965 build, 108 of 207,816 rows
  do: `other_uses` 70, `processing_primary` 19, `food` 15, `feed` 4.
  They are 1.03% of the 1961 `other_uses` mass and 0.17% of the `food`
  mass. The cause is not this arithmetic: 89 of them are FAOSTAT's own
  1961 balances not closing (the non-food Commodity Balances, which
  carry tobacco, hides and skins, silk, wool and fibres as `other_uses`
  and which no better-ranked source overwrites), and the other 19 are
  hops in net-exporting years, whose `processing_primary` is the whole
  production by construction. `"report"` therefore keeps every value as
  measured and only warns, so it **moves no published value**; it names
  the count, the split by destiny and the three largest. `"clamp"` caps
  the share at 1, `"drop"` sets it to `NA` so the key is filled from a
  neighbouring year instead (and booked as 0 where the violating year is
  the only observation), and `"abort"` refuses to build. Which of those
  is right is an open question — see whep#980 — so the reporting default
  is the one that invents nothing.

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

`fao_flag` is FAOSTAT's own observation-status code for the value, taken
from the source that `source` names (`"A"` official, `"E"` estimated,
`"I"` imputed, `"S"` standardized, `"SD"`, `"X"`). It is `NA` wherever
the number is not one FAOSTAT published under a flag: a WHEP-derived row
(the processing pathway, the destiny gap-fills, the pre-1961 historical
extension), a row whose source carries no flag, and a row summed or
averaged from parts whose flags disagree. The flag is a claim about the
value, so it is dropped rather than guessed when the parts do not agree.
Rows sourced from `"FAOSTAT_prod"` are `NA` today because
[`build_primary_production()`](https://eduaguilera.github.io/whep/reference/build_primary_production.md)
does not carry the flag out of the production pin.

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
