# Compute national trade flows: model vs. raw historical FAO series

Computes gross export and import flows (in MgN) by item and year, both
from the national GRAFS model (production and consumption aggregated to
Spain before splitting into export/import, avoiding the double-counting
of inter-provincial trade that a province-level sum would introduce) and
from the raw historical Export/Import series for Spain in
`Europe_FAO_completed.xlsx` (1849-1960), restricted to the item/year
combinations reported in the raw sheets. Items are classified as
`"Crop"` or `"Livestock"` for downstream aggregation, e.g. in
[`plot_national_trade_flows_raw()`](https://eduaguilera.github.io/whep/reference/plot_national_trade_flows_raw.md).

## Usage

``` r
compute_trade_flows_raw(n_nat_destiny = NULL, example = FALSE)
```

## Arguments

- n_nat_destiny:

  Optional pre-computed output from
  [`create_n_nat_destiny()`](https://eduaguilera.github.io/whep/reference/create_n_nat_destiny.md).
  If `NULL`, calls that function internally.

- example:

  If `TRUE`, return a small hardcoded output without downloading remote
  data. Default is `FALSE`.

## Value

A tibble with columns `year`, `item`, `category` (`"Crop"` or
`"Livestock"`), `source` (`"WHEP model"` or `"FAO (raw)"`), `flow`
(`"Export"` or `"Import"`), and `value_n` (MgN).

## Examples

``` r
compute_trade_flows_raw(example = TRUE)
#> # A tibble: 8 × 6
#>    year item                source    category flow   value_n
#>   <dbl> <chr>               <chr>     <chr>    <chr>    <dbl>
#> 1  1930 Nuts and products   FAO (raw) Crop     Export    825.
#> 2  1931 Nuts and products   FAO (raw) Crop     Export    966.
#> 3  1932 Nuts and products   FAO (raw) Crop     Export    850.
#> 4  1933 Nuts and products   FAO (raw) Crop     Export    928.
#> 5  1934 Nuts and products   FAO (raw) Crop     Export   1050.
#> 6  1935 Nuts and products   FAO (raw) Crop     Export   1312.
#> 7  1904 Barley and products FAO (raw) Crop     Import    584.
#> 8  1905 Barley and products FAO (raw) Crop     Import   1228.
```
