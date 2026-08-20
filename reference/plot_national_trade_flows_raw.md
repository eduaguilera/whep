# Plot national trade flows: model vs. raw historical FAO series

Plots time series of gross exports and imports, aggregated to Crop vs.
Livestock, comparing the provincial GRAFS model's bottom-up national
trade against the raw historical Export/Import series for Spain.

## Usage

``` r
plot_national_trade_flows_raw(trade_flows = NULL)
```

## Arguments

- trade_flows:

  Optional pre-computed output from
  [`compute_trade_flows_raw()`](https://eduaguilera.github.io/whep/reference/compute_trade_flows_raw.md).
  If `NULL`, calls that function internally.

## Value

A ggplot object.

## Examples

``` r
trade_flows <- tibble::tribble(
  ~year, ~item, ~source, ~category, ~flow, ~value_n,
  1930, "Nuts and products", "FAO (raw)", "Crop", "Export", 825,
  1930, "Nuts and products", "WHEP model", "Crop", "Export", 910,
  1930, "Bovine Meat", "FAO (raw)", "Livestock", "Import", 340,
  1930, "Bovine Meat", "WHEP model", "Livestock", "Import", 295
)
p <- plot_national_trade_flows_raw(trade_flows)
```
