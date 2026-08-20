# Plot national net trade validation

Plots a time series comparing Spain's national net trade computed
bottom-up from the provincial model against official FAOSTAT figures.
Values are aggregated over all items per year.

## Usage

``` r
plot_national_trade_validation(validation = NULL)
```

## Arguments

- validation:

  Optional pre-computed output from
  [`validate_national_trade()`](https://eduaguilera.github.io/whep/reference/validate_national_trade.md).
  If `NULL`, calls that function internally.

## Value

A ggplot object.

## Examples

``` r
validation <- tibble::tribble(
  ~year, ~item, ~net_prov, ~net_fao, ~diff_net,
  1960, "Barley and products", 12000, 9500, 2500,
  1960, "Bovine Meat", -3000, -2800, -200,
  2000, "Barley and products", 56943, 2175, 54768,
  2000, "Bovine Meat", 1601, 1569, 32
)
p <- plot_national_trade_validation(validation)
```
