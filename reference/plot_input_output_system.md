# Plot national nitrogen inputs and uses for the full agro-food system.

Builds a stacked-area plot of Spanish national nitrogen inputs (soil
inputs and imports, as negative values) against uses (feed, food, other
uses, exports) and surplus over time. A nitrogen "Accumulation" term is
added when the `n_balance_ygpit_all` pin is available.

## Usage

``` r
plot_input_output_system(per_ha = FALSE, example = FALSE)
```

## Arguments

- per_ha:

  Logical. If `TRUE`, express nitrogen flows per hectare of agricultural
  land (kg N/ha) instead of national totals (Gg N). Requires remote
  data, so it is ignored in example mode. Default is `FALSE`.

- example:

  If `TRUE`, build the plot from a small example dataset without
  downloading remote data. Default is `FALSE`.

## Value

A `ggplot` object.

## Examples

``` r
plot_input_output_system(example = TRUE)
```
