# Plot national nitrogen inputs, production, and surplus for a land system.

Builds a stacked-area plot of Spanish national nitrogen inputs (as
negative values), production, residues, and surplus over time for either
cropland or semi-natural agroecosystems. For the semi-natural system a
nitrogen "Accumulation" term (net soil/biomass N accumulation) is added
when the `n_balance_ygpit_all` pin is available.

## Usage

``` r
plot_input_output(
  system = c("Cropland", "semi_natural_agroecosystems"),
  per_ha = FALSE,
  example = FALSE
)
```

## Arguments

- system:

  Character. One of `"Cropland"` or `"semi_natural_agroecosystems"`.

- per_ha:

  Logical. If `TRUE`, express nitrogen flows per hectare of the system's
  land area (kg N/ha) instead of national totals (Gg N). Requires remote
  data, so it is ignored in example mode. Default is `FALSE`.

- example:

  If `TRUE`, build the plot from a small example dataset without
  downloading remote data. Default is `FALSE`.

## Value

A `ggplot` object.

## Examples

``` r
plot_input_output(example = TRUE)
```
