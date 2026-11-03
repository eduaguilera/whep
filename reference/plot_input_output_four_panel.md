# Plot all four input-output panels together

Combines
[`plot_input_output()`](https://eduaguilera.github.io/whep/reference/plot_input_output.md)
for `"Cropland"` and `"semi_natural_agroecosystems"`,
[`plot_input_output_livestock()`](https://eduaguilera.github.io/whep/reference/plot_input_output_livestock.md),
and
[`plot_input_output_system()`](https://eduaguilera.github.io/whep/reference/plot_input_output_system.md)
into a single four-panel (2x2) figure of national nitrogen totals (Gg
N). There is no `per_ha` argument: per hectare, the four systems do not
share one area basis (Cropland and semi-natural agroecosystems each use
their own land-use area; Livestock and the agro-food system use total
agricultural area – see
[`plot_input_output_land_panel()`](https://eduaguilera.github.io/whep/reference/plot_input_output_land_panel.md)
and
[`plot_input_output_total_panel()`](https://eduaguilera.github.io/whep/reference/plot_input_output_total_panel.md)),
so folding all four normalized panels into one grid would silently mix
two different denominators. Use those two functions for the per-hectare
view instead, kept as two separate figures for that reason.

## Usage

``` r
plot_input_output_four_panel(example = FALSE)
```

## Arguments

- example:

  If `TRUE`, build all four panels from a small example dataset without
  downloading remote data. Default is `FALSE`.

## Value

A patchwork ggplot object.

## Examples

``` r
if (
  requireNamespace("ggplot2", quietly = TRUE) &&
    requireNamespace("patchwork", quietly = TRUE)
) {
  plot_input_output_four_panel(example = TRUE)
}
#> Warning: no non-missing arguments to min; returning Inf
#> Warning: Position guide is perpendicular to the intended axis.
#> ℹ Did you mean to specify a different guide `position`?
#> Warning: No shared levels found between `names(values)` of the manual scale and the
#> data's fill values.
```
