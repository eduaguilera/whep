# Plot livestock and agro-food system input-output panels side by side

Combines
[`plot_input_output_livestock()`](https://eduaguilera.github.io/whep/reference/plot_input_output_livestock.md)
and
[`plot_input_output_system()`](https://eduaguilera.github.io/whep/reference/plot_input_output_system.md)
into a single two-panel figure. When `per_ha = TRUE`, both panels are
normalized by the same total national agricultural area – unlike
[`plot_input_output_land_panel()`](https://eduaguilera.github.io/whep/reference/plot_input_output_land_panel.md),
whose two panels each use a different, smaller land-use area. The y-axis
label and caption only mention per-hectare units when normalization
actually took effect (it silently falls back to national totals if the
area data could not be loaded), so the figure never claims a unit it did
not use.

## Usage

``` r
plot_input_output_total_panel(per_ha = FALSE, example = FALSE)
```

## Arguments

- per_ha:

  Logical. If `TRUE`, express nitrogen flows per hectare of total
  national agricultural land (kg N/ha) instead of national totals (Gg
  N). Requires remote data, so it is ignored in example mode. Default is
  `FALSE`.

- example:

  If `TRUE`, build both panels from a small example dataset without
  downloading remote data. Default is `FALSE`.

## Value

A patchwork ggplot object.

## Examples

``` r
if (
  requireNamespace("ggplot2", quietly = TRUE) &&
    requireNamespace("patchwork", quietly = TRUE)
) {
  plot_input_output_total_panel(example = TRUE)
}
```
