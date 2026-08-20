# Plot each compartment's own factor breakdown, rolling mean, as one combined panel plot

Same factor-level breakdown as
[`plot_compart_factor_roll()`](https://eduaguilera.github.io/whep/reference/plot_compart_factor_roll.md),
combining all four compartments side by side into a single patchwork
plot, matching
[`plot_compart_factor_periods()`](https://eduaguilera.github.io/whep/reference/plot_compart_factor_periods.md)'s
style: one shared y-axis per axis-sharing pair (Cropland+Semi-natural
share "N surpluses", Livestock+Urban share "N losses"), one shared
legend, fixed unique colors per factor label.

## Usage

``` r
plot_compart_factor_roll_panel(
  cropland = NULL,
  semi_natural = NULL,
  manure = NULL,
  urban = NULL,
  window = 10
)
```

## Arguments

- cropland:

  A tibble from
  [`decompose_cropland_surplus()`](https://eduaguilera.github.io/whep/reference/decompose_cropland_surplus.md).
  If `NULL`, computed automatically (slow).

- semi_natural:

  A tibble from
  [`decompose_semi_natural_surplus()`](https://eduaguilera.github.io/whep/reference/decompose_semi_natural_surplus.md).
  If `NULL`, computed automatically (slow).

- manure:

  A tibble from
  [`decompose_manure_losses()`](https://eduaguilera.github.io/whep/reference/decompose_manure_losses.md).
  If `NULL`, computed automatically (slow).

- urban:

  A tibble from
  [`decompose_urban_losses()`](https://eduaguilera.github.io/whep/reference/decompose_urban_losses.md).
  If `NULL`, computed automatically (slow).

- window:

  Width of the centered rolling-mean window, in years. Default `10`.

## Value

A patchwork ggplot object: one panel per compartment, side by side, with
a single shared legend.

## Examples

``` r
lmdi <- tibble::tribble(
  ~period, ~factor_label, ~component_type, ~additive,
  "1861-1862", "N surplus", "target", 20000,
  "1861-1862", "Size", "factor", 9000,
  "1862-1863", "N surplus", "target", 10000,
  "1862-1863", "Size", "factor", 5000,
  "1863-1864", "N surplus", "target", 12000,
  "1863-1864", "Size", "factor", 6000
)
panel <- plot_compart_factor_roll_panel(
  lmdi,
  lmdi,
  lmdi,
  lmdi,
  window = 3
)
```
