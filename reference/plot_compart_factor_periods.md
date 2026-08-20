# Plot each compartment's own factor breakdown by period, as one combined panel plot

Same factor-level breakdown as
[`plot_compart_factor()`](https://eduaguilera.github.io/whep/reference/plot_compart_factor.md),
but using the four reference-period bars from
[`decompose_terr_losses_periods()`](https://eduaguilera.github.io/whep/reference/decompose_terr_losses_periods.md)
(matching
[`plot_loss_decomp_periods()`](https://eduaguilera.github.io/whep/reference/plot_loss_decomp_periods.md))
instead of the year-on-year cumulative view, and combining all four
compartments side by side into a single patchwork plot sharing one
legend, instead of four separate ggplot objects. Each factor label has a
fixed, unique color across the whole plot, so no two factors share a
color.

## Usage

``` r
plot_compart_factor_periods(
  cropland = NULL,
  semi_natural = NULL,
  manure = NULL,
  urban = NULL
)
```

## Arguments

- cropland:

  A tibble from
  [`decompose_cropland_surplus()`](https://eduaguilera.github.io/whep/reference/decompose_cropland_surplus.md)
  with `by_period = TRUE`. If `NULL`, computed automatically (slow).

- semi_natural:

  A tibble from
  [`decompose_semi_natural_surplus()`](https://eduaguilera.github.io/whep/reference/decompose_semi_natural_surplus.md)
  with `by_period = TRUE`. If `NULL`, computed automatically (slow).

- manure:

  A tibble from
  [`decompose_manure_losses()`](https://eduaguilera.github.io/whep/reference/decompose_manure_losses.md)
  with `by_period = TRUE`. If `NULL`, computed automatically (slow).

- urban:

  A tibble from
  [`decompose_urban_losses()`](https://eduaguilera.github.io/whep/reference/decompose_urban_losses.md)
  with `by_period = TRUE`. If `NULL`, computed automatically (slow).

## Value

A patchwork ggplot object: one panel per compartment, side by side, with
a single shared legend.

## Examples

``` r
# Each argument is a by-period calculate_lmdi() table, as returned by
# decompose_cropland_surplus(by_period = TRUE).
lmdi <- tibble::tribble(
  ~period, ~period_years, ~factor_label, ~component_type, ~additive,
  "1865-1925", 60, "Size", "factor", 5400,
  "1865-1925", 60, "Intensity", "factor", 3600,
  "1865-1925", 60, "Inefficiency", "factor", -1200,
  "1925-1965", 40, "Size", "factor", 4000,
  "1925-1965", 40, "Intensity", "factor", 9200,
  "1925-1965", 40, "Inefficiency", "factor", 2800
)
panel <- plot_compart_factor_periods(lmdi, lmdi, lmdi, lmdi)
```
