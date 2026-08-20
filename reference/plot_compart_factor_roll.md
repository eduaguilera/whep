# Plot each compartment's own factor breakdown, rolling mean

Same as
[`plot_compart_factor_yearly()`](https://eduaguilera.github.io/whep/reference/plot_compart_factor_yearly.md),
but smooths each year's own additive contribution with a centered
rolling mean (`window` years wide, `NA`-padded at the edges) before
plotting, one panel per compartment.

## Usage

``` r
plot_compart_factor_roll(
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

A named list with ggplot objects `cropland`, `semi_natural`, `manure`,
and `urban`.

## Examples

``` r
# `window` must not exceed the number of periods available per factor.
lmdi <- tibble::tribble(
  ~period, ~factor_label, ~component_type, ~additive,
  "1861-1862", "N surplus", "target", 20000,
  "1861-1862", "Size", "factor", 9000,
  "1862-1863", "N surplus", "target", 10000,
  "1862-1863", "Size", "factor", 5000,
  "1863-1864", "N surplus", "target", 12000,
  "1863-1864", "Size", "factor", 6000
)
plots <- plot_compart_factor_roll(lmdi, lmdi, lmdi, lmdi, window = 3)
```
