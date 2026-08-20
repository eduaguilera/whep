# Plot each compartment's own factor breakdown, year-on-year (non-cumulative)

Same as
[`plot_compart_factor()`](https://eduaguilera.github.io/whep/reference/plot_compart_factor.md),
but plots each year's own additive contribution directly, without
accumulating it over time — matching
[`plot_loss_decomp_yearly()`](https://eduaguilera.github.io/whep/reference/plot_loss_decomp_yearly.md)'s
year-on-year view, one panel per compartment.

## Usage

``` r
plot_compart_factor_yearly(
  cropland = NULL,
  semi_natural = NULL,
  manure = NULL,
  urban = NULL
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

## Value

A named list with ggplot objects `cropland`, `semi_natural`, `manure`,
and `urban`.

## Examples

``` r
lmdi <- tibble::tribble(
  ~period, ~factor_label, ~component_type, ~additive,
  "1861-1862", "N surplus", "target", 20000,
  "1861-1862", "Size", "factor", 9000,
  "1861-1862", "Intensity", "factor", 7000,
  "1862-1863", "N surplus", "target", 10000,
  "1862-1863", "Size", "factor", 5000,
  "1862-1863", "Intensity", "factor", 3000
)
plots <- plot_compart_factor_yearly(lmdi, lmdi, lmdi, lmdi)
```
