# Plot each compartment's own factor breakdown

Plots panels 3-6 of the composite decomposition figure described in the
decomposition proposal (section 14): the cumulative, year-on-year
contribution of each factor *within* one compartment's own decomposition
(e.g. Cropland's Size/Intensity/Inefficiency), as opposed to the
AFS-wide panels 1-2 from
[`plot_loss_decomp()`](https://eduaguilera.github.io/whep/reference/plot_loss_decomp.md),
which only show each compartment's total contribution.

## Usage

``` r
plot_compart_factor(
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
# Each argument is a calculate_lmdi() table; the same shape is reused for
# all four compartments here.
lmdi <- tibble::tribble(
  ~period, ~factor_label, ~component_type, ~additive,
  "1861-1862", "N surplus", "target", 20000,
  "1861-1862", "Size", "factor", 9000,
  "1861-1862", "Intensity", "factor", 7000,
  "1861-1862", "Inefficiency", "factor", 4000,
  "1862-1863", "N surplus", "target", 10000,
  "1862-1863", "Size", "factor", 5000,
  "1862-1863", "Intensity", "factor", 3000,
  "1862-1863", "Inefficiency", "factor", 2000
)
plots <- plot_compart_factor(lmdi, lmdi, lmdi, lmdi)
```
