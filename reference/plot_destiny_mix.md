# Plot the cropland destiny mix over time

Plots the national cropland destiny mix (domestic food / feed / exported
food / non-food shares) from
[`decompose_destiny_mix()`](https://eduaguilera.github.io/whep/reference/decompose_destiny_mix.md)
as a line chart over time, showing the local-food -\> feed + export
transition described in the decomposition proposal (sections 3 and 14).
This is meant as a supplementary diagnostic reported alongside (not
inside) the main additive decomposition, since
[`decompose_cropland_surplus()`](https://eduaguilera.github.io/whep/reference/decompose_cropland_surplus.md)
no longer carries a destiny factor.

## Usage

``` r
plot_destiny_mix(destiny_mix = NULL)
```

## Arguments

- destiny_mix:

  A tibble from
  [`decompose_destiny_mix()`](https://eduaguilera.github.io/whep/reference/decompose_destiny_mix.md).
  If `NULL`, computed automatically (slow).

## Value

A ggplot object.

## Examples

``` r
destiny_mix <- tibble::tribble(
  ~year, ~destiny_grp, ~share,
  1960, "domestic_food", 0.55,
  1960, "feed", 0.30,
  1960, "exported", 0.10,
  1960, "non_food", 0.05,
  2000, "domestic_food", 0.35,
  2000, "feed", 0.45,
  2000, "exported", 0.15,
  2000, "non_food", 0.05
)
p <- plot_destiny_mix(destiny_mix)
```
