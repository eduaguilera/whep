# Plot the crop-livestock connectivity index

Plots the national, unweighted average of local feed self-sufficiency
and the manure-recycling ratio from
[`decompose_crop_livestock_conn()`](https://eduaguilera.github.io/whep/reference/decompose_crop_livestock_conn.md)
as a line chart over time. Falling lines indicate growing crop-livestock
disconnection. This is meant as a supplementary diagnostic reported
alongside (not inside) the main additive decomposition, per the
decomposition proposal.

## Usage

``` r
plot_crop_livestock_conn(connectivity = NULL)
```

## Arguments

- connectivity:

  A named list from
  [`decompose_crop_livestock_conn()`](https://eduaguilera.github.io/whep/reference/decompose_crop_livestock_conn.md).
  If `NULL`, computed automatically (slow).

## Value

A ggplot object.

## Examples

``` r
connectivity <- list(
  national = tibble::tribble(
    ~year, ~self_sufficiency, ~recycling_ratio,
    1960, 0.95, 0.62,
    1980, 0.71, 0.48,
    2000, 0.54, 0.39
  )
)
p <- plot_crop_livestock_conn(connectivity)
```
