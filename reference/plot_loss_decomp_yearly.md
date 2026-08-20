# Plot year-on-year (non-cumulative) drivers of the change in territorial N losses

Uses the same data as
[`plot_loss_decomp()`](https://eduaguilera.github.io/whep/reference/plot_loss_decomp.md),
but plots each year's own additive contribution directly, without
accumulating it over time: each bar shows how much a compartment or
mechanism contributed to the change in territorial N losses in that one
year-on-year transition, not the running total since 1860.

## Usage

``` r
plot_loss_decomp_yearly(decomp = NULL)
```

## Arguments

- decomp:

  A named list from
  [`decompose_terr_losses()`](https://eduaguilera.github.io/whep/reference/decompose_terr_losses.md).
  If `NULL`, computed automatically (slow).

## Value

A named list with ggplot objects `by_compartment` and `by_mechanism`.

## Examples

``` r
decomp <- list(
  by_compartment = tibble::tribble(
    ~t0, ~compartment, ~contribution_mgn, ~cumulative_mgn,
    1861, "cropland", 12000, 12000,
    1862, "cropland", 8000, 20000,
    1861, "urban", 500, 500,
    1862, "urban", 700, 1200
  ),
  by_mechanism = tibble::tribble(
    ~t0, ~mechanism, ~contribution_mgn, ~cumulative_mgn,
    1861, "Size", 9000, 9000,
    1862, "Size", 5000, 14000,
    1861, "Intensification", 3500, 3500,
    1862, "Intensification", 3700, 7200
  )
)
plots <- plot_loss_decomp_yearly(decomp)
```
