# Plot period-based drivers of territorial N losses, as one combined panel plot

Combines the two views from
[`plot_loss_decomp_periods()`](https://eduaguilera.github.io/whep/reference/plot_loss_decomp_periods.md)
(by compartment and by mechanism) side by side into a single patchwork
plot with one shared y-axis label, since both break down contributions
to the same total territorial N losses. Each panel keeps its own legend
(compartment and mechanism are different fill scales, so the legends
aren't collected into one).

## Usage

``` r
plot_loss_decomp_periods_panel(decomp = NULL)
```

## Arguments

- decomp:

  A named list from
  [`decompose_terr_losses_periods()`](https://eduaguilera.github.io/whep/reference/decompose_terr_losses_periods.md).
  If `NULL`, computed automatically (slow).

## Value

A patchwork ggplot object with two panels ("By compartment", "By
mechanism").

## Examples

``` r
if (
  requireNamespace("ggplot2", quietly = TRUE) &&
    requireNamespace("patchwork", quietly = TRUE)
) {
  decomp <- list(
    by_compartment = tibble::tribble(
      ~period, ~compartment, ~contribution_per_yr_mgn,
      "1865-1925", "cropland", 120,
      "1925-1965", "cropland", 260,
      "1865-1925", "urban", 15,
      "1925-1965", "urban", 35
    ),
    by_mechanism = tibble::tribble(
      ~period, ~mechanism, ~contribution_per_yr_mgn,
      "1865-1925", "Size", 90,
      "1925-1965", "Size", 150,
      "1865-1925", "Intensification", 45,
      "1925-1965", "Intensification", 145
    )
  )
  panel <- plot_loss_decomp_periods_panel(decomp)
}
```
