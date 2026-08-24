# Plot rolling-mean year-on-year drivers of territorial N losses, as one combined panel plot

Combines the two views from
[`plot_loss_decomp_rolling()`](https://eduaguilera.github.io/whep/reference/plot_loss_decomp_rolling.md)
(by compartment and by mechanism) side by side into a single patchwork
plot, matching
[`plot_loss_decomp_periods_panel()`](https://eduaguilera.github.io/whep/reference/plot_loss_decomp_periods_panel.md)'s
style: one shared y-axis label, each panel keeping its own legend
(compartment and mechanism are different fill scales, so the legends
aren't collected into one).

## Usage

``` r
plot_loss_decomp_rolling_panel(decomp = NULL, window = 10)
```

## Arguments

- decomp:

  A named list from
  [`decompose_terr_losses()`](https://eduaguilera.github.io/whep/reference/decompose_terr_losses.md).
  If `NULL`, computed automatically (slow).

- window:

  Width of the centered rolling-mean window, in years. Default `10`.

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
      ~t0, ~compartment, ~contribution_mgn, ~cumulative_mgn,
      1861, "cropland", 12000, 12000,
      1862, "cropland", 8000, 20000,
      1863, "cropland", 9000, 29000
    ),
    by_mechanism = tibble::tribble(
      ~t0, ~mechanism, ~contribution_mgn, ~cumulative_mgn,
      1861, "Size", 9000, 9000,
      1862, "Size", 5000, 14000,
      1863, "Size", 6000, 20000
    )
  )
  panel <- plot_loss_decomp_rolling_panel(decomp, window = 3)
}
```
