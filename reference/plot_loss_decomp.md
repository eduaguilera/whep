# Plot cumulative drivers of the change in territorial N losses

Plots two stacked-area charts of the cumulative, year-on-year
contribution to the change in Spain's total territorial nitrogen (N)
losses since the start of the reconstruction: one broken down by
compartment (cropland, semi-natural, manure, urban), one regrouped by
transformation mechanism (scale, specialization, intensification,
efficiency), as computed by
[`decompose_terr_losses()`](https://eduaguilera.github.io/whep/reference/decompose_terr_losses.md).

## Usage

``` r
plot_loss_decomp(decomp = NULL)
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
    1861, "manure", 4000, 4000,
    1862, "cropland", 8000, 20000,
    1862, "manure", -1000, 3000
  ),
  by_mechanism = tibble::tribble(
    ~t0, ~mechanism, ~contribution_mgn, ~cumulative_mgn,
    1861, "Size", 9000, 9000,
    1861, "Inefficiency", 7000, 7000,
    1862, "Size", 5000, 14000,
    1862, "Inefficiency", 2000, 9000
  )
)
plots <- plot_loss_decomp(decomp)
```
