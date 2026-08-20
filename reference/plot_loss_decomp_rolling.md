# Plot rolling-mean year-on-year drivers of the change in territorial N losses

Same data as
[`plot_loss_decomp_yearly()`](https://eduaguilera.github.io/whep/reference/plot_loss_decomp_yearly.md),
but smooths each year's own additive contribution with a centered
rolling mean (`window` years wide, `NA`-padded at the edges) before
plotting, to make sustained multi-year trends (e.g. a period of
continuously improving efficiency) easier to see than in the raw, noisy
year-on-year series.

## Usage

``` r
plot_loss_decomp_rolling(decomp = NULL, window = 10)
```

## Arguments

- decomp:

  A named list from
  [`decompose_terr_losses()`](https://eduaguilera.github.io/whep/reference/decompose_terr_losses.md).
  If `NULL`, computed automatically (slow).

- window:

  Width of the centered rolling-mean window, in years. Default `10`.

## Value

A named list with ggplot objects `by_compartment` and `by_mechanism`.

## Examples

``` r
# `window` must not exceed the number of years available per group.
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
plots <- plot_loss_decomp_rolling(decomp, window = 3)
```
