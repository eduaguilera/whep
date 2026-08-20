# Plot period-based drivers of the change in territorial N losses

Plots two stacked bar charts from
[`decompose_terr_losses_periods()`](https://eduaguilera.github.io/whep/reference/decompose_terr_losses_periods.md):
one bar per reference period comparison, labeled by mean year
(1865-1925, 1925-1965, 1965-2015), each compared against the immediately
preceding reference period (chained), plus one extra bar for the full
analysis window (Total (1865-2015)), broken down by compartment in one
chart and by mechanism in the other. Contributions are normalized to Gg
N/yr (see
[`decompose_terr_losses_periods()`](https://eduaguilera.github.io/whep/reference/decompose_terr_losses_periods.md)),
since the chained transitions and the Total span very different numbers
of years.

## Usage

``` r
plot_loss_decomp_periods(decomp = NULL)
```

## Arguments

- decomp:

  A named list from
  [`decompose_terr_losses_periods()`](https://eduaguilera.github.io/whep/reference/decompose_terr_losses_periods.md).
  If `NULL`, computed automatically (slow).

## Value

A named list with ggplot objects `by_compartment` and `by_mechanism`.

## Examples

``` r
decomp <- list(
  by_compartment = tibble::tribble(
    ~period, ~compartment, ~contribution_per_yr_mgn,
    "1865-1925", "cropland", 120,
    "1865-1925", "manure", 40,
    "1925-1965", "cropland", 260,
    "1925-1965", "manure", 90
  ),
  by_mechanism = tibble::tribble(
    ~period, ~mechanism, ~contribution_per_yr_mgn,
    "1865-1925", "Size", 90,
    "1865-1925", "Inefficiency", 70,
    "1925-1965", "Size", 150,
    "1925-1965", "Inefficiency", 200
  )
)
plots <- plot_loss_decomp_periods(decomp)
```
