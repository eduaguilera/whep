# Plot Finn Cycling Index evolution and period comparison

Creates three panels: (a) temporal evolution of mean FCI per typology
with interquartile ribbon; (b) FCI distribution at four key periods as
boxplots; (c) net change in mean FCI from first to last period.

## Usage

``` r
plot_finn_circularity(
  periods = c(1860, 1920, 1960, 2010),
  finn_data = NULL,
  n_prov_destiny = NULL,
  typologies = NULL
)
```

## Arguments

- periods:

  Integer vector of years to mark as dashed reference lines on the
  `evolution` panel. The `periods` and `change` panels always compare
  the four fixed historical eras (1860-1870, 1920-1930, 1960-1970,
  2010-2020) regardless of this argument.

- finn_data:

  Pre-computed tibble from
  [`create_finn_indicator()`](https://eduaguilera.github.io/whep/reference/create_finn_indicator.md).
  If `NULL`, computed automatically (slow).

- n_prov_destiny:

  Passed to
  [`create_finn_indicator()`](https://eduaguilera.github.io/whep/reference/create_finn_indicator.md)
  when `finn_data` is `NULL`.

- typologies:

  Typology assignment per year and province, with columns `year`,
  `province_name` and `Typology_base`. If `NULL`, derived from
  `create_typo_ts_plot()`.

## Value

A named list with ggplot objects `evolution`, `periods`, `change`.

## Examples

``` r
# `periods` must name years present in the data.
finn_data <- tibble::tribble(
  ~year, ~province_name, ~finn_index,
  1960, "A", 0.12,
  1960, "B", 0.18,
  2000, "A", 0.07,
  2000, "B", 0.09
)
typologies <- tibble::tribble(
  ~year, ~province_name, ~Typology_base,
  1960, "A", "Specialized cropping systems (intensive)",
  1960, "B", "Semi-natural agroecosystems",
  2000, "A", "Specialized cropping systems (intensive)",
  2000, "B", "Semi-natural agroecosystems"
)
plots <- plot_finn_circularity(
  periods = c(1960, 2000),
  finn_data = finn_data,
  typologies = typologies
)
```
