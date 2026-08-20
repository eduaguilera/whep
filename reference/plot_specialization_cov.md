# Plot the specialization-vs-diversification allocation covariance

Plots the Olley-Pakes allocation covariance from
[`decompose_specialization_cov()`](https://eduaguilera.github.io/whep/reference/decompose_specialization_cov.md)
as a line chart, one line per dimension (cropland province, cropland
destiny, livestock species), with a zero reference line. Positive and
rising values indicate genuine specialization (allocation concentrating
into high-surplus units); values near zero or falling indicate
diversification. This is meant as a supplementary diagnostic reported
alongside (not inside) the main additive decomposition, per the
decomposition proposal.

## Usage

``` r
plot_specialization_cov(covariance = NULL)
```

## Arguments

- covariance:

  A named list from
  [`decompose_specialization_cov()`](https://eduaguilera.github.io/whep/reference/decompose_specialization_cov.md).
  If `NULL`, computed automatically (slow).

## Value

A ggplot object.

## Examples

``` r
covariance <- list(
  cropland_province = tibble::tribble(
    ~year, ~covariance,
    1960, -0.02,
    1980, 0.05,
    2000, 0.11
  ),
  livestock_species = tibble::tribble(
    ~year, ~covariance,
    1960, 0.01,
    1980, 0.08,
    2000, 0.17
  )
)
p <- plot_specialization_cov(covariance)
```
