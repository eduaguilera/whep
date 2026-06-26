# Combine independent coefficient-of-variation components.

Aggregate several independent sources of relative uncertainty into a
single coefficient of variation by adding them in quadrature. This is
how per-indicator data-quality scores (for example the reliability,
completeness and representativeness columns of a pedigree matrix) are
turned into one CoV for
[`propagate_fp_uncertainty()`](https://eduaguilera.github.io/whep/reference/propagate_fp_uncertainty.md).
Supply the per-indicator CoV values yourself from a sourced, citable
pedigree table; no uncertainty factors are hard-coded here.

## Usage

``` r
combine_cov(...)
```

## Arguments

- ...:

  Numeric vectors of equal length, one per uncertainty source, or a
  single matrix/data frame with one column per source.

## Value

A numeric vector of combined coefficients of variation.

## Examples

``` r
combine_cov(c(0.3, 0.1), c(0.4, 0.2))
#> [1] 0.5000000 0.2236068
```
