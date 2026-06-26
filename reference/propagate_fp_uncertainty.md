# Propagate input uncertainty through a footprint.

Monte Carlo propagation of extension uncertainty: perturb the extension
vector with multiplicative lognormal noise (so values stay non-negative
and the expected factor is one), re-run the footprint for each draw, and
summarise the spread of each output cell. A point estimate with no
interval is not a trustworthy result; this turns one into a
distribution.

## Usage

``` r
propagate_fp_uncertainty(run_fn, extensions, cov = 0.1, options = list())
```

## Arguments

- run_fn:

  Function taking a perturbed extension vector and returning a footprint
  tibble with the grouping columns named in `options$by` plus a `value`
  column. Wrap
  [`compute_footprint()`](https://eduaguilera.github.io/whep/reference/compute_footprint.md)
  with the other arguments fixed.

- extensions:

  Numeric vector of base extensions per sector.

- cov:

  Coefficient of variation of the extensions: one number, or one per
  sector. Zero means no uncertainty.

- options:

  Named list overriding `n` (draws, default 200), `probs`
  (lower/median/upper quantiles), `by` (grouping columns) and `seed`
  (for reproducible draws).

## Value

A tibble with the `by` columns plus `mean`, `sd`, `cv`, `q_low`, `q_med`
and `q_high` per output cell.

## Examples

``` r
run_fn <- function(ext) {
  tibble::tibble(
    target_area = 1L, target_item = 10L, value = sum(ext)
  )
}
propagate_fp_uncertainty(
  run_fn,
  extensions = c(60, 40),
  cov = 0.1,
  options = list(n = 100, seed = 1, by = c("target_area", "target_item"))
)
#> # A tibble: 1 × 8
#>   target_area target_item  mean    sd     cv q_low q_med q_high
#>         <int>       <int> <dbl> <dbl>  <dbl> <dbl> <dbl>  <dbl>
#> 1           1          10  100.  6.76 0.0673  88.3  99.8   115.
```
