# Local sensitivity of a footprint to each extension.

One-at-a-time sensitivity analysis: nudge each sector's extension by a
small relative step and measure the elasticity of the total footprint.
High-elasticity sectors are where data quality matters most and where a
result is most fragile.

## Usage

``` r
footprint_sensitivity(run_fn, extensions, options = list())
```

## Arguments

- run_fn:

  Function taking an extension vector and returning a footprint tibble
  with a `value` column.

- extensions:

  Numeric vector of base extensions per sector.

- options:

  Named list overriding `delta` (relative step, default 0.05) and
  `which` (sector indices to test; default all non-zero extensions).

## Value

A tibble with `sector` (index into `extensions`) and `elasticity`,
ordered by descending absolute elasticity.

## Examples

``` r
run_fn <- function(ext) tibble::tibble(value = sum(ext))
footprint_sensitivity(run_fn, extensions = c(60, 40))
#> # A tibble: 2 × 2
#>   sector elasticity
#>    <int>      <dbl>
#> 1      1        0.6
#> 2      2        0.4
```
