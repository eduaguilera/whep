# Interpolate anchor points at arbitrary output positions.

Vector-level interpolation primitive: anchor positions and values in,
one interpolated value per requested output position out. With
`log_space = TRUE` an output point is filled in log space (constant
compound growth rate) whenever both of its bracketing anchors are finite
and strictly positive; every other point falls back to ordinary linear
interpolation.

This is the same rule, and the same internal implementation, that
[`fill_linear()`](https://eduaguilera.github.io/whep/reference/fill_linear.md)
applies with `log_space = TRUE`.
[`fill_linear()`](https://eduaguilera.github.io/whep/reference/fill_linear.md)
is a data-frame gap filler that needs the target rows to already exist;
`interp_vec()` is for callers that hold plain vectors and need values at
positions with no pre-existing row, or that call the primitive once per
gap segment inside their own guard logic.

## Usage

``` r
interp_vec(x, y, xout, log_space = FALSE, rule = 1)
```

## Arguments

- x:

  Numeric vector of anchor positions, for example years. It does not
  need to be sorted.

- y:

  Numeric vector of anchor values, the same length as `x`.

- xout:

  Numeric vector of positions to interpolate at. It does not need to be
  sorted; results follow the order of `xout`.

- log_space:

  Logical. If `TRUE`, each output point bracketed by two finite and
  strictly positive anchors is interpolated in log space (constant
  compound growth rate); any other point falls back to linear
  interpolation. Default: `FALSE`, i.e. linear interpolation everywhere.

- rule:

  Either `1` or `2`, passed to
  [`stats::approx()`](https://rdrr.io/r/stats/approxfun.html) to handle
  output positions outside the anchor range. `1` (default) returns `NA`
  there, `2` carries the nearest anchor value. Log space never applies
  outside the anchor range.

## Value

A list of two vectors, each as long as `xout`:

- `y`: the interpolated values.

- `method`: `"loglinear"` where the value came from log space,
  `"linear"` where it came from linear interpolation, and
  `NA_character_` where no value could be produced.

## Details

Anchors with a non-finite position or a missing value are dropped, and
anchors sharing a position are averaged, so that the linear and
log-space paths always see the same anchor set. Anchors whose value is
infinite are kept, but a segment bounded by one is never eligible for
log space. If fewer than two usable anchors remain there is nothing to
interpolate between, and every element of `y` and `method` is `NA`.

An output position that coincides with an anchor position returns that
anchor's value unchanged, labelled `"linear"`, because nothing needed
interpolating there. In particular log space never rebuilds an anchor
value it was handed, which would perturb it in the last bits.

## See also

[`fill_linear()`](https://eduaguilera.github.io/whep/reference/fill_linear.md)

## Examples

``` r
# Constant compound growth: the 2005 midpoint of 1 and 1024 is 32, whereas
# linear interpolation returns the arithmetic midpoint 512.5.
interp_vec(c(2000, 2010), c(1, 1024), xout = 2005, log_space = TRUE)
#> $y
#> [1] 32
#> 
#> $method
#> [1] "loglinear"
#> 
interp_vec(c(2000, 2010), c(1, 1024), xout = 2005)
#> $y
#> [1] 512.5
#> 
#> $method
#> [1] "linear"
#> 

# A non-positive anchor makes log space undefined, so the point stays linear.
interp_vec(c(2000, 2010), c(0, 10), xout = 2005, log_space = TRUE)
#> $y
#> [1] 5
#> 
#> $method
#> [1] "linear"
#> 

# Several output positions at once, from an unsorted anchor set. With
# `rule = 2` the position beyond the last anchor carries that anchor value.
interp_vec(
  x = c(2010, 2000, 2005),
  y = c(400, 100, 200),
  xout = c(2002, 2007, 2015),
  log_space = TRUE,
  rule = 2
)
#> $y
#> [1] 131.9508 263.9016 400.0000
#> 
#> $method
#> [1] "loglinear" "loglinear" "linear"   
#> 
```
