# Balance input-output flows so the footprint conserves.

RAS-balance an inter-industry flow matrix toward the product-balance
margin. The row target is intermediate use, \\u = X -
\text{rowSums}(Y)\\; the column target keeps the original column
composition, rescaled so its total matches \\\sum u\\.

When the balanced system is productive (the spectral radius of \\A\\ is
below 1), \\(I - A) X = Y\\ holds and the footprint conserves – the case
for the productive systems in the examples and tests.

Caveat for physical agriculture: livestock feed conversion makes some
columns of \\A\\ sum well above 1 (many tonnes of feed per tonne of
product), so the balanced system is not productive. Balancing to
faithful margins does not remove that, and the traced footprint then
over-traces rather than conserving (on the real 2010 model, by about
15%). RAS therefore does not, on its own, fix the grassland
under-tracing; always verify with
[`check_footprint_conservation()`](https://eduaguilera.github.io/whep/reference/check_footprint_conservation.md).

Pass the result as `z_mat` to
[`compute_footprint()`](https://eduaguilera.github.io/whep/reference/compute_footprint.md)
with a large `max_column_sum` (e.g. `1e12`) and
`conserve_extensions = FALSE`.

## Usage

``` r
balance_io_flows(z_mat, x_vec, y_mat, max_iter = 1000L, tol = 1e-06)
```

## Arguments

- z_mat:

  Inter-industry flow matrix (dense or `Matrix`).

- x_vec:

  Total output per sector.

- y_mat:

  Final demand vector, or matrix whose row sums give total final demand
  per product.

- max_iter, tol:

  Passed to
  [`balance_ras()`](https://eduaguilera.github.io/whep/reference/balance_ras.md).

## Value

The balanced flow matrix.

## Examples

``` r
z_mat <- matrix(c(1, 4, 2, 3), nrow = 2)
x_vec <- c(10, 10)
y_mat <- c(3, 3)
balance_io_flows(z_mat, x_vec, y_mat)
#>         [,1]     [,2]
#> [1,] 2.65857 4.341428
#> [2,] 4.34143 2.658572
```
