# Compute Leontief inverse.

Compute the Leontief inverse matrix from intermediate flows and total
output. The Leontief inverse captures both direct and indirect
requirements across the entire supply chain, enabling footprint tracing.

The technical coefficients matrix is computed as \\A\_{ij} = Z\_{ij} /
X_j\\, representing the input of sector \\i\\ needed per unit of output
from sector \\j\\. Column sums of A are capped below 1 using
`value_added_floor` (FABIO convention plus an explicit leakage floor) to
ensure \\(I - A)\\ is invertible even when supply-use data are
inconsistent. The Leontief inverse is then \\L = (I - A)^{-1}\\.

For large systems (thousands of sectors) this function is not usable:
the dense L matrix requires \\n^2 \times 8\\ bytes of memory (e.g. ~4.8
GiB for n = 25 000). Use
[`compute_footprint()`](https://eduaguilera.github.io/whep/reference/compute_footprint.md)
directly with `z_mat` and `x_vec` instead, which solves \\(I - A) x =
Y\\ without ever materialising L.

Accepts both dense and sparse (Matrix package) inputs.

## Usage

``` r
compute_leontief_inverse(z_mat, x_vec, max_n = 5000, value_added_floor = 0.001)
```

## Arguments

- z_mat:

  Square numeric matrix of inter-industry flows. Entry \\Z\_{ij}\\ is
  the flow from sector \\i\\ to sector \\j\\. Can be dense or sparse.

- x_vec:

  Numeric vector of total output per sector. Must have the same length
  as `nrow(z_mat)`.

- max_n:

  Maximum system size before aborting. Defaults to 5000. Set higher at
  your own risk of memory exhaustion.

- value_added_floor:

  Minimum share of each sector's output that is treated as
  non-intermediate leakage when constructing A. Column sums larger than
  `1 - value_added_floor` are rescaled to that maximum. Use `0` to
  recover the previous cap-at-one behavior, though this can leave
  singular systems.

## Value

The Leontief inverse matrix \\L\\. Negative values are set to zero.
Returns a dense matrix.

## Examples

``` r
z_mat <- matrix(c(0, 5, 10, 0), nrow = 2)
x_vec <- c(100, 200)
compute_leontief_inverse(z_mat, x_vec)
#> Computing Leontief inverse (2x2 matrix)...
#> Inverting (I - A)...
#> ✔ Leontief inverse computed.
#>            [,1]       [,2]
#> [1,] 1.00250627 0.05012531
#> [2,] 0.05012531 1.00250627
```
