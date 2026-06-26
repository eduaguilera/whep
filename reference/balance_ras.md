# Balance a matrix to target margins by RAS.

Reconcile a non-negative matrix to prescribed row and column sums using
the RAS (biproportional / iterative proportional fitting) algorithm:
alternately rescale rows then columns until both margins are matched,
preserving the matrix's structure (zero pattern and relative
magnitudes). The row and column targets must have equal totals.

Works on dense and sparse (`Matrix`) inputs, staying in the input
representation so it is efficient for both small dense tables and large
sparse input-output systems. (The same core also balances the bilateral
trade matrices.) For matrices with negative entries the signed GRAS
variant is needed and is not implemented here.

## Usage

``` r
balance_ras(x, target_rows, target_cols, max_iter = 1000L, tol = 1e-09)
```

## Arguments

- x:

  Non-negative matrix (dense or `Matrix`).

- target_rows:

  Desired row sums, length `nrow(x)`.

- target_cols:

  Desired column sums, length `ncol(x)`.

- max_iter:

  Maximum RAS iterations.

- tol:

  Convergence tolerance on the largest margin deviation, relative to the
  margin total (so it is scale-free).

## Value

The balanced matrix, in the same representation as `x` (dense in, dense
out; sparse in, sparse out).

## Examples

``` r
m <- matrix(c(1, 2, 3, 4), nrow = 2)
balance_ras(m, target_rows = c(10, 20), target_cols = c(12, 18))
#>          [,1]      [,2]
#> [1,] 3.363083  6.636917
#> [2,] 8.636917 11.363083
```
