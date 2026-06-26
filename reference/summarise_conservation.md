# Summarise a footprint conservation report.

Roll up the per-origin report from
[`check_footprint_conservation()`](https://eduaguilera.github.io/whep/reference/check_footprint_conservation.md)
into a single global verdict.

## Usage

``` r
summarise_conservation(report)
```

## Arguments

- report:

  Tibble returned by
  [`check_footprint_conservation()`](https://eduaguilera.github.io/whep/reference/check_footprint_conservation.md).

## Value

A one-row tibble with `n_origin`, `n_ok`, `n_flagged`, `n_dropped`,
`n_under_traced`, `n_over_traced`, `total_direct`, `total_embodied` and
`global_rel_discrepancy`.

## Examples

``` r
z_mat <- matrix(c(0, 5, 10, 0), nrow = 2)
x_vec <- c(100, 200)
y_mat <- matrix(c(85, 195), ncol = 1)
extensions <- c(50, 30)
labels <- tibble::tibble(
  area_code = c(1L, 1L),
  item_cbs_code = c(1L, 2L)
)
compute_footprint(
  x_vec = x_vec, y_mat = y_mat, extensions = extensions,
  labels = labels, z_mat = z_mat
) |>
  check_footprint_conservation(extensions, labels, x_vec) |>
  summarise_conservation()
#> ℹ Computing footprint for 2 sectors.
#>   2 sectors have non-zero extensions.
#>   Final demand: 1 column.
#> Sparse solve path (no dense Leontief inverse).
#> Computing footprints...
#> ✔ Footprint complete: 2 non-zero flows.
#> # A tibble: 1 × 9
#>   n_origin  n_ok n_flagged n_dropped n_under_traced n_over_traced total_direct
#>      <int> <int>     <int>     <int>          <int>         <int>        <dbl>
#> 1        2     1         1         0              1             0           80
#> # ℹ 2 more variables: total_embodied <dbl>, global_rel_discrepancy <dbl>
```
