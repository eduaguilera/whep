# Decompose a weighted aggregate ratio.

Separates a change in an aggregate numerator-to-denominator ratio into a
between-group composition effect and a within-group ratio effect. The
default symmetric Kitagawa allocation is equivalent to a two-factor
Shapley value for the weight and within-group-ratio factor blocks.

## Usage

``` r
decompose_weighted_ratio(
  data,
  time,
  .by,
  ratio,
  method = c("kitagawa", "lmdi", "weights_first", "ratios_first", "all")
)
```

## Arguments

- data:

  A tibble with exactly two periods and one row per group-period.

- time:

  An unquoted numeric, date-time, or ordered-factor column identifying
  the two ordered periods.

- .by:

  An unquoted column identifying persistent groups.

- ratio:

  An unquoted expression of the exact form `numerator / denominator`,
  using two bare numeric column names.

- method:

  Decomposition method. One of `"kitagawa"`, `"lmdi"`,
  `"weights_first"`, `"ratios_first"`, or `"all"`.

## Value

A tibble with one `component_type = "group"` row per group and one
`component_type = "summary"` row for each requested method. Group rows
contain endpoint stocks, weights, ratios, signed contributions, and
group closure. Summary rows contain aggregate endpoint ratios, signed
effects, percentage contributions, closure diagnostics, and cancellation
metrics. Additive signed contributions have the same units as `ratio`.

## Details

For group `g` and endpoint `t`, the aggregate ratio is
`R_t = sum_g(w_gt * r_gt)`, where `w_gt` is the group's share of the
total denominator and `r_gt` is its numerator-to-denominator ratio.

Write `dw_g = w_g1 - w_g0` and `dr_g = r_g1 - r_g0`. The symmetric
Kitagawa contributions are \$\$B_g = dw_g (r_g0 + r_g1) / 2,\$\$ \$\$W_g
= (w_g0 + w_g1) dr_g / 2.\$\$ The weights-first polar contributions are
`B_g = dw_g r_g0` and `W_g = w_g1 dr_g`; the ratios-first polar
contributions are `B_g = dw_g r_g1` and `W_g = w_g0 dr_g`. Kitagawa is
their arithmetic mean. Its Shapley interpretation concerns the complete
weight and ratio factor blocks; groups are not treated as Shapley
players.

`"lmdi"` implements additive LMDI-I. With `y_gt = w_gt r_gt` and the
logarithmic mean `L(a, b) = (a - b) / (log(a) - log(b))`, using
`L(a, a) = a`, its contributions are \$\$B_g = L(y_g1, y_g0) log(w_g1 /
w_g0),\$\$ \$\$W_g = L(y_g1, y_g0) log(r_g1 / r_g0).\$\$ Every method
satisfies `B_g + W_g = y_g1 - y_g0` and therefore
`sum_g(B_g + W_g) = R_1 - R_0`, up to floating-point error. `"all"`
returns every method on their common strictly positive domain.

The function requires exactly two periods and identical, unique group
support. Denominators must be positive. Numerators may be zero for the
arithmetic methods but must be positive for LMDI. Groups are never
silently dropped and zeros are never replaced by an epsilon.

Reversing endpoints negates Kitagawa and LMDI-I contributions. Reversing
a polar path negates the opposite forward polar path. Percentage
contributions are signed and are not clipped to zero or 100. They are
missing when `abs(R_1 - R_0)` is no larger than
`sqrt(.Machine$double.eps) * max(abs(R_0), abs(R_1))`. High
`cancellation_index` values indicate cancellation between the aggregate
between and within effects. It is defined as
`1 - abs(total_change) / (abs(between) + abs(within))`, with zero used
when every effect is zero. Results are descriptive accounting
identities, not causal attribution, and depend on the chosen group
resolution.

## References

Kitagawa, E. M. (1955). Components of a Difference Between Two Rates.
*Journal of the American Statistical Association*, 50(272), 1168-1194.
[doi:10.1080/01621459.1955.10501299](https://doi.org/10.1080/01621459.1955.10501299)
.

Ang, B. W. (2005). The LMDI approach to decomposition analysis: a
practical guide. *Energy Policy*, 33(7), 867-871.
[doi:10.1016/j.enpol.2003.10.010](https://doi.org/10.1016/j.enpol.2003.10.010)
.

## Examples

``` r
ratio_data <- tibble::tribble(
  ~year, ~group, ~numerator, ~denominator,
  2000, "a", 400, 40,
  2000, "b", 1200, 60,
  2020, "a", 600, 50,
  2020, "b", 900, 50
)

decompose_weighted_ratio(
  ratio_data,
  year,
  group,
  numerator / denominator
)
#> # A tibble: 3 × 36
#>   method   component_type group period_start period_end ratio_expression       
#>   <chr>    <chr>          <chr>        <dbl>      <dbl> <chr>                  
#> 1 kitagawa group          a             2000       2020 numerator / denominator
#> 2 kitagawa group          b             2000       2020 numerator / denominator
#> 3 kitagawa summary        NA            2000       2020 numerator / denominator
#> # ℹ 30 more variables: numerator_start <dbl>, numerator_end <dbl>,
#> #   denominator_start <dbl>, denominator_end <dbl>, weight_start <dbl>,
#> #   weight_end <dbl>, within_ratio_start <dbl>, within_ratio_end <dbl>,
#> #   ratio_contribution_start <dbl>, ratio_contribution_end <dbl>,
#> #   group_change <dbl>, between_contribution <dbl>, within_contribution <dbl>,
#> #   group_closure_residual <dbl>, global_ratio_start <dbl>,
#> #   global_ratio_end <dbl>, total_change <dbl>, between_share_pct <dbl>, …
```
