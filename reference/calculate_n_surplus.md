# Calculate the gridded soil-surface nitrogen surplus.

Derives the nitrogen surplus from a
[`build_nitrogen_balance()`](https://eduaguilera.github.io/whep/reference/build_nitrogen_balance.md)
output. The default `"harvest_removal"` method is the standard
soil-surface nitrogen balance: net inputs (`n_input_std_t`) minus the
nitrogen exported in harvest (crop product, used or exported residue and
grazed forage), matching the basis of the Schulte-Uebbing et al. (2022)
critical nitrogen surplus. Field-burnt residue nitrogen is not
subtracted (it volatilises in place, a loss inside the surplus, not an
export), nor is recycled residue (returned to the field, internal). The
`"full_balance"` method instead returns the balance's post-loss
`n_balance_t`, a sensitivity alternative. The surplus may be negative (a
nitrogen deficit) and is not clamped here, as clamping is a boundary or
leaching concern. The full grid key (`lon`, `lat`, `area_code`,
`item_cbs_code`, `year`) and every balance column are preserved.

## Usage

``` r
calculate_n_surplus(
  balance,
  method = c("harvest_removal", "full_balance"),
  example = FALSE
)
```

## Arguments

- balance:

  A
  [`build_nitrogen_balance()`](https://eduaguilera.github.io/whep/reference/build_nitrogen_balance.md)
  output tibble. For `"harvest_removal"` it must carry `n_input_std_t`,
  `prod_n_t`, `used_residue_n_t` and `grazed_weeds_n_t`; for
  `"full_balance"` it must carry `n_balance_t`. When an `area_ha` column
  (each crop's harvested hectares in the cell) is present, the
  per-hectare surplus `surplus_kgn_ha` is also emitted.

- method:

  Surplus definition: `"harvest_removal"` (default,
  `n_input_std_t - (prod_n_t + used_residue_n_t + grazed_weeds_n_t)`) or
  `"full_balance"` (the balance's `n_balance_t`, a sensitivity).

- example:

  If `TRUE`, return a small fixture instead of computing from `balance`.
  Defaults to `FALSE`.

## Value

The `balance` tibble with `surplus_n_t` (tonnes N, may be negative),
`method_surplus` and, when `area_ha` is present, `surplus_kgn_ha` (kg N
per hectare).

## Examples

``` r
calculate_n_surplus(example = TRUE)
#> # A tibble: 8 × 16
#>     lon   lat area_code item_cbs_code  year area_ha n_input_std_t prod_n_t
#>   <dbl> <dbl>     <int>         <int> <int>   <dbl>         <dbl>    <dbl>
#> 1  0.25  0.25         1          2511  2010     100            50       20
#> 2  0.25  0.25         1          2513  2010      50            10        8
#> 3  0.25  0.25         1          2555  2010      40             4        6
#> 4  0.75  0.25         1          2511  2010     200           120       40
#> 5  0.75  0.25         1          2513  2010      80            30       12
#> 6  0.25  0.25         1          2511  2011     100            60       25
#> 7  0.75  0.25         1          3000  2010     300            15        5
#> 8  0.25  0.25         1          2555  2011      20             8        3
#> # ℹ 8 more variables: used_residue_n_t <dbl>, grazed_weeds_n_t <dbl>,
#> #   burnt_residue_n_t <dbl>, n_balance_t <dbl>, surplus_n_t <dbl>,
#> #   method_surplus <chr>, production_n_t <dbl>, surplus_kgn_ha <dbl>
```
