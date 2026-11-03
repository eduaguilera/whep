# Check the commodity balance sheet supply-use identity.

Verify that total supply equals total use for every row of a wide
commodity balance sheet, the fundamental accounting identity behind the
input-output model. Supply is `production + import + stock_withdrawal`;
physical use is
`export + food + feed + seed + processing + processing_primary + other_uses + stock_addition + losses + tourist_consumption`.
`processing_primary` is treated as `0` when the column is absent, since
it is only nonzero for the handful of primary items whose entire
domestic supply is destined for processing (see
[`get_wide_cbs()`](https://eduaguilera.github.io/whep/reference/get_wide_cbs.md)).

Two identities are reported, because they answer different questions.
The physical one (`balanced`) asks whether the physical flows close; it
is `NA` where the stock change is unobserved. The accounting one
(`accounting_balanced`) adds FAOSTAT's statistical `residuals` and
WHEP's `balance_discrepancy`, and counts an unobserved stock change as
none, so it holds by construction on a
[`build_commodity_balances()`](https://eduaguilera.github.io/whep/reference/build_commodity_balances.md)
output: it proves the ledger is complete, not that the physical account
is right. `losses`, `tourist_consumption`, `residuals` and
`balance_discrepancy` are taken as `0` when the column is absent, as in
a balance built before those elements existed.

## Usage

``` r
check_supply_use_balance(cbs, tol = 1e-06)
```

## Arguments

- cbs:

  Wide commodity balance sheet, e.g. from
  [`get_wide_cbs()`](https://eduaguilera.github.io/whep/reference/get_wide_cbs.md).

- tol:

  Absolute tolerance (in the data's mass units) for a row to count as
  balanced.

## Value

A tibble with the key columns present in `cbs` plus:

- `supply`: Total supply.

- `use`: Total physical use.

- `abs_diff`: `abs(supply - use)`.

- `rel_diff`: `abs_diff / supply` (`NA` when supply is zero).

- `balanced`: `TRUE` when `abs_diff <= tol`.

- `residuals`, `balance_discrepancy`: the statistical terms.

- `accounting_diff`: supply minus use, residuals and discrepancy.

- `accounting_balanced`: `TRUE` when `abs(accounting_diff) <= tol`. Rows
  are ordered by descending absolute difference.

## Examples

``` r
get_wide_cbs(example = TRUE) |>
  check_supply_use_balance()
#> # A tibble: 10 × 14
#>     year reporting_polity_code area_code item_cbs_code unit       supply     use
#>    <int> <chr>                     <int>         <dbl> <chr>       <dbl>   <dbl>
#>  1  1987 COD-1960-2025               250          2106 tonnes 13741247    1.37e7
#>  2  2012 CHN-1950-2025                41          2633 tonnes   146000    1.50e5
#>  3  1984 LBR-1847-2025               123          2595 tonnes     3854    5.36e3
#>  4  1995 CUB-1800-2025                49          2734 tonnes    71524    7.11e4
#>  5  1961 VEN-1830-2025               236          2620 tonnes    11200    1.12e4
#>  6  1961 NZL-1840-2025               156          2658 tonnes     6880    6.88e3
#>  7  1977 NGA-1961-2025               159          2658 tonnes     2220    2.22e3
#>  8  1982 PAK-1971-2025               165          2633 tonnes       87.5  8.75e1
#>  9  1995 URY-1828-2025               234          2671 tonnes     4490    4.49e3
#> 10  1975 AUS-1901-2025                10           677 tonnes     2270    2.27e3
#> # ℹ 7 more variables: abs_diff <dbl>, rel_diff <dbl>, balanced <lgl>,
#> #   residuals <dbl>, balance_discrepancy <dbl>, accounting_diff <dbl>,
#> #   accounting_balanced <lgl>
```
