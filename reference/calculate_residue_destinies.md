# Estimate the destinies of crop residues.

Splits crop residue dry matter into three destinies that sum to the
total residue: fed to livestock, burned / removed for fuel, and left on
the field for soil incorporation.

## Usage

``` r
calculate_residue_destinies(x, method = c("krausmann_regional", "shares"))
```

## Arguments

- x:

  A tibble with `item_prod_code` and `residue_dm_t`. The
  `krausmann_regional` method also needs `region_krausmann` (for the
  recovery rate) and `region_hanpp` (for the feed-use fraction).
  `region_krausmann` can use the recovery-table labels or the matching
  `regions_full` labels. The `shares` method needs `year`.

- method:

  Destiny method: `"krausmann_regional"` (default, Krausmann recovery x
  HANPP-regional feed-use fraction) or `"shares"` (the Spain-specific
  per-crop-year use/burn shares, flagged `to_be_revised`).

## Value

The input tibble with `residue_feed_dm_t`, `residue_burn_dm_t`,
`residue_soil_dm_t` and `method_residue_destiny`.

## Examples

``` r
calculate_residue_destinies(
  tibble::tibble(
    item_prod_code = "15", residue_dm_t = 100,
    region_krausmann = "Western Europe", region_hanpp = "Western Europe"
  )
)
#> # A tibble: 1 × 8
#>   item_prod_code residue_dm_t region_krausmann region_hanpp   residue_feed_dm_t
#>   <chr>                 <dbl> <chr>            <chr>                      <dbl>
#> 1 15                      100 West Europe      Western Europe              10.5
#> # ℹ 3 more variables: residue_burn_dm_t <dbl>, residue_soil_dm_t <dbl>,
#> #   method_residue_destiny <chr>
```
