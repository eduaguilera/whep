# Summarise biological nitrogen fixation results.

Aggregates the per-row BNF components into group totals, shares and mean
modifiers.

## Usage

``` r
summarize_bnf(x, group_by = "item_prod_code")
```

## Arguments

- x:

  A tibble with `crop_bnf_t`, `weed_bnf_t`, `nonsymbiotic_bnf_t` and
  `bnf_t` (the output of
  [`calculate_bnf()`](https://eduaguilera.github.io/whep/reference/calculate_bnf.md)).

- group_by:

  Character vector of grouping columns (default `"item_prod_code"`); use
  `NULL` for an overall summary.

## Value

A tibble with per-group counts, BNF totals, component percentages and
mean environmental factors.

## Examples

``` r
tibble::tibble(
  item_prod_code = "176", crop_npp_n_t = 10, product_n_t = 5,
  weed_npp_n_t = 4, land_use = "Cropland", legumes_seeded = 0,
  seeded_cover_crop_share = 0, area_ha = 40
) |>
  calculate_bnf() |>
  summarize_bnf()
#> # A tibble: 1 × 13
#>   item_prod_code     n legume_category total_crop_bnf_t total_weed_bnf_t
#>   <chr>          <int> <chr>                      <dbl>            <dbl>
#> 1 176                1 Grain                          4            0.136
#> # ℹ 8 more variables: total_nonsymbiotic_bnf_t <dbl>, total_bnf_t <dbl>,
#> #   mean_ndfa_adj <dbl>, mean_f_env_symbiotic <dbl>,
#> #   mean_f_env_nonsymbiotic <dbl>, pct_crop_bnf <dbl>, pct_weed_bnf <dbl>,
#> #   pct_nonsymbiotic_bnf <dbl>
```
