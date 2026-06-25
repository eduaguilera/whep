# Estimate livestock nitrogen, carbon and volatile-solids excretion.

Converts realised feed intake (the output of
[`redistribute_feed()`](https://eduaguilera.github.io/whep/reference/redistribute_feed.md))
into excreted nitrogen, carbon and volatile solids per
`year x territory x sub_territory x livestock_category`. All excretion
methods share one canonical nitrogen intake,
`n_intake = sum(intake_dm_t * feed_n_content)`, so the methods are
directly comparable.

## Usage

``` r
estimate_n_excretion(intake, options = list())
```

## Arguments

- intake:

  A tibble of realised feed intake with at least `year`, `territory`,
  `sub_territory`, `livestock_category`, `item_cbs_code`, `feed_quality`
  and `intake_dm_t` (the
  [`redistribute_feed()`](https://eduaguilera.github.io/whep/reference/redistribute_feed.md)
  result).

- options:

  A named list of method options:

  - `method`: `"intake_minus_retention"` (default,
    `n_intake * (1 - n_retention_frac)`) or `"intake_minus_product_n"`
    (`n_intake - product_n`).

  - `method_vs`: `"intake_digestibility"` (default,
    `intake_dm_t * (1 - digestibility) * (1 - ash)`).

  - `product_n`: a tibble (`year`, `territory`, `sub_territory`,
    `livestock_category`, `product_n`) required by
    `"intake_minus_product_n"`.

## Value

A tibble with one row per
`year x territory x sub_territory x livestock_category` and columns
`n_intake`, `n_excretion`, `c_excretion`, `vs_excretion`,
`method_n_excretion` and `method_vs`.

## Examples

``` r
intake <- tibble::tribble(
  ~year, ~territory, ~sub_territory, ~livestock_category,
  ~item_cbs_code, ~feed_quality, ~intake_dm_t,
  2020L, "ES", NA, "Cattle_milk", 2513L, "high_quality", 100,
  2020L, "ES", NA, "Cattle_milk", NA, "grass", 500
)
estimate_n_excretion(intake)
#> # A tibble: 1 × 10
#>    year territory sub_territory livestock_category n_intake n_excretion
#>   <int> <chr>     <lgl>         <chr>                 <dbl>       <dbl>
#> 1  2020 ES        NA            Cattle_milk            11.9        9.49
#> # ℹ 4 more variables: c_excretion <dbl>, vs_excretion <dbl>,
#> #   method_n_excretion <chr>, method_vs <chr>
```
