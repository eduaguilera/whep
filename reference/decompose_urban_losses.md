# Decompose urban nitrogen losses into population, per-capita, and recycling drivers

Decomposes the year-on-year change in non-recycled human excreta
nitrogen (N) into three multiplicative drivers, computed at the national
level: population, per-capita excreted N (approximated by per-capita
food N consumption, since intake is approximately equal to excretion),
and the non-recycled fraction of excreted N.

Only the `population_food` destiny is used as the excretion proxy.
`population_other_uses` (non-food industrial use, e.g. cotton/tobacco)
is deliberately excluded even though `.create_wastewater_surplus_df()`
in `grafs_plot_df.R` includes it: that material is not ingested, so it
cannot be assumed to leave the body as excreta, breaking the
intake-approximates-excretion logic this compartment relies on.

Food waste is not included: the commodity-balance sheets underlying
`n_prov_destiny` carry Food/Feed/Seed/Other-uses/Export/Import destinies
but no separate consumer food-waste line, so this loss is excreta-only.
Whether the `urban`/`People` recycling flows already folded in some food
waste from the original source is unconfirmed.

## Usage

``` r
decompose_urban_losses(
  n_prov_destiny = NULL,
  population_yg = NULL,
  by_period = FALSE,
  example = FALSE
)
```

## Arguments

- n_prov_destiny:

  Nitrogen flows tibble from
  [`create_n_prov_destiny()`](https://eduaguilera.github.io/whep/reference/create_n_prov_destiny.md).
  If `NULL`, loaded automatically.

- population_yg:

  Population tibble from `whep_read_file("population_yg")`. If `NULL`,
  loaded automatically.

- by_period:

  If `TRUE`, compares each reference period (each averaged across its
  ten years) against the immediately preceding one — 1860-1870 -\>
  1920-1930 -\> 1960-1970 -\> 2010-2020 — plus one extra transition
  spanning the full analysis window, 1860-1870 straight to 2010-2020
  (the total change) — instead of chaining year on year.

- example:

  If `TRUE`, return a small hardcoded output without downloading remote
  data. Default is `FALSE`.

## Value

A tibble from
[`calculate_lmdi()`](https://eduaguilera.github.io/whep/reference/calculate_lmdi.md)
with columns `period`, `period_years`, `factor_label`, `component_type`,
`additive`, `multiplicative`, and `multiplicative_log`.

## Examples

``` r
decompose_urban_losses(example = TRUE)
#> # A tibble: 8 × 7
#>   period    period_years factor_label     component_type additive multiplicative
#>   <chr>            <dbl> <chr>            <chr>             <dbl>          <dbl>
#> 1 1860-1861            1 Population       factor            393.           1.00 
#> 2 1860-1861            1 Per-capita excr… factor           -388.           0.996
#> 3 1860-1861            1 Non-recycled fr… factor            -17.5          1.000
#> 4 1860-1861            1 Urban N loss     target            -12.6          1.000
#> 5 1861-1862            1 Population       factor            391.           1.00 
#> 6 1861-1862            1 Per-capita excr… factor           -387.           0.996
#> 7 1861-1862            1 Non-recycled fr… factor            -17.5          1.000
#> 8 1861-1862            1 Urban N loss     target            -12.6          1.000
#> # ℹ 1 more variable: multiplicative_log <dbl>
```
