# Decompose livestock manure management losses into herd, feed, excretion, and management-loss drivers

Decomposes the year-on-year change in nitrogen (N) lost from livestock
housing and manure storage (before any manure reaches land) into four
multiplicative drivers, computed at the national level: herd size, feed
N intake per livestock unit, excreted fraction of feed N (1 - feed
nitrogen use efficiency), and the management-loss fraction of excreted
N.

This is a simplified, national-only view with no species breakdown (no
species-mix factor);
[`decompose_specialization_cov()`](https://eduaguilera.github.io/whep/reference/decompose_specialization_cov.md)
still uses the full per-species detail for its `livestock_species`
series. Only livestock categories with a livestock-unit (LU) coefficient
in `livestock_units` are included in the underlying herd/feed/excretion
totals (currently Cattle_milk, Cattle_meat, Sheep, Goats, Horses,
Donkeys_mules, Pigs, Poultry, Rabbits); categories present in
`intake_ygiac`/`n_excretion_ygs` but absent from `livestock_units` (e.g.
"Fur animals", "Other", "Other_birds") are dropped.

`n_prov_destiny` records manure already applied to land without
retaining which species it came from, so the management-loss fraction
has always been computed nationally, not per species.

## Usage

``` r
decompose_manure_losses(
  n_prov_destiny = NULL,
  intake_ygiac = NULL,
  n_excretion_ygs = NULL,
  stock_prod_ygps = NULL,
  livestock_units = NULL,
  by_period = FALSE,
  example = FALSE
)
```

## Arguments

- n_prov_destiny:

  Nitrogen flows tibble from
  [`create_n_prov_destiny()`](https://eduaguilera.github.io/whep/reference/create_n_prov_destiny.md).
  If `NULL`, loaded automatically.

- intake_ygiac:

  Feed intake tibble from `whep_read_file("intake_ygiac")`. If `NULL`,
  loaded automatically.

- n_excretion_ygs:

  Livestock excretion tibble from `whep_read_file("n_excretion_ygs")`.
  If `NULL`, loaded automatically.

- stock_prod_ygps:

  Livestock stock tibble from `whep_read_file("stock_prod_ygps")`. If
  `NULL`, loaded automatically.

- livestock_units:

  Livestock unit coefficients tibble from
  `whep_read_file("livestock_units")`. If `NULL`, loaded automatically.

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
decompose_manure_losses(example = TRUE)
#> # A tibble: 10 × 7
#>    period    period_years factor_label    component_type additive multiplicative
#>    <chr>            <dbl> <chr>           <chr>             <dbl>          <dbl>
#>  1 1860-1861            1 Herd size       factor             0             1    
#>  2 1860-1861            1 Feed intensity  factor             1.78          1    
#>  3 1860-1861            1 Excreted feed … factor            16.7           1.00 
#>  4 1860-1861            1 Management los… factor            56.3           1.00 
#>  5 1860-1861            1 Manure managem… target            74.8           1.00 
#>  6 1861-1862            1 Herd size       factor             0             1    
#>  7 1861-1862            1 Feed intensity  factor            80.1           1.00 
#>  8 1861-1862            1 Excreted feed … factor           -25.0           1.000
#>  9 1861-1862            1 Management los… factor          -126.            0.998
#> 10 1861-1862            1 Manure managem… target           -71.2           0.999
#> # ℹ 1 more variable: multiplicative_log <dbl>
```
