# Decompose cropland N surplus into size, intensity, and inefficiency drivers

Decomposes the year-on-year change in Spain's cropland nitrogen (N)
surplus into three multiplicative drivers, following an additive LMDI
(logarithmic mean Divisia index) shift-share decomposition computed at
the national level:

- **Size**: national cropland area.

- **Intensity**: N input per hectare of cropland.

- **Inefficiency**: surplus fraction of inputs (1 - nitrogen use
  efficiency).

Contributions are additive and residual-free: they sum exactly to the
observed change in cropland N surplus for every year-on-year transition.

This is a simplified, national-only view (no provincial or destiny
breakdown);
[`decompose_specialization_cov()`](https://eduaguilera.github.io/whep/reference/decompose_specialization_cov.md)
still uses the full province x destiny detail for its
`cropland_province` and `cropland_destiny` series.

## Usage

``` r
decompose_cropland_surplus(
  n_prov_destiny = NULL,
  npp_ygpit = NULL,
  codes_coefs = NULL,
  by_period = FALSE,
  example = FALSE
)
```

## Arguments

- n_prov_destiny:

  Nitrogen flows tibble from
  [`create_n_prov_destiny()`](https://eduaguilera.github.io/whep/reference/create_n_prov_destiny.md).
  If `NULL`, loaded automatically.

- npp_ygpit:

  Land use and area tibble from `whep_read_file("npp_ygpit")`. If
  `NULL`, loaded automatically.

- codes_coefs:

  Item and biomass coefficients tibble from
  `whep_read_file("codes_coefs")`. If `NULL`, loaded automatically.

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
decompose_cropland_surplus(example = TRUE)
#> # A tibble: 8 × 7
#>   period    period_years factor_label     component_type additive multiplicative
#>   <chr>            <dbl> <chr>            <chr>             <dbl>          <dbl>
#> 1 1860-1861            1 Size             factor            1004.          1.01 
#> 2 1860-1861            1 Intensity        factor            -171.          0.999
#> 3 1860-1861            1 Inefficiency     factor          -17200.          0.897
#> 4 1860-1861            1 Cropland N surp… target          -16367.          0.902
#> 5 1861-1862            1 Size             factor            1082.          1.01 
#> 6 1861-1862            1 Intensity        factor           13085.          1.08 
#> 7 1861-1862            1 Inefficiency     factor           32979.          1.21 
#> 8 1861-1862            1 Cropland N surp… target           47146           1.31 
#> # ℹ 1 more variable: multiplicative_log <dbl>
```
