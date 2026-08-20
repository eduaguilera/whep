# Decompose semi-natural agroecosystem N surplus into size, intensity, and inefficiency drivers

Decomposes the year-on-year change in Spain's semi-natural agroecosystem
(grazing land, dehesa, and non-cropland vegetation) nitrogen (N) surplus
into three multiplicative drivers, following an additive LMDI
shift-share decomposition computed at the national level:

- **Size**: national semi-natural area.

- **Intensity**: N input per hectare of semi-natural land.

- **Inefficiency**: surplus fraction of inputs (1 - nitrogen use
  efficiency).

No destiny factor is used because grazed and cut vegetation is assumed
to be overwhelmingly a single destiny (livestock feed).

The land-use categories included (`Dehesa`, `Forest_high`, `Forest_low`,
`Other`, `Pasture_Shrubland`) are all of `npp_ygpit`'s non-cropland
categories, matching the existing `semi_natural_agroecosystems` box used
elsewhere in the package. Some of that land (e.g. `Forest_high`/
`Forest_low`) may not actually be grazed and can produce non-feed output
(firewood), which would call for its own destiny factor (as in
[`decompose_cropland_surplus()`](https://eduaguilera.github.io/whep/reference/decompose_cropland_surplus.md))
rather than the single-destiny assumption above; that refinement is not
implemented here.

This is a simplified, national-only view (no provincial breakdown).
Semi-natural surplus can turn negative (soil N mining) in some years.
LMDI relies on logarithms and cannot handle a series that changes sign
between two compared years; this function warns when that occurs instead
of silently returning `NA`, but does not implement the Shapley/Sun
alternative required for those cases.

## Usage

``` r
decompose_semi_natural_surplus(
  n_prov_destiny = NULL,
  npp_ygpit = NULL,
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
decompose_semi_natural_surplus(example = TRUE)
#> # A tibble: 8 × 7
#>   period    period_years factor_label     component_type additive multiplicative
#>   <chr>            <dbl> <chr>            <chr>             <dbl>          <dbl>
#> 1 1860-1861            1 Size             factor            -287.          0.997
#> 2 1860-1861            1 Intensity        factor           -3265.          0.965
#> 3 1860-1861            1 Inefficiency     factor          -10384.          0.892
#> 4 1860-1861            1 Semi-natural N … target          -13936.          0.858
#> 5 1861-1862            1 Size             factor            -401.          0.997
#> 6 1861-1862            1 Intensity        factor           30174.          1.27 
#> 7 1861-1862            1 Inefficiency     factor           69339.          1.72 
#> 8 1861-1862            1 Semi-natural N … target           99112.          2.17 
#> # ℹ 1 more variable: multiplicative_log <dbl>
```
