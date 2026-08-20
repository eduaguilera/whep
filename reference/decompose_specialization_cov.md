# Decompose specialization from diversification via the Olley-Pakes allocation covariance

[`decompose_cropland_surplus()`](https://eduaguilera.github.io/whep/reference/decompose_cropland_surplus.md),
[`decompose_semi_natural_surplus()`](https://eduaguilera.github.io/whep/reference/decompose_semi_natural_surplus.md),
and
[`decompose_manure_losses()`](https://eduaguilera.github.io/whep/reference/decompose_manure_losses.md)
are all simplified to national-only views with no spatial, destiny, or
species-mix factor, so the LMDI "Specialization" mechanism (in
[`decompose_terr_losses()`](https://eduaguilera.github.io/whep/reference/decompose_terr_losses.md))
is currently empty. This function recovers the provincial and species
allocation signal independently, straight from the underlying panels: it
shows whether the allocation of area or herd across units (provinces,
destinies, species) concentrated into high-surplus units (genuine
specialization) or spread towards low-surplus ones (diversification) — a
distinction the mix alone cannot make. This function adds that signal,
following the Olley-Pakes allocation identity used in the decomposition
proposal (`sum(w_i * s_i) = mean(s) + covariance(w_i, s_i)`): for a set
of units with area/herd share `w_i` and per-unit surplus `s_i`, the
covariance between the two is positive and growing when the allocation
concentrates into high-surplus units (specialization raising surplus),
and shrinks towards zero or turns negative under diversification.

Unlike the additive LMDI contributions (in Mg N), the covariance is
expressed in per-unit-area or per-unit-herd surplus terms (Mg N per ha,
or Mg N per livestock unit) — it is not directly comparable in magnitude
to the "Specialization" mechanism total from
[`decompose_terr_losses()`](https://eduaguilera.github.io/whep/reference/decompose_terr_losses.md),
only in sign and trend.

## Usage

``` r
decompose_specialization_cov(
  n_prov_destiny = NULL,
  raw = NULL,
  example = FALSE
)
```

## Arguments

- n_prov_destiny:

  Nitrogen flows tibble from
  [`create_n_prov_destiny()`](https://eduaguilera.github.io/whep/reference/create_n_prov_destiny.md).
  If `NULL`, loaded automatically.

- raw:

  Named list overriding any of the raw inputs (`npp_ygpit`,
  `codes_coefs`, `intake_ygiac`, `n_excretion_ygs`, `stock_prod_ygps`,
  `livestock_units`). Missing elements are loaded automatically.

- example:

  If `TRUE`, return a small hardcoded output without downloading remote
  data. Default is `FALSE`.

## Value

A named list with tibbles `cropland_province`, `cropland_destiny`, and
`livestock_species`, each with columns `year` and `covariance`.

## Examples

``` r
decompose_specialization_cov(example = TRUE)
#> $cropland_province
#> # A tibble: 3 × 2
#>    year covariance
#>   <dbl>      <dbl>
#> 1  1900    -0.0009
#> 2  1950    -0.0031
#> 3  2000    -0.0152
#> 
#> $cropland_destiny
#> # A tibble: 3 × 2
#>    year covariance
#>   <dbl>      <dbl>
#> 1  1900     0.0026
#> 2  1950     0.0002
#> 3  2000    -0.0178
#> 
#> $livestock_species
#> # A tibble: 3 × 2
#>    year covariance
#>   <dbl>      <dbl>
#> 1  1900    -0.0007
#> 2  1950    -0.0006
#> 3  2000    -0.0041
#> 
```
