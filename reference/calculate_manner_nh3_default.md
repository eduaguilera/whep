# Estimate ammonia-N volatilisation with MANNER's gross-default technique and incorporation-delay blend.

A thin wrapper around
[`calculate_manner_nh3()`](https://eduaguilera.github.io/whep/reference/calculate_manner_nh3.md)'s
organic-manure path that fills in `technique` and
`incorporation_delay_h` from
[manner_default_technique_mix](https://eduaguilera.github.io/whep/reference/manner_default_technique_mix.md)
instead of requiring them as caller- supplied drivers. It calls
[`calculate_manner_nh3()`](https://eduaguilera.github.io/whep/reference/calculate_manner_nh3.md)
once per row of
[manner_default_technique_mix](https://eduaguilera.github.io/whep/reference/manner_default_technique_mix.md)
and combines the results into a single share-weighted emission factor.
Use this only where real per-cell/per-era application-technique survey
data does not exist (see Details).

## Usage

``` r
calculate_manner_nh3_default(
  n_applied_t = NULL,
  fertiliser = NULL,
  drivers = list(),
  example = FALSE
)
```

## Arguments

- n_applied_t:

  Numeric, nitrogen applied (t).

- fertiliser:

  One of `"cattle_slurry"`, `"pig_slurry"`, `"FYM"`, `"poultry_manure"`,
  `"urban"` (the
  [`calculate_manner_nh3()`](https://eduaguilera.github.io/whep/reference/calculate_manner_nh3.md)
  organic-manure path; the gross default only applies there).

- drivers:

  A named list of driver values, as in
  [`calculate_manner_nh3()`](https://eduaguilera.github.io/whep/reference/calculate_manner_nh3.md)'s
  organic path, EXCEPT `technique` and `incorporation_delay_h` are
  neither required nor used (they come from
  [manner_default_technique_mix](https://eduaguilera.github.io/whep/reference/manner_default_technique_mix.md)
  instead).

- example:

  If `TRUE`, return a small fixture instead of computing from drivers.
  Defaults to `FALSE`.

## Value

A tibble with `n_applied_t`, `ef` (share-weighted realised emission
factor), `nh3_n_t` and `method_manner`.

## Details

[manner_default_technique_mix](https://eduaguilera.github.io/whep/reference/manner_default_technique_mix.md)
is a deliberate, permanent gross- assumption default, not a
region/era-specific survey. It fixes `technique = "Broadcast"` on every
row, matching Spain_Hist's own real production MANNER run, which itself
hardcodes Broadcast application nationally with no region/era variation
(`factor_ap_technique <- application_technique_manure[Technique == "Broadcast", ...]`
applied unconditionally to its whole national run). For incorporation
delay, it blends four of
[manner_incorporation_factor](https://eduaguilera.github.io/whep/reference/manner_incorporation_factor.md)'s
`delay_bin` categories in equal 25% shares: 25% of applied nitrogen
assumed never incorporated (`incorporation_delay_h = NA`, matching how
[`calculate_manner_nh3()`](https://eduaguilera.github.io/whep/reference/calculate_manner_nh3.md)
treats a missing delay as `"No incorporation"`), 25% incorporated within
2 hours (`incorporation_delay_h = 2`), 25% within 12-24 hours
(`incorporation_delay_h = 24`) and 25% within 1-2 days
(`incorporation_delay_h = 48`). Only `ef` and `nh3_n_t` are
share-weighted across the four blend calls; `n_applied_t` is carried
through as-is, not re-weighted, since the full `n_applied_t` is assumed
split across the four incorporation-delay scenarios rather than
quadruplicated.

## Examples

``` r
calculate_manner_nh3_default(example = TRUE)
#> # A tibble: 1 × 4
#>   n_applied_t    ef nh3_n_t method_manner               
#>         <dbl> <dbl>   <dbl> <chr>                       
#> 1          10 0.219    1.31 manner_default_cattle_slurry
```
