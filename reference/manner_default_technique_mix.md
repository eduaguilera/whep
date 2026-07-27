# Gross-default technique/incorporation-delay blend for MANNER.

A deliberate, permanent gross-assumption default for
[`calculate_manner_nh3()`](https://eduaguilera.github.io/whep/reference/calculate_manner_nh3.md)'s
`technique` and `incorporation_delay_h` organic-manure drivers, for use
where real per-cell/per-era manure-application-technique survey data
does not exist (which is everywhere right now). Every row fixes
`technique = "Broadcast"`, matching Spain_Hist's own real production
MANNER run (which itself hardcodes Broadcast application nationally with
no region/era variation). The four rows blend
[manner_incorporation_factor](https://eduaguilera.github.io/whep/reference/manner_incorporation_factor.md)'s
`delay_bin` categories in equal shares: a quarter of applied nitrogen
assumed never incorporated, a quarter incorporated within 2 hours, a
quarter within 12-24 hours, and a quarter within 1-2 days. See
[`calculate_manner_nh3_default()`](https://eduaguilera.github.io/whep/reference/calculate_manner_nh3_default.md)'s
Details for the full reasoning; this is not derived from a region/era
survey.

## Usage

``` r
manner_default_technique_mix
```

## Format

A tibble with columns:

- technique:

  Manure application technique: always `"Broadcast"`.

- delay_bin:

  Incorporation-delay bin label, one of
  [manner_incorporation_factor](https://eduaguilera.github.io/whep/reference/manner_incorporation_factor.md)'s
  `delay_bin` values: `"No incorporation"`, `"<2 h"`, `"12-24 h"` or
  `"1-2 days"`.

- incorporation_delay_h:

  Hours between surface application and soil incorporation passed to
  [`calculate_manner_nh3()`](https://eduaguilera.github.io/whep/reference/calculate_manner_nh3.md):
  `NA` for `"No incorporation"`, `2` for `"<2 h"`, `24` for `"12-24 h"`
  and `48` for `"1-2 days"`.

- share:

  Fraction of applied nitrogen assumed to follow this
  incorporation-delay bin; the four shares sum to 1 (0.25 each).

## Source

Gross default assumption, not a region/era-specific survey; see
[`calculate_manner_nh3_default()`](https://eduaguilera.github.io/whep/reference/calculate_manner_nh3_default.md)'s
Details for the reasoning.

## Examples

``` r
manner_default_technique_mix
#> # A tibble: 4 × 4
#>   technique delay_bin        incorporation_delay_h share
#>   <chr>     <chr>                            <dbl> <dbl>
#> 1 Broadcast No incorporation                    NA  0.25
#> 2 Broadcast <2 h                                 2  0.25
#> 3 Broadcast 12-24 h                             24  0.25
#> 4 Broadcast 1-2 days                            48  0.25
```
