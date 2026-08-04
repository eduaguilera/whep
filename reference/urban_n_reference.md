# Spain historical urban nitrogen applied to agriculture.

National-total nitrogen from Spanish urban human excreta and municipal
waste actually applied to agricultural land, at benchmark years. Used by
[`build_urban_n()`](https://eduaguilera.github.io/whep/reference/build_urban_n.md)
as the global default per-capita urban-N-to-agriculture rate (a
documented placeholder, see that function's Details): this is Spain's
own historical series applied everywhere, not a globally-calibrated
estimate.

## Usage

``` r
urban_n_reference
```

## Format

A tibble with columns:

- area_code:

  Numeric FAOSTAT area code, as everywhere else in this package;
  currently only `203` (Spain). The vendored CSV records the ISO3 string
  `"ESP"` and it is resolved to a code through
  [polity_area_crosswalk](https://eduaguilera.github.io/whep/reference/polity_area_crosswalk.md)
  at build time, so this series joins to area-keyed tables without a
  hand conversion. It held the string itself until 0.3.0.9000, which
  made it the one column named `area_code` in this package that was not
  one (whep#401). Keep using it to join; it is an aggregation key, not
  the territory's identity.

- polity_code:

  The polity the row's territory IS, e.g. `"ESP-1800-2025"`: the
  identifier every place in WHEP that names a territory is meant to
  carry (whep#458). Resolved at build time against the polity active in
  that benchmark year, so a series spanning a succession would carry
  more than one code. Added alongside `area_code` rather than replacing
  it (whep#495), so callers keying on the numeric are unaffected.

- year:

  Benchmark calendar year.

- urban_n_gg:

  National-total urban nitrogen applied to agriculture (Gg N/year).

## Source

Aguilera, E. (WHEP project team). Own estimation, transcribed from the
Spain_Hist repository (private project data, not a public DOI):
`input/Urban_waste.xlsx` sheet `UrbanN` and
`input/updates/UrbanN_update.csv`.

## Examples

``` r
urban_n_reference
#> # A tibble: 10 × 4
#>    area_code polity_code    year urban_n_gg
#>        <int> <chr>         <dbl>      <dbl>
#>  1       203 ESP-1800-2025  1860       6.97
#>  2       203 ESP-1800-2025  1900       8.04
#>  3       203 ESP-1800-2025  1950      12.1 
#>  4       203 ESP-1800-2025  1990      16.8 
#>  5       203 ESP-1800-2025  2000      28.9 
#>  6       203 ESP-1800-2025  2008      43.0 
#>  7       203 ESP-1800-2025  2016      50.0 
#>  8       203 ESP-1800-2025  2018      54.8 
#>  9       203 ESP-1800-2025  2020      51.4 
#> 10       203 ESP-1800-2025  2022      61.3 
```
