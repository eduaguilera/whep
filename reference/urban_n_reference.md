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

  ISO3 country code (currently only `"ESP"`).

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
#> # A tibble: 10 × 3
#>    area_code  year urban_n_gg
#>    <chr>     <dbl>      <dbl>
#>  1 ESP        1860       6.97
#>  2 ESP        1900       8.04
#>  3 ESP        1950      12.1 
#>  4 ESP        1990      16.8 
#>  5 ESP        2000      28.9 
#>  6 ESP        2008      43.0 
#>  7 ESP        2016      50.0 
#>  8 ESP        2018      54.8 
#>  9 ESP        2020      51.4 
#> 10 ESP        2022      61.3 
```
