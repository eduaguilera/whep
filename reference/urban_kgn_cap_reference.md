# Spain historical per-capita urban nitrogen rate.

The per-capita urban-nitrogen-to-agriculture rate,
`urban_n_reference$urban_n_gg * 1e6 / spain_urban_population`, at each
`urban_n_reference` benchmark year
[`build_urban_n()`](https://eduaguilera.github.io/whep/reference/build_urban_n.md)
could compute a verified rate for. See `data-raw/build_urban_kgn_cap.R`
for the derivation: every `urban_n_reference` benchmark year, including
1860, 1900 and 1950, now has its own verified denominator. Most rows use
real gridded HYDE baseline-scenario urban population (summed over
Spain's cell_polity footprint from
[`whep::build_cell_polity()`](https://eduaguilera.github.io/whep/reference/build_cell_polity.md));
the 2018, 2020 and 2022 rows keep the World Bank `SP.URB.TOTL`
urban-population denominator instead, because the local HYDE mirror used
to build this table only extends through 2017. Every row is real,
verified data; not all rows share the same source.

## Usage

``` r
urban_kgn_cap_reference
```

## Format

A tibble with columns:

- year:

  Benchmark calendar year.

- urban_kgn_cap:

  Per-capita urban nitrogen applied to agriculture (kg N per person per
  year).

## Source

Derived from `urban_n_reference` and Spain urban population: HYDE
baseline-scenario gridded population (1860-2016 rows) and World Bank
indicator `SP.URB.TOTL` (2018-2022 rows); see
`data-raw/build_urban_kgn_cap.R`.

## Examples

``` r
urban_kgn_cap_reference
#> # A tibble: 10 × 2
#>     year urban_kgn_cap
#>    <dbl>         <dbl>
#>  1  1860         1.08 
#>  2  1900         0.869
#>  3  1950         0.803
#>  4  1990         0.576
#>  5  2000         0.941
#>  6  2008         1.24 
#>  7  2016         1.33 
#>  8  2018         1.47 
#>  9  2020         1.36 
#> 10  2022         1.60 
```
