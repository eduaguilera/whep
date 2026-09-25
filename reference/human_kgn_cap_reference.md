# Human-population nitrogen rate per urban inhabitant.

The calibration nitrogen per URBAN inhabitant,
`human_n_reference$human_n_gg * 1e6` over the calibration country's
urban population, at each
[human_n_reference](https://eduaguilera.github.io/whep/reference/human_n_reference.md)
benchmark year a verified rate could be computed for. It is the rate
[`build_human_n()`](https://eduaguilera.github.io/whep/reference/build_human_n.md)
applies under `population_basis = "urban"`. See
`data-raw/build_human_kgn_cap.R` for the derivation: every
[human_n_reference](https://eduaguilera.github.io/whep/reference/human_n_reference.md)
benchmark year, including 1860, 1900 and 1950, has its own verified
denominator. Most rows use real gridded HYDE baseline-scenario urban
population (summed over the calibration country's cell_polity footprint
from
[`whep::build_cell_polity()`](https://eduaguilera.github.io/whep/reference/build_cell_polity.md));
the 2018, 2020 and 2022 rows keep the World Bank `SP.URB.TOTL`
urban-population denominator instead, because the local HYDE mirror used
to build this table only extends through 2017. Every row is real,
verified data; not all rows share the same source.

`urban_kgn_cap_reference` is the deprecated former name of this table,
kept for one release. It holds the same rows, with `human_kgn_cap` under
its former name `urban_kgn_cap`.

## Usage

``` r
human_kgn_cap_reference

urban_kgn_cap_reference
```

## Format

A tibble with columns:

- year:

  Benchmark calendar year.

- human_kgn_cap:

  Human-population nitrogen applied to agriculture per urban inhabitant
  (kg N per person per year).

## Source

Derived from
[human_n_reference](https://eduaguilera.github.io/whep/reference/human_n_reference.md)
and the calibration country's urban population: HYDE baseline-scenario
gridded population (1860-2016 rows) and World Bank indicator
`SP.URB.TOTL` (2018-2022 rows); see `data-raw/build_human_kgn_cap.R`.

## Examples

``` r
human_kgn_cap_reference
#> # A tibble: 10 × 2
#>     year human_kgn_cap
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
