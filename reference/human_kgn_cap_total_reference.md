# Human-population nitrogen rate per inhabitant.

The calibration nitrogen per inhabitant,
`human_n_reference$human_n_gg * 1e6 / calibration_population`, where
`calibration_population` is the calibration country's UN WPP 2024 total
population. It is the rate
[`build_human_n()`](https://eduaguilera.github.io/whep/reference/build_human_n.md)
applies under its default `population_basis = "total"`, whose population
level is the UN WPP total
([`build_total_population_grid()`](https://eduaguilera.github.io/whep/reference/build_total_population_grid.md)),
and the counterpart of
[human_kgn_cap_reference](https://eduaguilera.github.io/whep/reference/human_kgn_cap_reference.md)
(kg N per URBAN inhabitant). Pairing each population with the rate on
its own basis is what lets either basis regenerate its calibration
total; applying the per-urban-inhabitant rate to a total population
would scale the term by the inverse urban fraction (global WPP total
over HYDE urban population: 3.02 in 1960, 2.01 in 2010, 1.91 in 2017).

The series starts at 1950, the first year UN WPP covers, so the 1860 and
1900 benchmarks of
[human_n_reference](https://eduaguilera.github.io/whep/reference/human_n_reference.md)
have no row here. The same calibration series is applied as a global
default under either basis; see
[`build_human_n()`](https://eduaguilera.github.io/whep/reference/build_human_n.md).

## Usage

``` r
human_kgn_cap_total_reference
```

## Format

A tibble with columns:

- year:

  Benchmark calendar year.

- human_kgn_cap:

  Human-population nitrogen applied to agriculture per inhabitant (kg N
  per person per year).

- calibration_population:

  The calibration country's total population that year (persons), the
  denominator, from
  [`read_wpp_population()`](https://eduaguilera.github.io/whep/reference/read_wpp_population.md)
  with `by = "total"`.

## Source

Derived from
[human_n_reference](https://eduaguilera.github.io/whep/reference/human_n_reference.md)
and United Nations, Department of Economic and Social Affairs,
Population Division (2024), World Population Prospects 2024, medium
variant (`WPP2024_PopulationByAge5GroupSex_Medium.csv.gz`); see
`data-raw/build_human_kgn_cap.R`.

## Examples

``` r
human_kgn_cap_total_reference
#> # A tibble: 8 × 3
#>    year human_kgn_cap calibration_population
#>   <dbl>         <dbl>                  <dbl>
#> 1  1950         0.430               28077050
#> 2  1990         0.431               39084978
#> 3  2000         0.704               41019777
#> 4  2008         0.931               46235055
#> 5  2016         1.07                46732771
#> 6  2018         1.16                47092820
#> 7  2020         1.08                47679489
#> 8  2022         1.28                47828383
```
