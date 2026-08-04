# Between-country population-weighted Gini of per-capita food supply.

Reduces a per-capita nourishment series to a per-year, between-country,
population-weighted Gini coefficient, a diagnostic of how unequally
per-capita supply is shared across countries (it does not rescale the
per-country nourishment score). Within each `year`, countries are sorted
in descending per-capita supply; each country's population fraction
`pop_frac` and its share of the total supply mass `value_frac`
(per-capita supply times population, over the world total) give a score
`value_frac * (pop_frac + 2 * richer_frac)`, where `richer_frac` is the
cumulative population fraction of the strictly richer countries. The
Gini is `1 - sum(score)` over the countries in that year, `0` under a
perfectly equal distribution and rising toward `1` as supply
concentrates. Protein per-capita supply is the SJOS-N nourishment axis,
so it is the default; passing a different `value_col` (for example
`energy_kcal_cap_day`) takes the Gini of that axis instead. Rows with a
missing supply or population value are dropped before the computation.

## Usage

``` r
calculate_food_gini(x, value_col = protein_g_cap_day, pop_col = population)
```

## Arguments

- x:

  A tibble with a `year` column, the per-capita supply column named by
  `value_col` and the population column named by `pop_col` (for example
  a
  [`build_food_supply()`](https://eduaguilera.github.io/whep/reference/build_food_supply.md)
  output).

- value_col:

  The unquoted per-capita supply column whose between-country inequality
  is measured. Defaults to `protein_g_cap_day`.

- pop_col:

  The unquoted population column used as the inequality weight. Defaults
  to `population`.

## Value

A tibble keyed by `year` with the `gini` coefficient.

## Examples

``` r
calculate_food_gini(
  tibble::tribble(
    ~year, ~area_code, ~protein_g_cap_day, ~population,
    2000L, 10L, 40, 100,
    2000L, 20L, 40, 100,
    2000L, 30L, 40, 100
  )
)
#> # A tibble: 1 × 2
#>    year  gini
#>   <int> <dbl>
#> 1  2000     0
```
