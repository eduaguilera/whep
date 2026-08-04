# Build the per-capita nitrogen-boundary versus nourishment scatter.

Normalizes each country's total anthropogenic reactive-nitrogen per
capita against the world per-capita planetary-nitrogen boundary and
joins it to the nourishment normalization, yielding the safe-and-just
nitrogen scatter (one point per country-year: nourishment adequacy on
the x axis, boundary pressure on the y axis, population as the point
weight). The world per-capita boundary converts the Tg N/yr limits in
`params` (`boundary_low`, `boundary_high`) to kg N/cap/yr by dividing by
the world population (the complete `population` table summed per year,
falling back to `nourishment`). The upper per-capita boundary is capped
at the packaged `per_capita_cap`. The normalization is the Global
piecewise: `n_percapita_kg / low_pc` below the lower bound,
`1 + (n_percapita_kg - low_pc) / (high_pc - low_pc)` within the band,
and `min(1 + n_percapita_kg / high_pc, 6)` above the upper bound. The
result is then scaled by `afs_share`, the agri-food-system share of the
boundary (a parameter, default 0.8, flagged provisional).

## Usage

``` r
build_n_boundary_percapita(
  n_percapita,
  nourishment,
  population = NULL,
  params = NULL,
  afs_share = 0.8
)
```

## Arguments

- n_percapita:

  A tibble keyed by `year`, `area_code` with `n_percapita_kg`, the
  country total anthropogenic reactive N per capita (kg N/cap/yr,
  synthetic plus biological fixation), from
  [`build_n_percapita()`](https://eduaguilera.github.io/whep/reference/build_n_percapita.md)
  or injected directly.

- nourishment:

  A tibble keyed by `year`, `area_code` with `value_norm` (the
  nourishment normalization, for example a
  [`normalize_nourishment()`](https://eduaguilera.github.io/whep/reference/normalize_nourishment.md)
  output) and `population` (absolute persons), whose per-year population
  sum sets the world per-capita boundary.

- population:

  Optional complete population tibble keyed by `year`, `area_code`, with
  `population` in persons. Supplying it prevents countries missing
  nourishment data from being omitted from the world denominator.

- params:

  Boundary parameters, defaulting to
  [n_boundary_params](https://eduaguilera.github.io/whep/reference/n_boundary_params.md),
  read for the `boundary_low` and `boundary_high` Tg N/yr limits.

- afs_share:

  The agri-food-system share of the planetary boundary applied to the
  normalized boundary pressure. Defaults to `0.8` (provisional).

## Value

A tibble keyed by `year`, `area_code` with `nourish_norm` (the
nourishment normalization), `boundary_norm` (the afs-scaled per-capita
boundary normalization) and `population`.

## Examples

``` r
build_n_boundary_percapita(
  n_percapita = tibble::tribble(
    ~year, ~area_code, ~n_percapita_kg,
    2000L, 10L, 5,
    2000L, 20L, 15
  ),
  nourishment = tibble::tribble(
    ~year, ~area_code, ~value_norm, ~population,
    2000L, 10L, 0.8, 3e9,
    2000L, 20L, 1.5, 3e9
  )
)
#> # A tibble: 2 × 5
#>    year area_code nourish_norm boundary_norm population
#>   <int>     <int>        <dbl>         <dbl>      <dbl>
#> 1  2000        10          0.8          0.4  3000000000
#> 2  2000        20          1.5          1.17 3000000000
```
