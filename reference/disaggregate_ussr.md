# Split the pre-1992 aggregate USSR supply into successor states.

For years before `cutoff_year`, replaces the single aggregate USSR row
(the `area_code` listed as `ussr_area_code` in `ussr_shares`) with one
row per successor state, inheriting the aggregate's per-capita supply
values and scaling its population by the successor's 1992 population
share, so the split conserves the aggregate population and each
successor carries the aggregate per-capita supply. Rows at or after
`cutoff_year`, and all non-USSR rows, pass through unchanged. This
mirrors the Global SJOS-N pre-1992 USSR disaggregation, which
distributes the Russian Federation Food Balance Sheet supply across the
ex-USSR states by their 1992 population weights.

## Usage

``` r
disaggregate_ussr(x, ussr_shares, cutoff_year = 1992L)
```

## Arguments

- x:

  A tibble with `year`, `area_code` and `population` columns (plus any
  per-capita supply columns to inherit), for example a
  [`build_food_supply()`](https://eduaguilera.github.io/whep/reference/build_food_supply.md)
  output.

- ussr_shares:

  A lookup tibble with `ussr_area_code` (the aggregate USSR area code),
  `successor_area_code` (a successor state's area code) and `pop_share`
  (the successor's 1992 population share, summing to one per aggregate).
  Injected as a package-data / integration input.

- cutoff_year:

  The first year for which successor states report separately; the split
  applies to years strictly before it. Defaults to `1992L`.

## Value

`x` with the pre-cutoff aggregate USSR rows replaced by their
successor-state rows.

## Examples

``` r
disaggregate_ussr(
  tibble::tribble(
    ~year, ~area_code, ~protein_g_cap_day, ~population,
    1990L, 228L, 50, 100,
    1995L, 228L, 55, 120
  ),
  tibble::tribble(
    ~ussr_area_code, ~successor_area_code, ~pop_share,
    228L, 1L, 0.6,
    228L, 2L, 0.4
  )
)
#> # A tibble: 3 × 4
#>    year area_code protein_g_cap_day population
#>   <int>     <int>             <dbl>      <dbl>
#> 1  1995       228                55        120
#> 2  1990         1                50         60
#> 3  1990         2                50         40
```
