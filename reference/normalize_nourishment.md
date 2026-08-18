# Normalize and classify per-capita nourishment.

Adds a piecewise-normalized nourishment score `value_norm` and its Under
/ Adequate / Over class `nourish`. The normalization (the Global
`Intake_normalization`) is `value / floor` below the floor,
`1 + value / ceiling` above the ceiling and
`1 + (value - floor) / (ceiling - floor)` in between, so the score is
below 1 under the floor, exactly 1 at the floor, between 1 and 2 across
the adequate band and 2 or above at or past the ceiling. The class is
`"Under"` when `value_norm < 1`, `"Adequate"` when `value_norm < 2` and
`"Over"` otherwise. Protein is the SJOS-N nourishment axis, so the
defaults are the protein floor and ceiling (62.1 and 85.05 g/cap/day)
from
[nourishment_thresholds](https://eduaguilera.github.io/whep/reference/nourishment_thresholds.md).

Of those two defaults only the underlying 46 g/cap/day floor is sourced
(WHO/FAO/UNU TRS 935 Table 46, the safe intake of a 55 kg adult, itself
a 97.5th-percentile individual level rather than a population one). The
63 ceiling and the 1.35 factor that lifts both to a supply basis carry
no source; `nourishment_thresholds$provenance` says so per row.
[`build_nourishment_band()`](https://eduaguilera.github.io/whep/reference/build_nourishment_band.md)
is the sourced replacement for both bounds and is not wired in here yet.

Passing `value_col = energy_kcal_cap_day` runs the same arithmetic on a
different quantity and is **not** a second WHEP axis: the packaged
energy bounds are unsourced, and WHEP's energy column is gross
combustion energy where a dietary threshold is metabolisable. Supply
your own bounds and your own metabolisable series if you want that
comparison.

## Usage

``` r
normalize_nourishment(x, value_col = protein_g_cap_day, thresholds = NULL)
```

## Arguments

- x:

  A tibble carrying the per-capita nourishment column named by
  `value_col` (for example a
  [`build_food_supply()`](https://eduaguilera.github.io/whep/reference/build_food_supply.md)
  output).

- value_col:

  The unquoted nourishment column to normalize. Defaults to
  `protein_g_cap_day`.

- thresholds:

  Either a named `floor`/`ceiling` pair applied to every row (a named
  numeric vector or list), or a **data frame of per-country-year
  bounds** keyed by `year` and `area_code` with either
  `floor_g_cap_day`/`ceiling_g_cap_day` or `floor`/`ceiling` — so a
  [`build_nourishment_band()`](https://eduaguilera.github.io/whep/reference/build_nourishment_band.md)
  output passes straight through. A row that matches no band is
  classified `NA` and named in a warning, never silently given the flat
  default. When `NULL` (default) the flat protein bounds from
  [nourishment_thresholds](https://eduaguilera.github.io/whep/reference/nourishment_thresholds.md)
  are used.

## Value

`x` with `value_norm` (numeric score) and `nourish` (`"Under"`,
`"Adequate"` or `"Over"`) added.

## Examples

``` r
normalize_nourishment(
  tibble::tribble(
    ~area_code, ~protein_g_cap_day,
    10L, 30,
    20L, 70,
    30L, 100
  )
)
#> # A tibble: 3 × 4
#>   area_code protein_g_cap_day value_norm nourish 
#>       <int>             <dbl>      <dbl> <chr>   
#> 1        10                30      0.483 Under   
#> 2        20                70      1.34  Adequate
#> 3        30               100      2.18  Over    
```
