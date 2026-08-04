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
[nourishment_thresholds](https://eduaguilera.github.io/whep/reference/nourishment_thresholds.md);
switching `value_col` to `energy_kcal_cap_day` and passing
`thresholds = c(floor = 2300, ceiling = 2900)` classifies dietary energy
instead, a completeness cross-check rather than the nitrogen
classification.

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

  Optional named `floor` and `ceiling` (a named numeric vector or list).
  When `NULL` (default) the protein floor and ceiling from
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
