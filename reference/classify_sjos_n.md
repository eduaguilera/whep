# Classify crops into the 2-way SJOS-N safe-and-just space.

Crosses the ecological boundary side with the nourishment side into the
six ordered
[sjos_levels](https://eduaguilera.github.io/whep/reference/sjos_levels.md).
Per crop (`item_cbs_code`), the boundary side is `"Exceedance"` when the
crop-country-year's `exceedance_n_t` is positive and `"Within_boundary"`
otherwise (the all-zero and missing cases fall to `"Within_boundary"`).
The country's nourishment class (`nourish`, from
[`normalize_nourishment()`](https://eduaguilera.github.io/whep/reference/normalize_nourishment.md))
is joined by `year` and `area_code` and broadcast to each of its crops.
The classification `paste(boundary_side, nourish)` is one of
`"Within_boundary Under"` ... `"Exceedance Over"`, returned as a factor
with all six `sjos_levels$level` levels. This reproduces Global's 2-way
remap (`Global/R/sjos_n.r:363`) at the per-`item_cbs` granularity Module
4's footprint needs.

The boundary side reads `exceedance_n_t`, which is a decomposition of
the actual pressure (see
[`build_n_boundary_exceedance()`](https://eduaguilera.github.io/whep/reference/build_n_boundary_exceedance.md))
and so is capped at it. Where the critical surplus is negative the
overshoot the source archive reports, `actual - critical`, is larger, so
the classification is conservative there: it can call a crop
within-boundary that Schulte-Uebbing's own exceedance layer puts over
it. Measured against that layer (`threshold = "mi"`, `land_use = "ara"`,
28,573 cells, 2,076 of them with a negative critical surplus): the two
definitions agree exactly on every positive-critical cell, 288 cells
fall on opposite sides, and after aggregation to countries 1 of 175
flips and the global exceedance mass is 0.6% low.

## Usage

``` r
classify_sjos_n(exceedance, nourishment, level_col = sjos_class)
```

## Arguments

- exceedance:

  A
  [`build_n_boundary_exceedance()`](https://eduaguilera.github.io/whep/reference/build_n_boundary_exceedance.md)
  output at `resolution = "country"`, keyed by `year`, `area_code`,
  `item_cbs_code` with the mass terms `exceedance_n_t`,
  `within_boundary_n_t`, `actual_n_t`.

- nourishment:

  A
  [`normalize_nourishment()`](https://eduaguilera.github.io/whep/reference/normalize_nourishment.md)
  output carrying `year`, `area_code` and the `nourish` class (`"Under"`
  / `"Adequate"` / `"Over"`), one row per country-year.

- level_col:

  The unquoted name for the classification column. Defaults to
  `sjos_class`.

## Value

A tibble keyed by `year`, `area_code`, `item_cbs_code` with the mass
terms `exceedance_n_t`, `within_boundary_n_t`, `actual_n_t`, the joined
`nourish` class, the `boundary_side` and the classification column (a
factor over `sjos_levels$level`, named by `level_col`).

## Examples

``` r
classify_sjos_n(
  exceedance = tibble::tribble(
    ~year,
    ~area_code,
    ~item_cbs_code,
    ~exceedance_n_t,
    ~within_boundary_n_t,
    ~actual_n_t,
    2010L, 10L, 2511L, 5, 3, 8,
    2010L, 10L, 2513L, 0, 4, 4
  ),
  nourishment = tibble::tribble(
    ~year, ~area_code, ~nourish,
    2010L, 10L, "Over"
  )
)
#> # A tibble: 2 × 9
#>    year area_code item_cbs_code exceedance_n_t within_boundary_n_t actual_n_t
#>   <int>     <int>         <int>          <dbl>               <dbl>      <dbl>
#> 1  2010        10          2511              5                   3          8
#> 2  2010        10          2513              0                   4          4
#> # ℹ 3 more variables: nourish <chr>, boundary_side <chr>, sjos_class <fct>
```
