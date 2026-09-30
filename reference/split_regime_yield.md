# Split a cell-crop's production between its rainfed and irrigated regimes.

Applies the irrigated:rainfed yield ratio `R` of
[`build_regime_yield_ratio()`](https://eduaguilera.github.io/whep/reference/build_regime_yield_ratio.md)
to a cell-crop's production `P` and its rainfed and irrigated harvested
areas `A_r`, `A_i`, keeping the production: `Y_r = P / (A_r + R * A_i)`
and `Y_i = R * Y_r`, so `A_r * Y_r + A_i * Y_i = P`.

`R` starts from `ratio_unbounded` and two plausibility bounds can lower
it, never below 1. The result is the output's `ratio`, the only ratio
fit to weight anything:

- **Irrigated-yield ceiling.** Where `Y_i` would exceed the item's
  `Y_max`, `R` is lowered until `Y_i = Y_max`,
  `R = Y_max * A_r / (P - Y_max * A_i)`. `Y_max` is the 99th percentile
  of the item's national yields (production over harvested area) in
  [`get_primary_production()`](https://eduaguilera.github.io/whep/reference/get_primary_production.md),
  pooled over countries and the years 1961-2023.

- **Rainfed-yield floor.** Where `Y_r` would fall below the item's
  `Y_min`, the 1st percentile of the same pool, `R` is lowered until
  `Y_r = Y_min`, `R = (P - Y_min * A_r) / (Y_min * A_i)`.

Lowering `R` raises `Y_r` and lowers `Y_i`, so the floor, applied after
the ceiling, cannot break it. An item with no FAOSTAT yields of its own
takes the pooled yields of the items that directly carry one of its SPAM
crops in
[regime_yield_crop_mapping](https://eduaguilera.github.io/whep/reference/regime_yield_crop_mapping.md)
(`spam_basis` `"direct"` or `"direct_aggregate"`), stamped
`"bound_via_spam_crop"`.

A cell-crop with area in one regime only is a trivial split: all its
production is on that regime, no ratio applies (`ratio` is 1, whatever
`ratio_unbounded` is) and the bounds are not tested. Only a cell-crop
with no area at all, or with both regimes and no `ratio_unbounded`, gets
`NA`.

## Usage

``` r
split_regime_yield(cells, bound = c("global", "region"), production = NULL)
```

## Arguments

- cells:

  A tibble with `area_code`, `item_prod_code`, `production_t`
  (production in the units of
  [`get_primary_production()`](https://eduaguilera.github.io/whep/reference/get_primary_production.md)'s
  `"tonnes"` rows), `rainfed_ha`, `irrigated_ha` and `ratio_unbounded`
  (as
  [`build_regime_yield_ratio()`](https://eduaguilera.github.io/whep/reference/build_regime_yield_ratio.md)
  returns it). Other columns are kept.

- bound:

  Which pool of national yields sets `Y_max` and `Y_min`: `"global"`
  (default, all countries) or `"region"` (the countries of the cell's
  WHEP region, the `region` column of
  [regions_full](https://eduaguilera.github.io/whep/reference/regions_full.md);
  where the region has no yields for the item, the world's pool, stamped
  `_global`).

- production:

  Optional
  [`get_primary_production()`](https://eduaguilera.github.io/whep/reference/get_primary_production.md)
  output (`year`, `area_code`, `item_prod_code`, `unit`, `value`) used
  instead of building it.

## Value

`cells` with:

- `ratio`: the irrigated:rainfed yield ratio used, after the bounds (1
  on a trivial split).

- `yield_rainfed`, `yield_irrigated`: production per hectare of each
  regime.

- `yield_max`, `yield_min`: the bounds applied; `NA` where the item has
  no yields to set them.

- `method_regime_split`: `"yield_ratio"`, `"trivial_rainfed_only"`,
  `"trivial_irrigated_only"`, `"no_area"` or `"no_ratio"`.

- `method_regime_bound`: the ceiling: `"not_binding"`, `"clipped"`,
  `"clipped_at_one"` (lowered to 1 and still above `Y_max`: the mean
  yield itself exceeds it), `"no_bound"`, `"not_applicable_trivial"` or
  `"not_applicable"` (no area, or no ratio).

- `method_rainfed_floor`: the floor: `"not_binding"`, `"rainfed_floor"`,
  `"rainfed_floor_at_one"` (lowered to 1 and still below `Y_min`: the
  mean yield itself is below it), `"no_bound"`,
  `"not_applicable_trivial"` or `"not_applicable"`.

- `method_bound_source`: which yields set the bounds: `"own_yields"`,
  `"bound_via_spam_crop"`, either suffixed `"_global"` where a regional
  pool fell back to the world's, or `"none"`.

- `method_yield_bound`: the `bound` chosen.

## Examples

``` r
cells <- tibble::tribble(
  ~area_code, ~item_prod_code, ~production_t, ~rainfed_ha, ~irrigated_ha,
  ~ratio_unbounded,
  203L, 15L, 500, 100, 50, 1.8
)
production <- tibble::tribble(
  ~year, ~area_code, ~item_prod_code, ~unit, ~value,
  2010L, 203L, 15L, "tonnes", 3000,
  2010L, 203L, 15L, "ha", 1000
)
split_regime_yield(cells, production = production)
#> # A tibble: 1 × 16
#>   area_code item_prod_code production_t rainfed_ha irrigated_ha ratio_unbounded
#>       <int>          <int>        <dbl>      <dbl>        <dbl>           <dbl>
#> 1       203             15          500        100           50             1.8
#> # ℹ 10 more variables: ratio <dbl>, yield_rainfed <dbl>, yield_irrigated <dbl>,
#> #   yield_max <dbl>, yield_min <dbl>, method_regime_split <chr>,
#> #   method_regime_bound <chr>, method_rainfed_floor <chr>,
#> #   method_bound_source <chr>, method_yield_bound <chr>
```
