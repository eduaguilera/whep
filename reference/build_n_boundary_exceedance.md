# Build the surplus-mode critical-nitrogen boundary exceedance.

Compares a
[`calculate_n_surplus()`](https://eduaguilera.github.io/whep/reference/calculate_n_surplus.md)
output against a
[`read_critical_n()`](https://eduaguilera.github.io/whep/reference/read_critical_n.md)
critical-nitrogen layer, per grid cell and per crop. For
`metric = "surplus"` each crop's per-hectare surplus (`surplus_kgn_ha`)
is compared to the cell's critical nitrogen surplus; for
`metric = "input"` each crop's per-hectare nitrogen input
(`n_input_std_t` per `area_ha`) is compared to the cell's critical
nitrogen input. The cell's single critical value broadcasts to every
crop sharing the cell. Each crop's nitrogen is split into an
`exceedance` part (above the boundary) and a `within_boundary` part, as
both a per-hectare intensity and a mass, with the exceedance share
`exceed_share = (actual - critical) / actual` when the crop is above the
critical value and `0` otherwise (and `0` when the crop is at or below
zero). `exceedance + within_boundary == actual` holds by construction.
`resolution = "grid"` keeps the full per-crop grid key; `"polity"` and
`"country"` sum the mass terms over cells to `area_code`,
`item_cbs_code`, `year`; `"image_region"` sums to IMAGE region when
`cell_polity` supplies an `image_region` column, else falls back to the
polity aggregate with a note.

## Usage

``` r
build_n_boundary_exceedance(
  surplus,
  critical,
  land_use = c("ara", "all"),
  resolution = c("grid", "polity", "country", "image_region"),
  metric = c("surplus", "input"),
  cell_polity = NULL,
  example = FALSE
)
```

## Arguments

- surplus:

  A
  [`calculate_n_surplus()`](https://eduaguilera.github.io/whep/reference/calculate_n_surplus.md)
  output keyed by `lon`, `lat`, `area_code`, `item_cbs_code`, `year`,
  carrying `area_ha` and, for `metric = "surplus"`, `surplus_kgn_ha`, or
  for `metric = "input"`, `n_input_std_t`.

- critical:

  A
  [`read_critical_n()`](https://eduaguilera.github.io/whep/reference/read_critical_n.md)
  output (`lon`, `lat`, `value` in kg N per hectare), the critical
  nitrogen surplus for `metric = "surplus"` or the critical nitrogen
  input for `metric = "input"`. Every retained positive-area cell must
  have a non-missing critical value; incomplete coverage aborts instead
  of silently dropping the cell.

- land_use:

  Land-use scope the `critical` layer was read for, `"all"` (default) or
  `"ara"`; a provenance stamp on the output.

- resolution:

  Output grain: `"grid"` (default, per crop per cell), `"polity"` or
  `"country"` (per crop per country) or `"image_region"`.

- metric:

  Which pressure to compare: `"surplus"` (default, per-hectare surplus
  vs critical surplus) or `"input"` (per-hectare nitrogen input vs
  critical input).

- cell_polity:

  Optional crosswalk carrying `area_code` and `image_region`, used only
  for `resolution = "image_region"`. Defaults to `NULL`.

- example:

  If `TRUE`, return a small fixture instead of computing. Defaults to
  `FALSE`.

## Value

For `resolution = "grid"`, a tibble keyed `lon`, `lat`, `area_code`,
`item_cbs_code`, `year` with `area_ha`, `critical_kgn_ha`,
`actual_kgn_ha`, `exceed_share`, `exceedance_kgn_ha`,
`within_boundary_kgn_ha`, the mass terms `exceedance_n_t`,
`within_boundary_n_t`, `actual_n_t`, `production_n_t` (the
harvest-removal nitrogen the footprint's `"production"` category traces,
carried through only when the `surplus` input supplies it), and the
`metric`, `land_use`, `method_boundary` stamps. For the aggregate
resolutions, the grouping key with the summed mass terms and the same
stamps.

## Exceedance is a decomposition, not the overshoot

`exceedance_kgn_ha` splits the actual pressure into its over- and
within-boundary parts, so `exceedance + within_boundary == actual` and
neither part can exceed the pressure itself. Where the critical value is
negative (the cell is so sensitive that the safe surplus is a net
removal) the whole pressure is exceedance, and the overshoot MAGNITUDE
the source archive publishes, `actual - critical`, is strictly larger.
Both are defensible; they answer different questions, and only the
decomposition can carry a footprint. Verified against Schulte-Uebbing's
own exceedance layer (`threshold = "mi"`, `land_use = "ara"`): the two
agree to floating-point on all 26,497 positive-critical cells, and
diverge only on the 2,076 negative-critical ones, costing 0.6% of global
exceedance mass.

## Examples

``` r
build_n_boundary_exceedance(example = TRUE)
#> # A tibble: 5 × 17
#>     lon   lat area_code item_cbs_code  year area_ha critical_kgn_ha
#>   <dbl> <dbl>     <int>         <int> <int>   <dbl>           <dbl>
#> 1  0.25  0.25         1          2511  2010     100              50
#> 2  0.25  0.25         1          2513  2010      50              50
#> 3  0.75  0.25         1          2511  2010     200             120
#> 4  0.25  0.75         1          2511  2010      40              40
#> 5  0.75  0.75         1          2555  2010      10             100
#> # ℹ 10 more variables: actual_kgn_ha <dbl>, exceed_share <dbl>,
#> #   exceedance_kgn_ha <dbl>, within_boundary_kgn_ha <dbl>,
#> #   exceedance_n_t <dbl>, within_boundary_n_t <dbl>, actual_n_t <dbl>,
#> #   metric <chr>, land_use <chr>, method_boundary <chr>
```
