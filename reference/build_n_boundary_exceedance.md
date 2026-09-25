# Build source-exact gridded critical-nitrogen exceedance.

Compares WHEP crop nitrogen pressure with the deposited Schulte-Uebbing
et al. (2022) 2010 yield-gap critical surface. The calculation first
aggregates every crop and polity contribution in a source cell, compares
that one cell pressure with one critical allowance, and only then
attributes the cell allowance, signed margin and positive overshoot back
to crops. Total-input attribution uses crop input shares; surplus
attribution uses signed crop surplus shares. Signed surplus shares can
be negative or greater than one.

The critical layer must carry its deposited `source_area_ha` and
`image_region`. `resolution = "cell"` returns one row per source cell
and year. Other resolutions return the approved crop attribution.
Exactly zero or numerically near-zero pressure denominators keep the
complete cell result but allocate no crop share; an explicit
`cell_residual` record carries the unallocated critical allowance,
signed margin and positive overshoot. APIs requiring complete crop
attribution hard-error rather than fabricate a fallback.

Actual-pressure rows naming no crop cannot meet a critical allowance and
are excluded before the cell comparison. The exclusion is reported,
never silent: a message names the rows and the pressure they carried
when that pressure is zero (the only case a gridded
[`build_nitrogen_balance()`](https://eduaguilera.github.io/whep/reference/build_nitrogen_balance.md)
produces), and a warning when it is not.

## Usage

``` r
build_n_boundary_exceedance(
  surplus = NULL,
  critical = NULL,
  land_use = c("ara", "all", "igl"),
  resolution = c("grid", "cell", "polity", "country", "image_region"),
  metric = c("surplus", "input", "new_fixation"),
  cell_polity = NULL,
  allocation_scenario = c("yield_gap", "no_increase", "new_fixation"),
  actual_year = NULL,
  critical_reference_year = NULL,
  actual = NULL,
  boundary = NULL,
  indicator = NULL,
  land_class = NULL,
  impact_scope = NULL,
  negative_critical = c("keep", "clamp"),
  binding = NULL,
  example = FALSE
)
```

## Arguments

- surplus:

  A
  [`calculate_n_surplus()`](https://eduaguilera.github.io/whep/reference/calculate_n_surplus.md)
  output with the grid/crop/year key. Surplus mode uses signed
  `surplus_n_t` when present, otherwise derives it from
  `surplus_kgn_ha * area_ha / 1000`. Input mode uses `n_input_std_t`.

- critical:

  A
  [`read_critical_n()`](https://eduaguilera.github.io/whep/reference/read_critical_n.md)
  critical layer. In addition to layer identity, it must carry deposited
  `source_area_ha` and `image_region`.

- land_use:

  Source land class: `"ara"`, `"igl"`, or `"all"`.

- resolution:

  Output grain: source `"cell"`, crop-cell `"grid"`, crop
  `"polity"`/`"country"`, or crop `"image_region"`.

- metric:

  Selected actual pressure: signed `"surplus"` or total agricultural
  `"input"`.

- cell_polity:

  Retained for API compatibility. IMAGE membership now comes from the
  deposited cell-key crosswalk in `critical`; country attribution comes
  from the crop rows themselves.

- allocation_scenario:

  Only source-exact `"yield_gap"` is supported. `"no_increase"` and
  `"new_fixation"` hard-error.

- actual_year:

  The actual-pressure year to compare. Must select exactly one year from
  `surplus` and is always retained in results.

- critical_reference_year:

  Must be `2010`, matching the deposited fixed reference surface, and is
  always retained in results.

- actual:

  Alias of `surplus` for the selector-oriented interface.

- boundary:

  Alias of `critical` for the selector-oriented interface.

- indicator:

  Selector-oriented pressure name: `"surplus"`, `"total_input"`, or the
  unsupported `"new_fixation"` mode (which hard-errors). When supplied,
  it overrides `metric`.

- land_class:

  Alias of `land_use`.

- impact_scope:

  Deposited impact surface: `"mi"`, `"sw"`, `"gw"`, or `"de"`. When
  supplied, it is validated against the critical layer.

- negative_critical:

  Treatment of a cell whose critical value is below zero. `"keep"`
  (default) compares the actual pressure with the deposited value as it
  is, as the source does. `"clamp"` sets it to zero (a zero allowance)
  before the cell comparison. The choice is stamped in every output row
  as `negative_critical`; see the Negative critical surplus section.

- binding:

  Optional
  [`build_critical_n_binding()`](https://eduaguilera.github.io/whep/reference/build_critical_n_binding.md)
  output for the same land-use scope. When supplied, its per-cell
  `binding_threshold` and `binding_matches_mi` are carried into the cell
  and grid results; when `NULL` (default) both columns are `NA`.

- example:

  If `TRUE`, return the package fixture.

## Value

A tibble at the requested grain. Cell results retain actual and critical
masses, signed margin, positive overshoot, coverage state, integer
source-grid key, IMAGE context, explicit years, selectors, and
provenance. `critical_kgn_ha` is the value compared (after the
`negative_critical` treatment) and `source_critical_kgn_ha` the
deposited one; the two differ only in clamped cells. Crop results
additionally retain the signed pressure share and crop-attributed
quantities, which reconcile algebraically to the cell. `exceedance_n_t`
is the crop's share of the cell overshoot `pmax(actual - critical, 0)`
and `within_boundary_n_t` is `actual_n_t - exceedance_n_t`, so the two
always sum to the actual pressure. Summed over a cell,
`within_boundary_n_t` is `min(actual, critical)`: under
`negative_critical = "keep"` it is negative wherever the critical value
is negative, and under `"clamp"` it is negative only where the actual
pressure itself is.

## Negative critical surplus

Schulte-Uebbing et al. (2022, Methods) set critical fertilizer and
manure inputs to zero where non-agricultural losses alone exceed a
threshold, but keep biological fixation and deposition in the critical
input, so their deposited critical surplus stays negative in those cells
(on the `"mi"` surface: 1,796 of 28,881 cells for `"all"`, minimum -396
kg N/ha; 2,075 of 28,573 cells for `"ara"`, minimum -317 kg N/ha).
`negative_critical = "keep"` follows the source, and it is the setting
under which the published 2010 decomposition (43 Mt N allowable plus 76
Mt N exceedance, 119 Mt N current surplus) is reproduced. `"clamp"` is a
declared departure from the source: it gives such cells a zero allowance
instead of a negative one, which lowers their overshoot to the actual
pressure and keeps the cell within-boundary mass at or above zero
wherever the actual pressure is.

## Examples

``` r
build_n_boundary_exceedance(example = TRUE)
#> # A tibble: 5 × 57
#>    year area_code polity_area_code reporting_polity_code reporting_polity_name
#>   <int>     <int>            <int> <chr>                 <chr>                
#> 1  2010         1                1 ARM-1991-2025         Armenia              
#> 2  2010         1                1 ARM-1991-2025         Armenia              
#> 3  2010         1                1 ARM-1991-2025         Armenia              
#> 4  2010         1                1 ARM-1991-2025         Armenia              
#> 5  2010         1                1 ARM-1991-2025         Armenia              
#> # ℹ 52 more variables: reporting_polity_has_geometry <lgl>, cell_id <int>,
#> #   source_row <int>, source_col <int>, lon <dbl>, lat <dbl>,
#> #   item_cbs_code <int>, actual_year <int>, critical_reference_year <int>,
#> #   area_ha <dbl>, source_area_ha <dbl>, image_region <int>,
#> #   critical_threshold <chr>, binding_threshold <chr>,
#> #   binding_matches_mi <lgl>, actual_n_t <dbl>, pressure_share <dbl>,
#> #   pressure_condition_ratio <dbl>, critical_n_t <dbl>, …
```
