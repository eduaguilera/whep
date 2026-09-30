# Split a national crop total across the units of its container

The first half of
[`allocate_level_crops()`](https://eduaguilera.github.io/whep/reference/allocate_level_crops.md),
exposed on its own because the reconciliation diagnostics read it: it
turns national totals plus resolved admin shares into one target per
`(year, container, unit, item)`, and says on every row which regime
produced it.

## Usage

``` r
build_level_crop_targets(
  country_areas,
  unit_weights,
  admin_shares = NULL,
  tolerance_relative = 0.1,
  tolerance_absolute = 1000
)
```

## Arguments

- country_areas:

  National crop areas keyed on the container; see
  [`allocate_level_crops()`](https://eduaguilera.github.io/whep/reference/allocate_level_crops.md).

- unit_weights:

  Per-unit pattern weights, from `.alloc_unit_weights()`: `year`,
  `area_code`, `level_polity_code`, `item_prod_code`, `weight_rainfed`,
  `weight_irrigated`, `cropland_rainfed_ha`, `cropland_irrigated_ha`,
  `n_cells`.

- admin_shares:

  Resolved admin shares, or `NULL`.

- tolerance_relative:

  Relative discrepancy above which a complete-coverage group is refused,
  `0.10` by decision T31(d).

- tolerance_absolute:

  Absolute discrepancy, in hectares, above which the same group is
  refused, `1000` by decision T31(d). BOTH must be breached.

## Value

A list of two tibbles, `targets` and `coverage`; see
[`allocate_level_crops()`](https://eduaguilera.github.io/whep/reference/allocate_level_crops.md)
for their columns.

## How the denominator is chosen

Per `(year, area_code, item_prod_code)`, writing `D` for the denominator
each unit's reported value is divided by:

- `"pattern"`:

  No unit reports. Every unit's share is its own pattern weight over the
  group's, `method_crop_alloc = "pattern_implied"` – or
  `"pattern_national"` where the container is at level 0 and is
  therefore its own single unit.

- `"admin_sum"`:

  Every unit of the layer reports a value: complete coverage, so
  `D = admin_sum` and the reported units rescale proportionally onto the
  national total. The leftover is a diagnostic; it is refused only when
  it breaks BOTH tolerances (decision T31(d)).

- `"residual"`:

  Some units report a value: `D = max(admin_sum, national)`, so a
  reported value binds in absolute hectares while `admin_sum` is below
  the national total, and rescales down when it is above. The residual,
  `1 - admin_sum / D` of the total, is split across the units that
  reported NO absolute area, and never touches a unit that did (decision
  T31(a)). Those units' pattern weights split it – unless every one of
  them declares a share, in which case the declared shares do, because
  that is what a declared share states.

- `"share_normalised"`:

  The rows carry a `share` and no `value`, which is what
  [`backcast_admin_shares()`](https://eduaguilera.github.io/whep/reference/backcast_admin_shares.md)
  produces: the shares already sum to 1 over the units they cover, so
  they are used as they stand and a unit outside them gets zero.
  `method_crop_alloc = "admin_backcast_luh2"`.

`method_crop_alloc` therefore takes one of `"pattern_national"` (a
container the layer holds at level 0, which is its own single unit),
`"pattern_implied"`, `"admin_area_shares"`, `"admin_backcast_luh2"`,
`"admin_residual"` (a unit carrying its share of the residual on the
weights that split it – whether because it reported nothing, or because
it declared a share the split did not run on) and `"unallocated"` (a
unit whose target is 0: the shares do not cover it and no residual
reaches it, or the group had neither pattern weight nor cropland for
anything to be split on, `coverage$weight_basis == "none"`). The column
names what placed the hectares, so a unit whose declared share was
displaced by the pattern weights is `"admin_residual"` and not
`"admin_area_shares"`, and a group the pattern never ran on is
`"unallocated"` and not `"pattern_implied"`.

A group whose reported values are all zero while the national total is
positive cannot set a shape. Under complete coverage every share is 0,
the hectares are counted in `coverage$dropped_ha`, and the T31(d)
refusal fires on any group where that matters (the discrepancy is then
100% of the national total). Where that leaves NO positive target at
all,
[`allocate_level_crops()`](https://eduaguilera.github.io/whep/reference/allocate_level_crops.md)
returns the empty allocation beside this coverage rather than calling an
engine with nothing to place.

An admin-share row carrying no `level_polity_code` names no unit – that
is what
[`resolve_admin_units()`](https://eduaguilera.github.io/whep/reference/resolve_admin_units.md)
leaves on a unit it could not resolve – and is dropped, counted and
named before the split. `NA` is the layer's own value for "this
container is at level 0 and IS its own unit", so such a row would
otherwise bind the container's own row and publish one unresolved
province as the country's complete subnational evidence.

## What counts as a report

One predicate. A unit reports when it carries a `value`, a `share`, or
both, and `coverage$n_units_reporting` counts exactly the units that
then bind. `reports_value` – an ABSOLUTE area – is a narrower question,
and it decides two things only: which denominator the group uses, and
which units the residual is spread over. So a unit declaring a share and
no value is a reporter, and it takes a slice of the residual the valued
units left. What sizes that slice is a property of the candidate SET,
not of the unit: where every residual candidate declares a share, those
shares split the residual (`coverage$weight_basis` is then `"declared"`)
and the declared share binds; where only some do, the pattern weights
split it, because a declared fraction and a hectare of potential are not
the same quantity. In that second case the unit is allocated exactly as
a silent one is, and `method_crop_alloc` says so.

A negative reported value, a negative declared share and a negative
derived target are all refused (classes `whep_alloc_negative_value` and
`whep_alloc_negative_target`);
[`allocate_level_crops()`](https://eduaguilera.github.io/whep/reference/allocate_level_crops.md)
refuses a negative national total ahead of both, as
`whep_alloc_negative_national`. A negative target is dropped by the
engine's positivity filter, after which the remaining units divide the
whole national total between them: the container over-allocates and
nothing downstream can see the row that is not there.

## Irrigation

The unit's irrigated target is the national irrigated area split by the
engine's own `ir_potential` aggregated per unit, so the two steps
compose; where a group has no irrigated potential the split falls to the
units' irrigated cropland, and where there is none of that either the
irrigated area is unplaceable and is reported as such, because the
engine would drop it too. The rainfed target is the remainder, and the
irrigated target is clipped at the unit's area target so the remainder
is never negative – the engine computes `harvested - irrigated` with no
clipping and lets a negative row through its output filter. Clipped
hectares are reported per unit in `irrigation_clipped_ha` and are NOT
redistributed to units with room: that would be an allocation rule
nothing has decided.

## Examples

``` r
weights <- tibble::tibble(
  year = 2000L,
  area_code = 1L,
  level_polity_code = c("A1", "A2"),
  item_prod_code = 15L,
  weight_rainfed = c(400, 600),
  weight_irrigated = 0,
  cropland_rainfed_ha = c(800, 1200),
  cropland_irrigated_ha = 0,
  n_cells = 1L
)
national <- tibble::tibble(
  year = 2000L, area_code = 1L, item_prod_code = 15L,
  harvested_area_ha = 1000
)
build_level_crop_targets(national, weights)$targets
#> # A tibble: 2 × 12
#>    year area_code level_polity_code item_prod_code share target_ha
#>   <int>     <int> <chr>                      <int> <dbl>     <dbl>
#> 1  2000         1 A1                            15   0.4       400
#> 2  2000         1 A2                            15   0.6       600
#> # ℹ 6 more variables: irrigated_target_ha <dbl>, rainfed_target_ha <dbl>,
#> #   irrigation_clipped_ha <dbl>, method_crop_alloc <chr>,
#> #   national_total_ha <dbl>, n_cells <int>
```
