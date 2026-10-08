# Summarise gridded nitrogen exceedance to a country-year table.

Turns the crop rows of a
[`build_n_boundary_exceedance()`](https://eduaguilera.github.io/whep/reference/build_n_boundary_exceedance.md)
grid result into one row per country and year, holding the exceedance,
the share of nitrogen inputs it represents, the share of the country's
positive surplus it represents, and the share of that surplus that lies
in comparison units above their critical surplus. Every quantity is
formed from the attributed crop rows of cells with
`coverage_state == "valid"`, so the numerator and every denominator
cover the same rows. Inputs in cells that are out of the
critical-surplus domain, lack a critical value or have no allowance area
fall outside all of them, as do rows of a grassland-split component the
comparison leaves out; rows naming no crop were removed by the
exceedance itself.

The comparison unit is the cell, or, under the grassland split
(`land_use = "all"`, `grassland_split = "image_density"`), the managed
and the extensive-grassland component of a cell, which are compared with
their own allowances and never net against each other. Under
`regime_comparison = "separate"` the unit is the rainfed or the
irrigated part of the cell or component, and `surplus` must keep its
`water_regime` rows, which are then joined regime by regime. Deficit
units never offset excess elsewhere. Two country quantities follow and
both use the country's **own** crop surplus (`actual_n_t`), never a
whole-cell surplus, which sums every polity in a shared border cell and
would count that cell once per polity:

- `positive_surplus_n_t`: the country's own crop surplus in the
  comparison units whose surplus is positive. Without the split this is
  the cells with positive whole-cell surplus; with it, the positive
  managed surplus plus the positive extensive-grassland surplus of each
  cell, so a positive managed surplus is counted even beside a negative
  extensive one;

- `exceeding_surplus_n_t`: the same, restricted to the units whose
  overshoot over their allowance is positive.

The fraction of the country's positive surplus that is excess is
`exceedance_share_of_positive_surplus = exceedance_n_t / positive_surplus_n_t`.
It is the country exceedance itself over the same positive surplus, so
it is not `beyond_share`, which counts the whole surplus of an exceeding
unit and not only the part above its allowance.

Unit membership is decided from the unit's own state (its surplus and
its overshoot), never from the crop-attributed exceedance. A unit whose
crop shares are undefined (zero or ill-conditioned total surplus)
attributes no exceedance to any crop and leaves it on a residual record,
yet its crop rows stay in `positive_surplus_n_t` and
`exceeding_surplus_n_t` when the unit is positive and exceeding, so
`beyond_share` does not depend on the attribution. The residual
exceedance stays in `unallocated_exceedance_n_t` of the diagnostics,
which also count the units and rows involved.

The country's own crop surplus in a shared unit can be negative while
the unit total is positive (the other polity carries the excess), and
crop exceedance is a signed share of the unit overshoot, so a country
ratio can fall outside `[0, 1]`: such negative own rows lower the
country's positive surplus, and its exceedance can be negative or larger
than that surplus. This affects `exceedance_share_of_positive_surplus`,
`beyond_share` and `excess_share_of_inputs`. Such rows are kept, flagged
in `ratio_outside_unit`, and counted in the diagnostics; they are never
clipped.

The world sum of `positive_surplus_n_t` equals the sum of the positive
unit surpluses of the valid cells. The world sum of `exceedance_n_t`
equals the summed cell exceedance minus the exceedance left on
unallocated residual rows, which the diagnostics report and reconcile
per year; the function aborts if they do not.

Every exceeding unit is a positive-surplus unit only with
`negative_critical = "clamp"`: a zero allowance means an overshoot needs
a positive surplus. With `"keep"` a unit with a negative critical
surplus can overshoot with a zero or negative surplus. Its exceedance
stays in `exceedance_n_t`, but the unit is outside both
`positive_surplus_n_t` and `exceeding_surplus_n_t`, so `beyond_share`
does not see it; the diagnostics report that overshoot in
`overshoot_without_surplus_n_t` (zero under the clamp).

## Usage

``` r
build_n_boundary_country(
  exceedance,
  surplus,
  ag_land,
  nourishment = NULL,
  beyond_share_cut = 0.5,
  example = FALSE
)
```

## Arguments

- exceedance:

  A
  [`build_n_boundary_exceedance()`](https://eduaguilera.github.io/whep/reference/build_n_boundary_exceedance.md)
  result at `resolution = "grid"` with `metric = "surplus"`, possibly
  bound over several years. Its `negative_critical`, `land_use`,
  `grassland_split` and `regime_comparison` stamps must each be
  constant. It may carry the grassland split.

- surplus:

  The
  [`calculate_n_surplus()`](https://eduaguilera.github.io/whep/reference/calculate_n_surplus.md)
  output the grid was computed from, carrying `lon`, `lat`, `area_code`,
  `item_cbs_code`, `year` and `n_input_std_t`. Every crop row of
  `exceedance` must find exactly one row here; a row that does not
  aborts.

- ag_land:

  A
  [`build_ag_land_support()`](https://eduaguilera.github.io/whep/reference/build_ag_land_support.md)
  table (`area_code`, `year`, `area_ha`). Its hectares are summed per
  country and year into `ag_area_ha`, WHEP's agricultural area, the
  basis of per-hectare exceedance. A country-year with no row keeps
  `NA`.

- nourishment:

  Optional
  [`normalize_nourishment()`](https://eduaguilera.github.io/whep/reference/normalize_nourishment.md)
  output (`year`, `area_code`, `nourish`), one row per country-year.
  When supplied, the result gains `nourish` and `sjos_class`, the
  country boundary side crossed with the nourishment class over the
  levels of
  [sjos_levels](https://eduaguilera.github.io/whep/reference/sjos_levels.md);
  a country-year with no boundary side or no class is `NA`.

- beyond_share_cut:

  Share of the positive surplus above which the country is on the
  `"Exceedance"` side, a number in `[0, 1)`. Defaults to `0.5`, a WHEP
  criterion.

- example:

  If `TRUE`, return a small fixture.

## Value

A named list of two tibbles.

`country`, one row per `area_code` and `year`:

- `exceedance_n_t`: country exceedance, t N (sum of the crop-attributed
  `exceedance_n_t`, signed).

- `input_std_n_t`: sum of `n_input_std_t` over the same rows, t N.

- `excess_share_of_inputs`: `exceedance_n_t / input_std_n_t`.

- `exceedance_share_of_positive_surplus`:
  `exceedance_n_t / positive_surplus_n_t`, `NA` when the denominator is
  not positive.

- `positive_surplus_n_t`, `exceeding_surplus_n_t`, `beyond_share`,
  `exceedance_share_of_positive_surplus`, `boundary_side`: see above.

- `ag_area_ha`: WHEP agricultural area, ha.

- `signed_denominator_nonpositive`: `positive_surplus_n_t` or
  `input_std_n_t` is zero or negative.

- `ratio_outside_unit`: `beyond_share`,
  `exceedance_share_of_positive_surplus` or `excess_share_of_inputs`
  lies outside `[0, 1]` (beyond a rounding tolerance of `1e-9`).

- `nourish`, `sjos_class` when `nourishment` is given.

- `negative_critical`, `land_use`, `grassland_split`,
  `regime_comparison`, `beyond_share_cut`: the run stamps.

- the polity columns below.

`diagnostics`, one row per `year`, world level: `n_countries`,
`input_std_n_t` and `all_input_n_t` (inputs in the compared rows and in
every crop row of the land-use scope), `valid_input_fraction` (their
ratio), `exceedance_n_t` (sum over countries),
`unallocated_exceedance_n_t` (exceedance left on residual rows),
`cell_exceedance_n_t` (summed cell exceedance), `exceedance_gap_n_t`
(`cell - country - unallocated`, zero up to rounding),
`positive_surplus_n_t`, `exceedance_share_of_positive_surplus` and
`excess_share_of_inputs` (world ratios), `world_ratio_outside_unit`
(either world ratio lies outside `[0, 1]`, beyond the `1e-9` tolerance),
`overshoot_without_surplus_n_t` (overshoot of units with no positive
surplus, zero under the clamp), and the counts
`n_undefined_beyond_share`, `n_undefined_exceedance_share`,
`n_undefined_excess_share`, `n_signed_denominator_nonpositive`,
`n_ratio_outside_unit`, `n_missing_ag_area`,
`n_undefined_attribution_rows` (crop rows whose attribution is
undefined), `n_unallocated_units` (units whose overshoot sits on a
residual record) and `n_unclassified` (`NA` without `nourishment`).

## Boundary side

`boundary_side` is decided after aggregation, from the country
`beyond_share = exceeding_surplus_n_t / positive_surplus_n_t`: the
country is `"Exceedance"` when more than `beyond_share_cut` of its
positive surplus lies in units above their critical surplus, and
`"Within_boundary"` otherwise. The one-half default of
`beyond_share_cut` is a WHEP criterion, not a published threshold. The
share is `NA`, and the side `NA`, when `positive_surplus_n_t` is zero or
negative: the country-year is left unclassified, flagged in
`signed_denominator_nonpositive` and counted in the diagnostics. The
labels are those
[`classify_sjos_n()`](https://eduaguilera.github.io/whep/reference/classify_sjos_n.md)
uses for its per-crop boundary side, which stays the producer-side
classification.

## Excess share of inputs

`excess_share_of_inputs = exceedance_n_t / input_std_n_t`, where
`input_std_n_t` is the sum of `n_input_std_t` (synthetic fertiliser,
manure and excreta, biological fixation, deposition and urban nitrogen)
over the same crop rows as the exceedance. It is `NA` when
`input_std_n_t` is zero or negative. The country ratio has a signed
numerator, so it is bounded by `[0, 1]` only when the country's
contributions to every cell with exceedance are non-negative; at world
level, summed over all attributed rows, it is bounded once negative
critical surpluses are clamped (`negative_critical = "clamp"`), because
a unit overshoot is then at most its positive surplus, which is at most
its inputs. The last step needs `surplus_n_t <= n_input_std_t` on every
row. That holds for `surplus_method = "harvest_removal"`, where the
surplus is the inputs minus non-negative harvest removals, but not for
`"full_balance"`: its `n_balance_t` includes soil organic matter
mineralisation, which `n_input_std_t` excludes, so the world share can
exceed one even after the clamp. Under `"keep"` a unit with a negative
critical surplus can overshoot by more than its inputs. The world ratio
outside `[0, 1]` is reported in `world_ratio_outside_unit` of the
diagnostics.

## Exceedance share of positive surplus

`exceedance_share_of_positive_surplus = exceedance_n_t / positive_surplus_n_t`,
with the exceedance signed as above. It is `NA` when
`positive_surplus_n_t` is zero or negative, exactly where `beyond_share`
is `NA`. The country ratio is guaranteed to lie in `[0, 1]` only when
negative critical surpluses are clamped and the country's contributions
to every positive unit are non-negative. At world level it is bounded
once negative critical surpluses are clamped, because a unit overshoot
is then at most its positive surplus; unlike the share of inputs, that
bound needs no assumption on the surplus method.

## Polity columns

Every area-keyed output carries the polity its `area_code` resolves to
in that row's year:

- `polity_area_code`: The numeric key rows are AGGREGATED on, for the
  matrix workflows. It is a bucket, not an identity: use
  `reporting_polity_code` to say which territory a row belongs to.

- `reporting_polity_code`: The polity itself, e.g. `ESP-1846-1914`. It
  is year-aware, so the same `area_code` resolves to different polities
  in different years, which is the point of the crosswalk.

- `reporting_polity_name`: Its name. It can differ from the area's own
  name where the area folds into an aggregate.

- `reporting_polity_has_geometry`: Whether the polity has a polygon in
  the WHEP polity database, for callers that need to map or intersect
  it. `FALSE` is a documented gap upstream, not an error.

Rows whose `area_code` resolves to no polity keep the columns with `NA`
rather than being dropped, so a gap is visible instead of silent.

Rows before the back-cast anchor year resolve to the polity live in that
anchor year rather than to the polity live in the row's own year,
because WHEP's pre-anchor series are back-cast onto the anchor-year
territory. See
[`add_polity_code()`](https://eduaguilera.github.io/whep/reference/add_polity_code.md)
for the reasoning. Where that polity is not live in the row's own year –
41.5% of the pre-1961 `(area, year)` cells –
[`add_polity_code()`](https://eduaguilera.github.io/whep/reference/add_polity_code.md)
says so as `mapping_status == "backcast_anchor"`, and
[`polity_coverage_gaps()`](https://eduaguilera.github.io/whep/reference/polity_coverage_gaps.md)
reports it as `gap_kind == "backcast_anchor"`. These columns do not say
so either way.

A row whose year no mapped period covers is resolved to the NEAREST
period of the same area instead, so `reporting_polity_code` can name a
polity that did not exist in that row's year – FAOSTAT bucket 206 "Sudan
(former)" keeps reporting after `SUD-1956-2011` ends, and its post-2011
rows carry that code. These columns do not say so:
[`add_polity_code()`](https://eduaguilera.github.io/whep/reference/add_polity_code.md)
reports such a row as `mapping_status == "out_of_span"`, and that column
is dropped here so that adding it does not change the schema of every
area-keyed output at once.
[`polity_coverage_gaps()`](https://eduaguilera.github.io/whep/reference/polity_coverage_gaps.md)
reports the stand-in rows of a built table, and
`options(whep.polity_mapping_status = "flag")` (or `"status"`) carries
the signal on the outputs themselves. Both are opt-in; the default is no
extra column.

## Examples

``` r
build_n_boundary_country(example = TRUE)
#> $country
#> # A tibble: 2 × 22
#>    year area_code polity_area_code reporting_polity_code reporting_polity_name
#>   <int>     <int>            <int> <chr>                 <chr>                
#> 1  2010         1                1 ARM-1991-2025         Armenia              
#> 2  2010         2                2 AFG-1919-2025         Afghanistan          
#> # ℹ 17 more variables: reporting_polity_has_geometry <lgl>,
#> #   exceedance_n_t <dbl>, input_std_n_t <dbl>, excess_share_of_inputs <dbl>,
#> #   positive_surplus_n_t <dbl>, exceeding_surplus_n_t <dbl>,
#> #   beyond_share <dbl>, exceedance_share_of_positive_surplus <dbl>,
#> #   boundary_side <chr>, ag_area_ha <dbl>,
#> #   signed_denominator_nonpositive <lgl>, ratio_outside_unit <lgl>,
#> #   negative_critical <chr>, land_use <chr>, grassland_split <chr>, …
#> 
#> $diagnostics
#> # A tibble: 1 × 23
#>    year n_countries input_std_n_t all_input_n_t valid_input_fraction
#>   <int>       <int>         <dbl>         <dbl>                <dbl>
#> 1  2010           2            76            76                    1
#> # ℹ 18 more variables: exceedance_n_t <dbl>, unallocated_exceedance_n_t <dbl>,
#> #   cell_exceedance_n_t <dbl>, exceedance_gap_n_t <dbl>,
#> #   positive_surplus_n_t <dbl>, exceedance_share_of_positive_surplus <dbl>,
#> #   excess_share_of_inputs <dbl>, world_ratio_outside_unit <lgl>,
#> #   overshoot_without_surplus_n_t <dbl>, n_undefined_beyond_share <int>,
#> #   n_undefined_exceedance_share <int>, n_undefined_excess_share <int>,
#> #   n_signed_denominator_nonpositive <int>, n_ratio_outside_unit <int>, …
#> 
```
