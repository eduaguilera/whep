# Reconcile a level allocation against the national totals that bind

The diagnostic half of
[`allocate_level_crops()`](https://eduaguilera.github.io/whep/reference/allocate_level_crops.md).
The allocation itself decides nothing here: this reads the tables that
function returns and says, per container and per unit, how far the
administrative statistics sat from the national total they were never
allowed to move, what the discarded production series would have
implied, and where a unit's target outran the land under it.

Two things it deliberately does not do. It never re-derives a share, so
a number in this report is the number the allocator used; and it reads
the breach as the **table**
[`allocate_level_crops()`](https://eduaguilera.github.io/whep/reference/allocate_level_crops.md)
returns, never by parsing the warning the engine also emits.

## Usage

``` r
reconcile_admin_allocation(
  allocation,
  admin_shares = NULL,
  intensity = list(),
  tolerance_relative = 0.1,
  tolerance_absolute = 1000
)
```

## Arguments

- allocation:

  The list
  [`allocate_level_crops()`](https://eduaguilera.github.io/whep/reference/allocate_level_crops.md)
  returns. Its `targets`, `coverage` and `breach` elements are read; the
  others are ignored, so a stored subset of the three is a valid input.

- admin_shares:

  The resolved admin shares the allocation was given, BEFORE the
  allocator drops non-area indicators: the production rows it discards
  are what the divergence diagnostic reads. `NULL` leaves the indicator,
  production and bridge columns empty or `NA`, and a table matching no
  `(container, item)` of the allocation warns rather than degrading to
  that same empty report in silence.

- intensity:

  Named list for the cropping-intensity diagnostic, with `unit_cropland`
  (`year`, `area_code`, `level_polity_code`, `cropland_ha`, as
  [`unit_cropland_extent()`](https://eduaguilera.github.io/whep/reference/unit_cropland_extent.md)
  returns) and `mc_national` (either a single number or a tibble of
  `year`, `area_code`, `mc_factor`). Any other name is refused rather
  than ignored.

- tolerance_relative:

  Relative discrepancy above which a complete-coverage group is refused;
  `0.10` by decision T31(d).

- tolerance_absolute:

  Absolute discrepancy, in hectares or head, above which the same group
  is refused; `1000` by decision T31(d). BOTH must be breached.

## Value

A list of three tibbles:

- `groups`: one row per `(year, area_code, item_prod_code)` with the
  binding `indicator`, `national_total`, `admin_sum`,
  `n_units_reporting`, `n_units_valid`, `coverage`, `coverage_complete`,
  the allocator's `basis`, `residual_target`, `discrepancy`,
  `discrepancy_frac`, `beyond_tolerance`, `n_units_production` and
  `production_divergence_tvd`. `n_units_valid` is
  [`allocate_level_crops()`](https://eduaguilera.github.io/whep/reference/allocate_level_crops.md)'s
  own `n_units`: the units of the allocation layer valid for that year,
  which is the denominator its coverage decision – and so its choice
  between rescaling and a residual – actually used. Decision T31(b)'s
  pattern extension gives every granted unit a row even where the crop
  has no gridded pattern inside it, so a unit is not quietly dropped
  from the denominator for lacking one.

- `units`: one row per
  `(year, area_code, level_polity_code, item_prod_code)` with
  `area_share`, `production_share`, `area_share_renorm`,
  `share_divergence`, `implied_yield_ratio`, `production_basis`, the
  irrigation-floor binding and its per-unit-year count, `sown_ha`,
  `cropland_ha`, `cropping_intensity`, `mc_factor_national`,
  `intensity_exceeds_mc`, and the capacity breach at both multi-cropping
  factors.

- `bridges`: one row per `(area_code, item_prod_code)` that carried any
  year, with `n_years_observed`, `n_years_carried`, `longest_run`,
  `longest_interior_run` and the longest interior run's span and
  treatments.

## What binds and what is only measured

The national total binds and the reported area shares set the shape
within the country (decisions 2 and 8). So a difference between the
national total and the sum of the reported units is a property of the
evidence, not of the allocation: it is reported, and past both
tolerances it refuses the run, but it never moves a national total.

`discrepancy` is `national_total - admin_sum` and `discrepancy_frac`
divides it by the national total, which is `NA` where that total is zero
– the absolute hectares stay beside it, because a zero total is exactly
the case where the fraction says nothing and the level does. A group
whose fraction cannot be evaluated is never refused: refusal asks for
BOTH tolerances, and one of them has no value.

`admin_sum` is `NA`, not `0`, where no unit reported an absolute area
(`basis` is `"pattern"` or `"share_normalised"`). The sum of nothing is
zero and would read as a discrepancy of the whole national total, which
would refuse every consented shares-only family in the pin while nothing
at all had been measured against it. Hectares can only be compared with
hectares.

`admin_sum` is also the point where a failure to harmonise units becomes
visible: the readers convert to hectares, tonnes and heads before the
pin (Eurostat serves thousands of each), so a source left in its native
unit shows up here as a discrepancy three orders of magnitude wide
rather than as a quietly wrong shape.

## Refusal

A group is refused when `|discrepancy_frac| > tolerance_relative` AND
`|discrepancy| > tolerance_absolute`, and only where coverage is
complete – every unit of the layer reported an absolute area, so the two
sides are comparable. Under partial coverage the same difference is the
residual pseudo-unit's own target and is not a discrepancy at all
(decision T31(a)), so it is reported as `beyond_tolerance` and left to
the reader rather than being refused or hidden.

Both tolerances are arguments. Their defaults, 10% and 1,000 ha or head,
are decision T31(d), and
[`build_level_crop_targets()`](https://eduaguilera.github.io/whep/reference/build_level_crop_targets.md)
applies the same rule while the targets are built. The rule is restated
here so that a report assembled from stored coverage rows is gated too,
and so that a run can be re-scored at a different tolerance without
being allocated again.

## The production-implied divergence

Production never anchors (decision T31(i)): `t0` is the first observed
AREA year and a production row is dropped by the allocator. The
information is not therefore worthless, so it is reported here.

Per unit, `production_share` is the unit's share of its group's reported
production and `area_share_renorm` is the binding area share on the same
denominator: both are renormalised over the **common set**, the units
carrying both quantities, because a share over one unit set and a share
over another are not comparable. `share_divergence` is their difference,
`implied_yield_ratio` their ratio – the unit's yield over the common
set's mean yield – and `production_divergence_tvd` on the group is half
the summed absolute divergence, the fraction of the crop the two shapes
place in different units.

A unit with production and no area share has no yield ratio: the
quotient is undefined and is left `NA` rather than reported as `Inf`,
while its whole production share stands in `share_divergence`, which is
where that case is meant to be read. A common set of fewer than two
units supports no comparison at all, and every column is `NA`.

`production_share` comes from reported production values where the group
has any (`production_basis` is `"value"`), and otherwise from the
producers' own declared shares (`"declared"`), which is what a consented
shares-only family ships.

## Interior bridges

Decision T31(e) admits a LUH2 proxy bridge across an interior gap of any
length, with no refusal threshold, so the length is the only thing that
makes a 60-year bridge legible rather than merely legal.

A year of a `(container, item)` series is observed when any unit's row
that year is `treatment == "observed"`, and carried otherwise. A run of
consecutive carried years is a **bridge** when the series has an
observed year both before and after it: the leading run that a pre-seam
back-cast produces is not a bridge, and reporting the longer of the two
would claim the series was interpolated across years it was never
observed in at all. `longest_run` keeps the unrestricted maximum beside
`longest_interior_run` so the difference is visible.

## The implied cropping intensity

Per unit-year, the whole allocated harvested area over the unit's
cropland: `sown_ha / cropland_ha`, flagged where it exceeds the national
multi-cropping factor, which is the ceiling a level-0 run would have
imposed. Both inputs are supplied by the caller through `intensity` and
neither is guessed: with no extent the intensity is `NA`, and with no
factor the flag is `NA`.
[`unit_cropland_extent()`](https://eduaguilera.github.io/whep/reference/unit_cropland_extent.md)
builds the extent from the same layer and cropland the allocation ran
on, weighting each cell by `cell_area_frac` exactly as the capacity
ceiling does.

## See also

[`allocate_level_crops()`](https://eduaguilera.github.io/whep/reference/allocate_level_crops.md),
[`build_level_crop_targets()`](https://eduaguilera.github.io/whep/reference/build_level_crop_targets.md),
[`unit_cropland_extent()`](https://eduaguilera.github.io/whep/reference/unit_cropland_extent.md).

## Examples

``` r
allocation <- list(
  coverage = tibble::tibble(
    year = 2000L, area_code = 1L, item_prod_code = 15L,
    n_units = 2L, n_units_reporting = 2L, coverage = 1,
    basis = "admin_sum", national_total_ha = 250, admin_sum = 225
  ),
  targets = tibble::tibble(
    year = 2000L, area_code = 1L,
    level_polity_code = c("A1", "A2"), item_prod_code = 15L,
    share = c(0.6, 0.4), target_ha = c(150, 100),
    irrigation_clipped_ha = 0, method_crop_alloc = "admin_area_shares"
  ),
  breach = tibble::tibble(
    year = integer(), area_code = integer(),
    level_polity_code = character(), item_prod_code = integer(),
    mc_basis = character(), in_force = logical(), over_ha = numeric()
  )
)
shares <- tibble::tibble(
  year = 2000L, area_code = 1L,
  level_polity_code = c("A1", "A2"), item_prod_code = 15L,
  indicator_used = "area_harvested", value = c(135, 90),
  treatment = "observed"
)
reconcile_admin_allocation(allocation, shares)$groups
#> # A tibble: 1 × 18
#>    year area_code item_prod_code indicator n_indicators national_total admin_sum
#>   <int>     <int>          <int> <chr>            <int>          <dbl>     <dbl>
#> 1  2000         1             15 area_har…            1            250       225
#> # ℹ 11 more variables: n_units_reporting <int>, n_units_valid <int>,
#> #   coverage <dbl>, coverage_complete <lgl>, basis <chr>,
#> #   residual_target <dbl>, discrepancy <dbl>, discrepancy_frac <dbl>,
#> #   beyond_tolerance <lgl>, n_units_production <int>,
#> #   production_divergence_tvd <dbl>
```
