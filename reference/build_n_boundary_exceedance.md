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
  grassland_split = c("image_density", "none"),
  grassland = list(classes = NULL, extensive_budget = NULL, critical_ara = NULL,
    critical_igl = NULL),
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

- grassland_split:

  Grassland treatment under `land_use = "all"`: `"image_density"`
  (default) splits each cell into a managed and an extensive component
  (see the section below); `"none"` compares one cell pressure with the
  deposited allowance, as before the split existed. Ignored for `"ara"`
  and `"igl"`. Under `"image_density"`, `metric = "new_fixation"`
  aborts: the archive has no extensive budget for it.

- grassland:

  Named list of the inputs the split needs, all required under
  `grassland_split = "image_density"` (their absence aborts; there is no
  fallback): `classes`, the
  [`build_grassland_intensity_classes()`](https://eduaguilera.github.io/whep/reference/build_grassland_intensity_classes.md)
  table (one row per cell and year: `cell_id`, `lon`, `lat`, `year`,
  `country_2010`, `image_region`, `a_crop_ha`, `grass_ha_image`,
  `whep_grass_ha`, `image_class_2010`, `grassland_class`,
  `method_grassland_split`), and `extensive_budget`, IMAGE's 2010
  extensive-grassland budget per cell (`cell_id`, `ext_input_kgn_ha`,
  `ext_surplus_kgn_ha`); and `critical_ara` and `critical_igl`, the
  [`read_critical_n()`](https://eduaguilera.github.io/whep/reference/read_critical_n.md)
  layers with `land_use = "ara"` and `"igl"` for the same threshold and
  metric as `critical` (validated). `critical` itself still defines the
  cell domain and is checked against the class table's areas. A cell
  absent from `classes` must carry no grassland pressure; it is compared
  as cropland only.

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
pressure itself is. Under the grassland split these hold per component
(managed, extensive) rather than per cell. Cell and grid results also
carry the split components: `managed_actual_n_t`,
`managed_critical_n_t`, `managed_positive_overshoot_n_t`,
`extensive_actual_n_t`, `extensive_critical_n_t`,
`extensive_positive_overshoot_n_t`, their areas (`managed_area_ha`,
`extensive_area_ha`), compared rates (`managed_critical_kgn_ha`,
`extensive_critical_kgn_ha`, after the `negative_critical` treatment)
and the rates before it (`source_managed_critical_kgn_ha`,
`source_extensive_critical_kgn_ha`), coverage states,
`excluded_actual_n_t` (of which `excluded_igl_actual_n_t` is intensive
grassland without an `"igl"` rate), `grassland_class`,
`method_allowance_managed`, `method_allowance_extensive` and
`method_grassland_split` (the per-cell class method, `"no_grassland"`
for a cell outside the class table, `"none"` without the split). These
are `NA` when the split is not applied, so the schema does not depend on
it. Under the split, `critical_kgn_ha` and `source_critical_kgn_ha`
remain the `"all"`-scope surface (it defines the domain and checks the
`"ara"`/`"igl"` layers); the cell allowance is the sum of the component
allowances. Grid rows add `boundary_component`
(`"managed"`/`"extensive"`, `NA` without the split); within the split,
`pressure_share` is the row's share of its component, and
`binding_threshold`/`binding_matches_mi` are `NA` on extensive rows (see
the Negative critical surplus section). Every row carries the call-level
`grassland_split`; aggregated rows list the per-cell methods they span
in `method_grassland_split`, separated by `;`.

## Grassland intensity split (`land_use = "all"`)

The deposited `all`-scope critical rate is per hectare of cropland plus
IMAGE-intensive grassland, while WHEP's pressure covers all grassland.
With `grassland_split = "image_density"` (the default) each cell is
compared as two independent components:

- **managed** – crop rows plus grassland rows (CBS 3000, 3002, 3003) of
  a cell classed intensive, against the cell's own `"ara"` critical rate
  times its IMAGE 2010 cropland plus, when the cell is classed
  intensive, an `"igl"` critical rate times its IMAGE 2010 grassland.
  The deposited per-hectare `"ara"` and `"igl"` layers combine exactly
  into the `"all"` layer (area-weighted, measured on the archive for
  every threshold and both metrics), so with the IMAGE 2010 classes this
  reproduces the `"all"`-scope allowance;

- **extensive** – grassland rows of a cell classed extensive, against
  IMAGE's 2010 extensive-grassland input or surplus per hectare
  (Schulte-Uebbing et al. 2022, SI Supplementary Table 4) times the
  cell's IMAGE 2010 grassland.

Cell overshoot is the **sum** of the two component overshoots: headroom
on one never nets against excess on the other. Cell actual, critical and
margin are the sums over the compared components. A component with no
allowance area but non-zero pressure (in practice crop pressure where
IMAGE has no cropland and the cell is extensive) is excluded from its
comparison (`managed_coverage_state`/`extensive_coverage_state`
`"zero_land"`); a component with area but no rate even after transfer is
`"missing_critical"`. Excluded pressure is reported per cell in
`excluded_actual_n_t` and in a message (classes
`whep_nbx_zero_land_component`, `whep_nbx_missing_critical_component`),
and never counts as overshoot. Crop attribution shares each component's
allowance, margin and overshoot among that component's rows only, and
reconciles to the component and to the cell. A compared component with
allowance area but no pressure row keeps its allowance in a
`cell_residual` record naming the component.

Declared assumptions (constructed methods without published precedent):
the classes are IMAGE's 2010 production-system map moved through time by
a national grazing-density proxy (see `grassland$classes`); the
extensive allowance is IMAGE's 2010 budget held constant – "no more than
in 2010", not an environmental limit, so 2010 extensive exceedance is
zero wherever WHEP's 2010 extensive pressure equals IMAGE's; a rate a
cell lacks for its class is borrowed from the nearest cell (great-circle
distance) with one in the same 2010 country, else the same IMAGE region
– an `"igl"` rate for grassland promoted from extensive to intensive, an
extensive budget rate for grassland classed extensive; cropland keeps
its own `"ara"` rate and is never lent one – stamped in
`method_allowance_managed`/`method_allowance_extensive` (`"archive"`,
`"nearest_country"`, `"nearest_region"`, `"none"`, or `"no_area"` for a
component without area; `NA` outside the critical domain). IMAGE 2010
intensive grassland with no published `"igl"` value is not lent one
(maintainer decision 2026-09-24): its pressure is left out of the
comparison and reported in `excluded_igl_actual_n_t`, the cell's
cropland is still compared at its own `"ara"` rate, and the managed
method is `"none"`. Allowance areas are IMAGE 2010 areas, fixed, except
WHEP grassland in a cell with no IMAGE grassland, whose extensive
allowance uses WHEP's own grassland area of the year (method suffix
`"_whep_area"`, maintainer decision 2026-09-24).
`grassland_split = "none"` reproduces the unsplit comparison exactly.

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

Under `grassland_split = "image_density"` the treatment applies to each
component's allowance, the unit a component is compared against: a
negative managed allowance (the cell's `"ara"` rate on its cropland plus
its `"igl"` rate on its intensive grassland, which can net against each
other as they do inside the `"all"` rate) becomes zero, and so would a
negative extensive one (IMAGE's 2010 extensive budget, which is never
negative on the deposited archive). The component rates are not clamped
one by one, so with the 2010 classes the managed comparison under
`"clamp"` matches the unsplit `"all"`-scope comparison under `"clamp"`
(to the 1 % within which the `"ara"` and `"igl"` layers combine into
`"all"`). A binding threshold names the impact that sets a critical
surplus, so it describes the managed allowance only; the extensive
allowance is a 2010 level, not a threshold, and its grid rows carry no
binding label.

## Examples

``` r
build_n_boundary_exceedance(example = TRUE)
#> # A tibble: 5 × 79
#>    year area_code polity_area_code reporting_polity_code reporting_polity_name
#>   <int>     <int>            <int> <chr>                 <chr>                
#> 1  2010         1                1 ARM-1991-2025         Armenia              
#> 2  2010         1                1 ARM-1991-2025         Armenia              
#> 3  2010         1                1 ARM-1991-2025         Armenia              
#> 4  2010         1                1 ARM-1991-2025         Armenia              
#> 5  2010         1                1 ARM-1991-2025         Armenia              
#> # ℹ 74 more variables: reporting_polity_has_geometry <lgl>, cell_id <int>,
#> #   source_row <int>, source_col <int>, lon <dbl>, lat <dbl>,
#> #   item_cbs_code <int>, actual_year <int>, critical_reference_year <int>,
#> #   area_ha <dbl>, source_area_ha <dbl>, image_region <int>,
#> #   critical_threshold <chr>, binding_threshold <chr>,
#> #   binding_matches_mi <lgl>, actual_n_t <dbl>, pressure_share <dbl>,
#> #   pressure_condition_ratio <dbl>, critical_n_t <dbl>, …
```
