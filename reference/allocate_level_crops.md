# Allocate national crop areas at a granted containment depth

The two-level allocation: a national total is split across the
administrative units of its container by reported harvested-**area**
shares, and each unit's target is then spread across that unit's cells
by the gridded pattern the engine already uses, under one capacity
ceiling, in ONE pass per country. Items with no statistics travel in the
same pass on pattern-implied unit shares, so a constrained country is
allocated at one grain and a cell's capacity is shared by everything in
it.

## Usage

``` r
allocate_level_crops(
  country_areas,
  crop_patterns,
  gridded_cropland,
  allocation_layer,
  admin_shares = NULL,
  config = list()
)
```

## Arguments

- country_areas:

  National crop areas, as
  [`build_gridded_landuse()`](https://eduaguilera.github.io/whep/reference/build_gridded_landuse.md)
  takes them: `year`, `area_code`, `item_prod_code`, `harvested_area_ha`
  and optionally `irrigated_area_ha`. Keyed on the CONTAINER; the unit
  split is this function's job. A negative `harvested_area_ha` is
  refused first, as `whep_alloc_negative_national`: the irrigation check
  would otherwise read it as a row with more irrigated than harvested
  area and say so.

- crop_patterns:

  Per-cell crop pattern, as
  [`build_gridded_landuse()`](https://eduaguilera.github.io/whep/reference/build_gridded_landuse.md)
  takes it.

- gridded_cropland:

  Per-cell cropland extent, as
  [`build_gridded_landuse()`](https://eduaguilera.github.io/whep/reference/build_gridded_landuse.md)
  takes it.

- allocation_layer:

  One country grid choosing a depth per country, from
  [`build_allocation_layer()`](https://eduaguilera.github.io/whep/reference/build_allocation_layer.md):
  granted countries arrive as their units, every other country as its
  level-0 row.

- admin_shares:

  Resolved admin shares – the `shares` element of
  [`resolve_admin_shares()`](https://eduaguilera.github.io/whep/reference/resolve_admin_shares.md),
  optionally completed by
  [`backcast_admin_shares()`](https://eduaguilera.github.io/whep/reference/backcast_admin_shares.md).
  `NULL` allocates every item on pattern-implied unit shares.

- config:

  Named list. Every
  [`build_gridded_landuse()`](https://eduaguilera.github.io/whep/reference/build_gridded_landuse.md)
  key is forwarded (and validated there); `pattern_extension` is fixed
  to `"granted_units"` by decision T31(b). This function's own keys are
  `tolerance_relative` (0.10), `tolerance_absolute` (1000 ha) and
  `conservation_tolerance` (1e-6, relative).

## Value

A list of seven tibbles:

- `allocation`: the gridded rows, at unit grain, exactly the
  [`build_gridded_landuse()`](https://eduaguilera.github.io/whep/reference/build_gridded_landuse.md)
  schema plus `level_polity_code`.

- `targets`: one row per
  `(year, area_code, level_polity_code, item_prod_code)` with `share`,
  `target_ha`, `irrigated_target_ha`, `rainfed_target_ha`,
  `irrigation_clipped_ha` and the regime, `method_crop_alloc`.

- `coverage`: one row per `(year, area_code, item_prod_code)` with
  `n_units`, `n_units_reporting`, `coverage`, `admin_sum`,
  `residual_target_ha`, `discrepancy_ha` and the denominator `basis`.

- `breach`: the capacity excess per
  `(year, area_code, level_polity_code, item_prod_code, mc_basis)`.

- `straddle`: per unit and item, `n_cells`, `straddle_sibling`,
  `straddle_foreign` and `cell_limited`.

- `conservation`: allocated against target, at container and unit grain.
  The container's target is the NATIONAL total, so hectares that became
  no unit's target are visible here as well as in `coverage$dropped_ha`;
  the unit rows are warned about only where their container reconciles,
  which is the sibling-absorption failure they exist to see.

- `bridges`: per `(area_code, item_prod_code, treatment)`, how many
  years of the shares were not observed and the longest contiguous run
  of them, so a 60-year LUH2 bridge is visible rather than merely legal
  (decision T31(e)).

## The composite key and where it must be carried

A granted-depth compartment is identified by
**`(area_code, level_polity_code)`**: the container's integer reporting
code, which every `area_code`-typed join,
[`add_area_name()`](https://eduaguilera.github.io/whep/reference/add_area_name.md)
and the `area_key` switch keep working on, plus the unit's polity code
from the containment edge (`NA` at level 0). The pair travels through
the whole chain, and these are the places that had to learn it:

- `.compartment_id_cols()`:

  already appends `level_polity_code` (T12), so every capacity,
  redistribution and CFT grouping keyed on it followed for free.

- The engine's national-table join and share denominators:

  `.spatialize_year()` joined `country_areas` to the grid on
  `(area_code, item_prod_code)` and formed `rf_pot_sum` / `ir_pot_sum` /
  `rainfed_sum` / `irrigated_sum` on the same pair. Both now use
  `.alloc_target_cols()`, the grain the NATIONAL TABLE is keyed at. That
  is the load-bearing distinction: the key comes from the targets, never
  from the grid, because a unit-keyed grid under a container-keyed
  national table is the pattern-implied case and must stay one national
  total.

- The LUH2 type-potential fallback:

  `type_pot` decides per group whether a crop has any of its LUH2 type
  to sit in; grouped on the container it would let one unit's type
  cropland keep another unit out of the whole-group fallback.

- `.redistribute_country_dt()`:

  `.crop_group` was `item_prod_code` alone. It is now the target grain
  inside the country, so the logit passes and the final rescale conserve
  each (unit, item). This is the grouping error the machine criterion
  pins: with the item alone, a unit whose cells are too small has its
  excess pushed across the border into a sibling, the two units' targets
  swap, and every national total still reconciles.

- `.warn_unallocated_crops()`:

  reports at the target grain, so a unit that cannot place its target is
  visible instead of being averaged into its container's success.

- The capacity ceiling:

  `.capacity_bases()` keys the unit multi-cropping factor on
  `(area_code, level_polity_code)`.

Two places do **not** carry it, for stated reasons.
`.redistribute_countries_dt()` still iterates `area_code`: it is a
memory bound on the per-country working set, and the compartment key is
inside each subset, so splitting further would only make the chunks
smaller. `.compartment_interval_groups()` stays on
`(lon, lat, area_code)` because `polycell_id` – and, at depth, the unit
code – changes at a succession, which is exactly what its
open-ended-interval test must see across.

## What binds, and what is only measured

- The national total binds. Admin statistics set the within-country
  shape and nothing else; a yield or production row never binds
  (decision T31(i)) and is dropped, counted, by
  [`build_level_crop_targets()`](https://eduaguilera.github.io/whep/reference/build_level_crop_targets.md).

- The reported share binds inside the country. Where a unit's target
  does not fit its cells, the target wins and the ceiling gives way; the
  excess is measured per unit, at both multi-cropping bases, and
  returned in `breach` (decisions T31(c), T31(g)).

- Partial coverage raises a RESIDUAL pseudo-unit carrying
  `national - admin_sum`, supported by the non-reporting units' cells
  only. With complete coverage there is no residual: the reported units
  rescale proportionally and the leftover is a diagnostic, refused only
  beyond both tolerances (decisions T31(a), T31(d)).

## See also

[`build_level_crop_targets()`](https://eduaguilera.github.io/whep/reference/build_level_crop_targets.md),
[`build_allocation_layer()`](https://eduaguilera.github.io/whep/reference/build_allocation_layer.md),
[`build_gridded_landuse()`](https://eduaguilera.github.io/whep/reference/build_gridded_landuse.md).

## Examples

``` r
# Two units of container 1 (Armenia), one cell each, one crop.
layer <- tibble::tribble(
  ~lon,  ~lat, ~area_code, ~level_polity_code, ~level, ~cell_area_frac,
  0.25, 50.25,         1L, "A-A1-1900-2100",       1L,               1,
  0.75, 50.25,         1L, "A-A2-1900-2100",       1L,               1
)
shares <- tibble::tribble(
  ~area_code, ~level_polity_code, ~item_prod_code, ~year, ~value,
          1L, "A-A1-1900-2100",               15L, 2000L,     150,
          1L, "A-A2-1900-2100",               15L, 2000L,     100
) |>
  dplyr::mutate(level = 1L, indicator_used = "area_harvested")
out <- allocate_level_crops(
  country_areas = tibble::tibble(
    year = 2000L, area_code = 1L, item_prod_code = 15L,
    harvested_area_ha = 250
  ),
  crop_patterns = tibble::tibble(
    lon = c(0.25, 0.75), lat = 50.25, item_prod_code = 15L,
    harvest_fraction = c(0.5, 0.5)
  ),
  gridded_cropland = tibble::tibble(
    lon = c(0.25, 0.75), lat = 50.25, year = 2000L,
    cropland_ha = c(1000, 1000)
  ),
  allocation_layer = layer,
  admin_shares = shares
)
#> →   Year 2000: 2 rows (alloc 0.02s, cap 0.01s)
out$targets[c("level_polity_code", "target_ha", "method_crop_alloc")]
#> # A tibble: 2 × 3
#>   level_polity_code target_ha method_crop_alloc
#>   <chr>                 <dbl> <chr>            
#> 1 A-A1-1900-2100          150 admin_area_shares
#> 2 A-A2-1900-2100          100 admin_area_shares
```
