# Run the gridded land-use spatialization pipeline

Wrapper around
[`build_gridded_landuse()`](https://eduaguilera.github.io/whep/reference/build_gridded_landuse.md)
that resolves a named preset (`"lpjml"` or `"whep"`) into a consistent
bundle of input files, engine flags, and output paths. Use this to
produce two comparable outputs from the same prepared parquet inputs: an
LPJmL/LandInG-faithful run (for cell-by-cell comparison against LPJmL
inputs) and the full WHEP run (all historical years, LUH2 type-aware
allocation).

Presets can be combined with per-flag `overrides` to produce any
intermediate configuration; the resolved configuration is written next
to the outputs as `run_metadata.yaml` for traceability.

## Usage

``` r
run_spatialize(
  preset = c("lpjml", "whep"),
  years = NULL,
  components = c("landuse", "livestock"),
  overrides = list(),
  paths = list()
)
```

## Arguments

- preset:

  One of `"lpjml"` or `"whep"`. Selects a default bundle of engine flags
  and input choices. See *Presets*.

- years:

  Integer vector of years to spatialize. If `NULL`, the preset default
  is used: for `"lpjml"` a 10-year benchmark sequence
  (`seq(1850L, 2020L, by = 10L)`), intersected with the years available
  in `country_areas`; for `"whep"` all years present in `country_areas`.

- components:

  Character vector selecting which engines to run. Defaults to
  `c("landuse", "livestock")`. Pass a subset to run only one (e.g.
  `"landuse"`). Unknown entries raise an error.

- overrides:

  Named list of flags that override the preset. Unknown keys raise an
  error. Recognised entries:

  - `use_type_constraint` (logical): enable/disable LUH2 type-aware
    allocation.

  - `aggregate_to_cft` (logical, default `TRUE`): write a CFT-aggregated
    parquet alongside the crop-level output.

  - `max_iterations`: forwarded to the landuse engine.

  - `expansion_threshold`: defunct, dropped with a warning; it never
    changed the allocation (whep#1001). See
    [`build_gridded_landuse()`](https://eduaguilera.github.io/whep/reference/build_gridded_landuse.md).

  - `pattern_signal_floor`: forwarded to the landuse engine as
    `config$pattern_signal_floor`; the `harvest_fraction` below which a
    `crop_patterns` cell is float underflow rather than an allocated
    area. `0` restores the untoleranced behaviour of whep#1070.

  - `cft_target`: one of `"whep"` (default for `preset = "whep"`) or
    `"lpjml"` (default for `preset = "lpjml"`). Selects which column of
    [cft_mapping](https://eduaguilera.github.io/whep/reference/cft_mapping.md)
    drives CFT aggregation: `cft_name` (granular 33-class WHEP taxonomy)
    or `cft_lpjml` (12 LPJmL crop CFTs + single `others` bucket).

  - `area_key`: one of `"grid"` (default) or `"polity_area"`, forwarded
    to both engines. See
    [`build_gridded_landuse()`](https://eduaguilera.github.io/whep/reference/build_gridded_landuse.md)'s
    *Which area code the output is keyed on*.

  - `country_grid`: which cell-to-polity crosswalk the engines allocate
    into, `"polycell"` (default), `"centroid"` or `"fraction"`. See
    *Which cell-to-polity crosswalk*.

  - `grid_vintage`: which vintage of the polycell support the level-0
    grid is read at, `"year_aware"` (default) or `"snapshot_2015"`. See
    [`read_level_country_grid()`](https://eduaguilera.github.io/whep/reference/read_level_country_grid.md)'s
    *Which vintage of the support level 0 is read at*. The two are
    different geographies, not two precisions of one: `"snapshot_2015"`
    allocates every year of a run into the present-day cell-to-country
    map, `"year_aware"` into the map valid that year. The year-aware
    read is the default because a cell belongs to the polity that
    actually held it; the national table is reconciled onto that polity
    by `.level0_reconcile_vintage()`, which both gridded builders call
    before the match is judged: see `.grid_vintages()` and
    `validation/spatialize_grid_vintage.R`. Recorded in
    `run_metadata.yaml` and per row in `method_grid_vintage`. Not read
    under `country_grid = "centroid"` or `"fraction"`, which carry no
    validity interval at all and are recorded as `"static_crosswalk"`.

  - `level` (integer, default `0L`): containment depth the grid is
    resolved at. `0L` is today's cell-to-`area_code` grid; `1L` and
    deeper key the cells on admin units through
    [`read_level_country_grid()`](https://eduaguilera.github.io/whep/reference/read_level_country_grid.md),
    which only the `"polycell"` crosswalk supports.

  - `granted_containers` (default `NULL`): the container `area_code`s
    the run grants a depth, e.g. `110L` for Japan. **Required whenever
    `level > 0`** and refused at `level = 0`; a depth run must say whose
    depth it is, because that decides which containment edges
    [`read_level_country_grid()`](https://eduaguilera.github.io/whep/reference/read_level_country_grid.md)
    reads, which admin-share rows constrain the run, and which cells
    [`build_allocation_layer()`](https://eduaguilera.github.io/whep/reference/build_allocation_layer.md)
    takes from the deep grid rather than from level 0. See *What a
    granted depth runs*.

  - `double_claim`: which clashes the depth read's double-claim gate
    refuses, `"co_presence"` (default) or `"measured"`, forwarded to
    [`read_level_country_grid()`](https://eduaguilera.github.io/whep/reference/read_level_country_grid.md).
    The default is the fail-closed rule.

  - `output_level` (integer, default `0L`): grain of the crop output.
    `0L` sums granted-depth rows back onto the container, so
    `(lon, lat, area_code, item_prod_code, year)` stays unique and the
    schema equals a level-0 run's; a positive value returns unit-grain
    rows carrying `level_polity_code`. It may not exceed `level`.

  - `constraint_exclude` (default `NULL`): container x year ranges to
    hold out of the admin constraint, as a list named by `area_code`,
    such as `list("840" = 1961:1989)`. That is the key
    [`resolve_admin_shares()`](https://eduaguilera.github.io/whep/reference/resolve_admin_shares.md)
    reads, and it is checked here against the same rule, so a hold-out
    this run records is one the resolver can honour. Recorded in
    `run_metadata.yaml` and consumed by the admin-shares resolver; it
    does not by itself change a run that has no admin constraint wired.

  - `livestock_proxy`: one of `"luh2"` (default) or `"glw3"`, forwarded
    to
    [`build_gridded_livestock()`](https://eduaguilera.github.io/whep/reference/build_gridded_livestock.md)'s
    `proxy_method`. Under `"glw3"` the density table is read with
    [`read_glw_density()`](https://eduaguilera.github.io/whep/reference/read_glw_density.md),
    which needs a `WHEP_GLW3_DIR` tree and aborts without one; under
    `"luh2"` it is not read at all.

  - `livestock_glw_variant`: which GLW3 product a `"glw3"` run allocates
    on, `"DA"` (default, the dasymetric rasters) or `"AW"` (the
    areal-weighted ones), forwarded to
    [`read_glw_density()`](https://eduaguilera.github.io/whep/reference/read_glw_density.md)'s
    `variant`. They are different within-country geographies, so the
    resolved value is recorded twice: in `run_metadata.yaml` with the
    rest of the config, and per row in the output's
    `method_livestock_proxy` as `"glw3_da"` or `"glw3_aw"`. Ignored
    under `livestock_proxy = "luh2"`, which reads no raster.

- paths:

  Named list of filesystem paths. Recognised entries:

  - `l_files_dir`: path to the `L_files` root, for local prepared
    inputs.

  - `input_dir`: directory holding the prepared input parquets. If
    `NULL` and `l_files_dir` is unset, the pinned WHEP spatialization
    inputs are used.

  - `out_dir`: output directory. If `NULL`, defaults to
    `<l_files_dir>/whep/spatialize/<preset>` when `l_files_dir` is
    supplied, otherwise to a session temporary directory (suffixed with
    `_custom` when `overrides` is non-empty). Created if missing.

## Value

Invisibly, a named list with `preset`, `components`, `cft_target`,
resolved `config`, `years`, `out_dir`, `output_paths`, and `admin` – the
resolver's coverage report and the constraint summary at a granted
depth, `NULL` otherwise.

## Presets

- `lpjml`:

  LandInG-faithful configuration: no LUH2 type-aware allocation
  (`use_type_constraint = FALSE`) and a short default year sample suited
  to comparison against LPJmL inputs.

- `whep`:

  Full WHEP configuration: LUH2 type-aware allocation
  (`use_type_constraint = TRUE`) and the full historical year range
  present in `country_areas`.

## Inputs read from `input_dir`

Landuse (`components` contains `"landuse"`):

- `country_areas.parquet`

- `crop_patterns.parquet`

- `gridded_cropland.parquet`

- `country_grid.parquet`

- `type_cropland.parquet` (required when `use_type_constraint = TRUE`).

Livestock (`components` contains `"livestock"`):

- `livestock_country_data.parquet`

- `gridded_pasture.parquet`

- `gridded_cropland.parquet`, `country_grid.parquet`

- `manure_pattern.parquet` (optional, enables manure-intensity weighting
  if present).

- `livestock_mapping.csv` from the installed package.

- The GLW3 rasters under `WHEP_GLW3_DIR`, read only when
  `livestock_proxy = "glw3"` (see
  [`read_glw_density()`](https://eduaguilera.github.io/whep/reference/read_glw_density.md)).

## What a granted depth runs

With `level > 0` the landuse step does not call
[`build_gridded_landuse()`](https://eduaguilera.github.io/whep/reference/build_gridded_landuse.md)
on a level-0 grid. It runs the subnational chain, in this order, and
each step aborts naming what it is missing rather than continuing on the
level-0 pattern:

1.  [`read_level_country_grid()`](https://eduaguilera.github.io/whep/reference/read_level_country_grid.md)
    twice – level 0 for the ungranted countries, `level` scoped to
    `granted_containers` for the granted ones – and
    [`build_allocation_layer()`](https://eduaguilera.github.io/whep/reference/build_allocation_layer.md)
    to assert the two partition every cell. Both halves are read
    year-aware, which is what `method_grid_vintage` has always recorded
    for a depth run.

2.  [`read_admin_shares()`](https://eduaguilera.github.io/whep/reference/read_admin_shares.md),
    scoped to `granted_containers`. A granted container with no admin
    row aborts with class `whep_run_admin_container_absent`; an
    unregistered pin with `whep_run_no_admin_shares`.

3.  [`resolve_admin_units()`](https://eduaguilera.github.io/whep/reference/resolve_admin_units.md),
    turning each source's native identifier into a polity code under the
    code system that identifier belongs to. Rows resolving to nothing
    are dropped and counted; a source resolving to nothing at all aborts
    with `whep_run_admin_unresolved`, and a source whose code system is
    undeclared with `whep_run_admin_code_system`.

4.  [`resolve_admin_shares()`](https://eduaguilera.github.io/whep/reference/resolve_admin_shares.md),
    applying indicator and source precedence and honouring
    `constraint_exclude`. A constraint sharing no `level_polity_code`
    with the layer aborts with `whep_run_admin_layer_mismatch`, because
    a run whose constraint matched nothing is indistinguishable from an
    unconstrained one in every output it writes.

5.  [`allocate_level_crops()`](https://eduaguilera.github.io/whep/reference/allocate_level_crops.md),
    splitting each national total across the container's units and
    spreading each unit's target over that unit's cells.

6.  [`reconcile_admin_allocation()`](https://eduaguilera.github.io/whep/reference/reconcile_admin_allocation.md)
    and
    [`seam_gate()`](https://eduaguilera.github.io/whep/reference/seam_gate.md),
    written beside the parquets as the run's own audit trail.

What actually happened is recorded in `run_metadata.yaml` under
`admin_constraint` – the resolved, dropped and held-out row counts, the
units constrained, the `method_crop_alloc` tally over the targets, the
share basis the gate judged on, and the gate's verdict – and per row in
the targets' own `method_crop_alloc`. A level-0 run records
`admin_constraint: none`.

## Which cell-to-polity crosswalk

The producer builds two crosswalks from the same polygons. `"centroid"`
is the deployed `spatialize-country-grid` pin: one `area_code` per
0.5-degree cell, winner-take-all at a border, no share column, so a
whole border cell goes to a single polity. `"fraction"` is
`cell_polity_fraction.parquet`, which splits each border cell by
fractional coverage; the engines already read its `polity_frac` as
`cell_area_frac`, so no engine change is involved.

They are alternatives, never a fallback. The fractional parquet used to
carry a different area vocabulary from the centroid grid — it keyed
Ethiopia `62` and Sudan `206` where today's `regions.csv` uses `238` and
`276`, so substituting it dropped both countries entirely (whep#461).
Regenerating it closed that gap: the two grids now carry the same 178
area codes, it is published as the `spatialize-cell-polity-fraction` pin
so no user has to rebuild it, and
[`build_cell_polity()`](https://eduaguilera.github.io/whep/reference/build_cell_polity.md)
refuses a copy still holding a retired code instead of deleting the
countries silently (whep#694). It still cannot rescue a polity smaller
than a cell, because its producer restricts it to the cells the centroid
grid already has, and it drops 4 of those cells, whose only land is a
sliver covering the 0.5-degree cell centre but no 1/12-degree subcell
centre. Whichever is selected,
[`build_gridded_landuse()`](https://eduaguilera.github.io/whep/reference/build_gridded_landuse.md)
and
[`build_gridded_livestock()`](https://eduaguilera.github.io/whep/reference/build_gridded_livestock.md)
now warn once per call naming every reporting area the chosen grid has
no cell for and the national total at stake.

## Outputs written to `out_dir`

Every parquet below carries `method_grid_vintage`, the geography the run
allocated into: `"year_aware"`, `"snapshot_2015"` or
`"static_crosswalk"`.

- `gridded_landuse_crops.parquet` — crop-level output.

- `gridded_landuse.parquet` — CFT-aggregated output (when
  `aggregate_to_cft = TRUE`).

- `gridded_livestock_emissions.parquet` — gridded livestock stocks and
  emissions (when livestock component selected).

- `run_metadata.yaml` — resolved preset, components, flags, years,
  timestamp, package version, and the resolved `method_grid_vintage`.

- `admin_coverage.csv` — which admin source constrained each container x
  item x year, at what tier, grain and depth. Written only when
  `level > 0`, with its header and no rows where no coverage is granted.
  See
  [`admin_coverage_prototype()`](https://eduaguilera.github.io/whep/reference/admin_coverage_prototype.md).

- Eleven further CSVs, written only when `level > 0` and the landuse
  component ran: `admin_targets.csv` (one row per unit, item and year
  with its share, target and `method_crop_alloc`),
  `admin_group_coverage.csv`, `admin_conservation.csv`,
  `admin_breach.csv`, `admin_reconciliation.csv`,
  `admin_reconciliation_units.csv`, `admin_unit_cropland.csv`,
  `admin_seams.csv` and the three seam-gate tiers
  `admin_seam_gate_a.csv`, `admin_seam_gate_b.csv`,
  `admin_seam_gate_c.csv`.

## See also

[`build_gridded_landuse()`](https://eduaguilera.github.io/whep/reference/build_gridded_landuse.md).

## Examples

``` r
# Dispatch to the engine with a filtered year range (offline
# example; normally called against prepared parquet inputs).
country_areas <- tibble::tribble(
  ~year, ~area_code, ~item_prod_code, ~harvested_area_ha,
  1999L,         1L,             15L,                500,
  2000L,         1L,             15L,               1000
)
crop_patterns <- tibble::tribble(
  ~lon,  ~lat, ~item_prod_code, ~harvest_fraction,
   0.25, 50.25,             15L,               0.6,
   0.75, 50.25,             15L,               0.4
)
gridded_cropland <- tibble::tribble(
  ~lon,  ~lat,  ~year, ~cropland_ha,
   0.25, 50.25, 1999L,          800,
   0.75, 50.25, 1999L,          500,
   0.25, 50.25, 2000L,          800,
   0.75, 50.25, 2000L,          500
)
country_grid <- tibble::tribble(
  ~lon,  ~lat, ~area_code, ~cell_area_frac,
   0.25, 50.25,         1L,               1,
   0.75, 50.25,         1L,               1
)
build_gridded_landuse(
  country_areas, crop_patterns, gridded_cropland, country_grid,
  config = list(years = 2000L)
)
#> →   Year 2000: 2 rows (alloc 0.01s, cap 0s)
#> # A tibble: 2 × 11
#>    year area_code polity_area_code reporting_polity_code reporting_polity_name
#>   <int>     <int>            <int> <chr>                 <chr>                
#> 1  2000         1                1 ARM-1991-2025         Armenia              
#> 2  2000         1                1 ARM-1991-2025         Armenia              
#> # ℹ 6 more variables: reporting_polity_has_geometry <lgl>, lon <dbl>,
#> #   lat <dbl>, item_prod_code <int>, rainfed_ha <dbl>, irrigated_ha <dbl>
```
