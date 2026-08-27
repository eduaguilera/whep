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

  - `max_iterations`, `expansion_threshold`: forwarded to the landuse
    engine.

  - `cft_target`: one of `"whep"` (default for `preset = "whep"`) or
    `"lpjml"` (default for `preset = "lpjml"`). Selects which column of
    `cft_mapping.csv` drives CFT aggregation: `cft_name` (granular
    33-class WHEP taxonomy) or `cft_lpjml` (12 LPJmL crop CFTs + single
    `others` bucket).

  - `area_key`: one of `"grid"` (default) or `"polity_area"`, forwarded
    to both engines. See
    [`build_gridded_landuse()`](https://eduaguilera.github.io/whep/reference/build_gridded_landuse.md)'s
    *Which area code the output is keyed on*.

  - `country_grid`: which cell-to-polity crosswalk the engines allocate
    into, `"centroid"` (default) or `"fraction"`. See *Which
    cell-to-polity crosswalk*.

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

Invisibly, a named list with `preset`, resolved `config`, `years`,
`out_dir`, and `output_paths`.

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

- `gridded_landuse_crops.parquet` — crop-level output.

- `gridded_landuse.parquet` — CFT-aggregated output (when
  `aggregate_to_cft = TRUE`).

- `gridded_livestock_emissions.parquet` — gridded livestock stocks and
  emissions (when livestock component selected).

- `run_metadata.yaml` — resolved preset, components, flags, years,
  timestamp, and package version.

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
#> →   Year 2000: 2 rows (alloc 0.01s, cap 0.03s)
#> # A tibble: 2 × 11
#>    year area_code polity_area_code reporting_polity_code reporting_polity_name
#>   <int>     <int>            <int> <chr>                 <chr>                
#> 1  2000         1                1 ARM-1991-2025         Armenia              
#> 2  2000         1                1 ARM-1991-2025         Armenia              
#> # ℹ 6 more variables: reporting_polity_has_geometry <lgl>, lon <dbl>,
#> #   lat <dbl>, item_prod_code <int>, rainfed_ha <dbl>, irrigated_ha <dbl>
```
