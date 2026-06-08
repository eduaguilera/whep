# Build gridded landuse dataset

Disaggregate country-level FAOSTAT crop harvested areas to a 0.5-degree
grid. This reproduces the core spatialization workflow of the
[LandInG](https://github.com/PIK-LPJmL/LandInG) toolbox, adapted to WHEP
conventions and tidy data structures.

The algorithm follows three main steps:

1.  Each crop's country total is distributed to grid cells
    proportionally to a spatial reference pattern (e.g. Monfreda)
    weighted by gridded cropland extent (e.g. LUH2/HYDE).

2.  If total allocated harvested area in any cell exceeds its capacity
    (cropland times multi-cropping suitability), excess is iteratively
    redistributed using a logit-based transformation.

3.  Individual crops are aggregated into crop functional types (CFTs).

## Usage

``` r
build_gridded_landuse(
  country_areas,
  crop_patterns,
  gridded_cropland,
  country_grid,
  config = list()
)
```

## Arguments

- country_areas:

  A tibble with country-level crop harvested areas. Expected columns:

  - `year`: Integer year.

  - `area_code`: Country code (numeric, matching WHEP polities).

  - `item_prod_code`: FAOSTAT item code for the crop.

  - `harvested_area_ha`: Total harvested area in hectares.

  - `irrigated_area_ha`: Irrigated harvested area in hectares (optional,
    defaults to 0).

- crop_patterns:

  A tibble with per-cell spatial crop patterns. Expected columns:

  - `lon`: Longitude of cell centre.

  - `lat`: Latitude of cell centre.

  - `item_prod_code`: FAOSTAT item code.

  - `harvest_fraction`: Cropping intensity (Monfreda area divided by
    reference cropland).

- gridded_cropland:

  A tibble with per-cell cropland extent. Expected columns:

  - `lon`: Longitude of cell centre.

  - `lat`: Latitude of cell centre.

  - `year`: Integer year.

  - `cropland_ha`: Total cropland area in hectares.

  - `irrigated_ha`: Irrigated cropland in hectares (optional, defaults
    to 0).

- country_grid:

  A tibble mapping grid cells to countries. Expected columns:

  - `lon`: Longitude of cell centre.

  - `lat`: Latitude of cell centre.

  - `area_code`: Country code. Optional columns:

  - `cell_area_frac` (or `area_frac`): Fraction of the physical cell
    belonging to this polity compartment. Defaults to 1.

  - `polycell_id`, `cell_id`: Stable compartment/cell identifiers
    preserved in outputs when present.

  - `year` or validity intervals (`valid_from`/`valid_to`,
    `start_year`/`end_year`, `from_year`/`to_year`) for historical,
    time-varying polity overlays.

- config:

  Named list of optional extras. Unknown keys raise an error. Recognised
  keys:

  - `years`: Integer vector of years to spatialize. If `NULL` (default),
    all years present in `country_areas` are processed. When supplied,
    `country_areas`, `gridded_cropland`, and `type_cropland` are
    filtered to this set before processing.

  - `cft_mapping`: A tibble mapping FAOSTAT items to CFT names
    (`item_prod_code`, `cft_name`). If `NULL`, no CFT aggregation is
    performed and individual crop results are returned.

  - `type_cropland`: A tibble with per-cell, per-year, per-type cropland
    (`lon`, `lat`, `year`, `luh2_type`, `type_ha`, `type_irrig_ha`).
    When provided alongside `type_mapping`, each crop is allocated only
    into cells containing its LUH2 type. If `NULL`, falls back to total
    cropland.

  - `type_mapping`: A tibble (`item_prod_code`, `luh2_type`) that maps
    each crop to its LUH2 type. If `NULL`, type-aware allocation is
    disabled even when `type_cropland` is provided.

  - `multicropping`: A tibble with per-cell multi-cropping suitability
    factors. Required columns: `lon`, `lat`, `mc_rainfed`,
    `mc_irrigated`. An optional `year` column keys factors to year (one
    row per cell per year); when present, the table is filtered to the
    current year before the capacity constraint is applied. When absent,
    the table is treated as a static spatial layer applied to every
    year. If `NULL` (default), the capacity constraint still runs with
    `mc_rainfed = mc_irrigated = 1` (harvested area capped at physical
    cropland).

  - `max_iterations`: Maximum iterations for the redistribution loop.
    Default: `1000L`.

  - `expansion_threshold`: Iteration number after which crops are
    allowed to expand into cells without an existing pattern. Default:
    `100L`.

## Value

A tibble with gridded crop (or CFT) harvested areas. Columns:

- `lon`, `lat`: Cell centre coordinates.

- `year`: Integer year.

- `area_code`: WHEP polity code for this cell compartment.

- `polycell_id`, `cell_id`: Preserved when supplied in `country_grid`.

- `crop_name` or `cft_name`: Crop or CFT identifier.

- `rainfed_ha`: Rainfed harvested area in the cell.

- `irrigated_ha`: Irrigated harvested area in the cell.

## Methodology

This function reimplements the spatial crop allocation from the LandInG
toolbox (Ostberg et al. 2023, doi:10.5194/gmd-16-3375-2023) with the
following extensions:

- LUH2 crop-functional-type constraints (`type_cropland` +
  `type_mapping` parameters) restrict each crop to cells containing its
  LUH2 type (c3ann, c4ann, c3per, c3nfx). LandInG allocates to total
  cropland without type constraints.

- MIRCA2000 crop-specific irrigated fractions (Portmann et al. 2010) for
  irrigation distribution, falling back to LUH2-proportional allocation.

## Data sources

- Country areas: FAOSTAT QCL via
  [`build_primary_production`](https://eduaguilera.github.io/whep/reference/build_primary_production.md)

- Crop patterns: EarthStat / Monfreda et al. (2008)

- Gridded cropland: LUH2 v2h (Hurtt et al. 2020)

- Irrigation: MIRCA2000 (Portmann et al. 2010) + LUH2

## Examples

``` r
# Minimal example with toy data
country_areas <- tibble::tribble(
  ~year, ~area_code, ~item_prod_code, ~harvested_area_ha,
  2000L, 1L, 15L, 1000
)
crop_patterns <- tibble::tribble(
  ~lon, ~lat, ~item_prod_code, ~harvest_fraction,
  0.25, 50.25, 15L, 0.6,
  0.75, 50.25, 15L, 0.4
)
gridded_cropland <- tibble::tribble(
  ~lon, ~lat, ~year, ~cropland_ha,
  0.25, 50.25, 2000L, 800,
  0.75, 50.25, 2000L, 500
)
country_grid <- tibble::tribble(
  ~lon, ~lat, ~area_code,
  0.25, 50.25, 1L,
  0.75, 50.25, 1L
)
build_gridded_landuse(
  country_areas, crop_patterns, gridded_cropland, country_grid,
  config = list(years = 2000L)
)
#> →   Year 2000: 2 rows (alloc 0.01s, cap 0.01s)
#> # A tibble: 2 × 7
#>    year area_code   lon   lat item_prod_code rainfed_ha irrigated_ha
#>   <int>     <int> <dbl> <dbl>          <int>      <dbl>        <dbl>
#> 1  2000         1  0.25  50.2             15       706.            0
#> 2  2000         1  0.75  50.2             15       294.            0
```
