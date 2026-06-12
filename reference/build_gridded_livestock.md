# Build gridded livestock dataset

Disaggregate country-level FAOSTAT livestock stocks and emissions to a
0.5-degree grid. Each species group uses a tailored spatial proxy:

- **Ruminants** (cattle, buffalo, sheep/goats, equines): LUH2 managed
  pasture (`pastr`) plus rangeland (`range`), optionally weighted by a
  static manure-intensity reference (West et al. 2014).

- **Confined animals** (pigs, poultry): LUH2 aggregate cropland,
  reflecting intensive farming co-location with crop production.

- **Range specialists** (camels): LUH2 rangeland only.

- **Mixed** (other animals): 50/50 blend of pasture and cropland.

For each country, year, and species group the function distributes the
national total proportionally to cell-level proxy weights:

\$\$\text{cell} = \frac{w_i}{\sum\_{j \in \text{country}} w_j} \times
T\_{\text{country}}\$\$

where \\w_i\\ is the proxy weight in cell \\i\\ (land-use hectares times
optional reference-pattern intensity) and \\T\\ is the country total
(heads or emissions).

### Methodology

Livestock spatialization is not covered by LandInG (Ostberg et al.
2023), which focuses on crops only. The approach here extends the
LandInG framework by using the same LUH2-based spatial proxies (pasture,
rangeland, cropland) for livestock distribution.

Country-level data comes from
[`build_primary_production()`](https://eduaguilera.github.io/whep/reference/build_primary_production.md)
(stocks) and the `faostat-emissions-livestock` pin (CH4/N2O emissions),
with predecessor redistribution and pre-1961 backfill already applied.

The Zenodo livestock density input (Heinke 2025,
doi:10.5281/zenodo.14946695) provides an alternative calibrated LSU/ha
reference for use with the `glw_density` parameter.

### Data sources and references

|  |  |
|----|----|
| Source | Use |
| FAOSTAT Production_Livestock (FAO 2024) | Country-level heads |
| FAOSTAT Emissions_livestock (FAO 2024) | Enteric CH4, manure CH4/N2O |
| LUH2 v2h (Hurtt et al. 2020) | Time-varying pasture + cropland |
| West et al. (2014) | Static manure-N intensity reference |
| GLW3 (Gilbert et al. 2018) | Species-specific density (optional) |
| Heinke (2025) | Calibrated LSU/ha density (optional) |
| IPCC 2006/2019 | N-excretion rates, emission factors |

## Usage

``` r
build_gridded_livestock(
  livestock_data,
  gridded_pasture,
  gridded_cropland,
  country_grid,
  species_proxy = NULL,
  manure_pattern = NULL,
  glw_density = NULL,
  years = NULL
)
```

## Arguments

- livestock_data:

  A tibble with country-level livestock data. Required columns:

  - `year`: Integer year.

  - `area_code`: Country code (WHEP polities).

  - `species_group`: Livestock functional-type name (e.g. `"cattle"`,
    `"pigs"`, `"poultry"`).

  - `heads`: Live animal count (number of head). Any additional numeric
    columns (e.g. `enteric_ch4_kt`, `manure_ch4_kt`, `manure_n2o_kt`,
    `manure_n_mg`) are distributed to the grid using the same
    proportional weights as `heads`.

- gridded_pasture:

  A tibble with annual gridded pasture extent. Required columns:

  - `lon`, `lat`: Cell centre coordinates (0.5 degree).

  - `year`: Integer year.

  - `pasture_ha`: Managed pasture area in hectares (LUH2 `pastr`).

  - `rangeland_ha`: Rangeland area in hectares (LUH2 `range`).

- gridded_cropland:

  A tibble with annual gridded cropland extent. Required columns:

  - `lon`, `lat`: Cell centre coordinates.

  - `year`: Integer year.

  - `cropland_ha`: Total cropland area in hectares.

- country_grid:

  A tibble mapping grid cells to countries. Required columns:

  - `lon`, `lat`: Cell centre coordinates.

  - `area_code`: Country code. Optional columns:

  - `cell_area_frac` (or `area_frac`): Fraction of the physical cell
    belonging to this polity compartment. Defaults to 1.

  - `polycell_id`, `cell_id`: Stable compartment/cell identifiers
    preserved in outputs when present.

  - `year` or validity intervals (`valid_from`/`valid_to`,
    `start_year`/`end_year`, `from_year`/`to_year`) for historical,
    time-varying polity overlays.

- species_proxy:

  A tibble mapping each `species_group` to its spatial proxy type:
  `"pasture"`, `"cropland"`, `"rangeland"`, or `"mixed"`. Required
  columns:

  - `species_group`: Group name (must match `livestock_data`).

  - `spatial_proxy`: One of `"pasture"`, `"cropland"`, `"rangeland"`, or
    `"mixed"`. If `NULL`, a default mapping is used (see Details).

- manure_pattern:

  A tibble with static manure-intensity weights (e.g. from West et al.
  2014). Optional. Expected columns:

  - `lon`, `lat`: Cell centre coordinates.

  - `manure_intensity`: Relative intensity (kg N per ha or similar).
    Values are used multiplicatively with the land-use proxy. If `NULL`,
    land-use weights are used alone.

- glw_density:

  A tibble with species-specific gridded livestock density from GLW3
  (Gilbert et al. 2018). Optional. Expected columns:

  - `lon`, `lat`: Cell centre coordinates.

  - `species_group`: Must match `livestock_data`.

  - `density`: Heads per cell (reference year ~2010). If provided, this
    **replaces** the LUH2-based proxy for the matching groups, while
    still being scaled by LUH2 time trends. If `NULL`, LUH2 proxies are
    used for all groups.

- years:

  Integer vector of years to spatialize. If `NULL` (default), all years
  present in `livestock_data` are processed. When supplied,
  `livestock_data`, `gridded_pasture`, and `gridded_cropland` are
  filtered to this set before processing.

## Value

A tibble with gridded livestock data. Columns:

- `lon`, `lat`: Cell centre coordinates.

- `area_code`: WHEP polity code for this cell compartment.

- `polity_area_code`, `reporting_polity_code`, `reporting_polity_name`,
  `reporting_polity_has_geometry`: Polity metadata for `area_code`.

- `polycell_id`, `cell_id`: Preserved when supplied in `country_grid`.

- `year`: Integer year.

- `species_group`: Livestock functional type.

- `heads`: Allocated live animal count.

- Any additional numeric columns from `livestock_data` (e.g.
  `enteric_ch4_kt`, `manure_ch4_kt`).

## Examples

``` r
# Minimal example with toy data
livestock_data <- tibble::tribble(
  ~year, ~area_code, ~species_group, ~heads,
  2000L,         1L,       "cattle",   5000
)
gridded_pasture <- tibble::tribble(
  ~lon,  ~lat,  ~year, ~pasture_ha, ~rangeland_ha,
   0.25, 50.25, 2000L,         600,           200,
   0.75, 50.25, 2000L,         400,           100
)
gridded_cropland <- tibble::tribble(
  ~lon,  ~lat,  ~year, ~cropland_ha,
   0.25, 50.25, 2000L,          800,
   0.75, 50.25, 2000L,          500
)
country_grid <- tibble::tribble(
  ~lon,  ~lat, ~area_code,
   0.25, 50.25,         1L,
   0.75, 50.25,         1L
)
build_gridded_livestock(
  livestock_data, gridded_pasture, gridded_cropland, country_grid
)
#> ℹ Spatializing 1 groups over 1 years
#> # A tibble: 2 × 10
#>    year area_code polity_area_code reporting_polity_code reporting_polity_name
#>   <int>     <int>            <int> <chr>                 <chr>                
#> 1  2000         1                1 ARM-1991-2025         Armenia              
#> 2  2000         1                1 ARM-1991-2025         Armenia              
#> # ℹ 5 more variables: reporting_polity_has_geometry <lgl>, species_group <chr>,
#> #   lon <dbl>, lat <dbl>, heads <dbl>
```
