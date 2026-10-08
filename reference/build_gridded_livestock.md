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
  grass_productivity = NULL,
  years = NULL,
  proxy_method = c("luh2", "glw3"),
  area_key = c("grid", "polity_area"),
  polity_support = NULL
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

  - `area_code`: Country code.

  - `cell_area_frac` (or `polity_frac`, `area_frac`, `country_frac`):
    This polity compartment's share of the physical cell, a partition
    summing to 1 over the polities that overlap the cell. Required: a
    grid carrying no share is refused, because defaulting it to 1 gives
    a border cell wholly to one polity. Pass 1 only where the polity
    does own the whole cell. A land fraction (`landfrac`) is a different
    quantity and is refused rather than reinterpreted. Optional columns:

  - `polycell_id`, `cell_id`: Stable compartment/cell identifiers
    preserved in outputs when present.

  - `year` or validity intervals (`valid_from`/`valid_to`,
    `start_year`/`end_year`, `from_year`/`to_year`) for historical,
    time-varying polity overlays. The start bound is inclusive; the end
    bound is **exclusive at a succession** and **inclusive at the open
    end**, so 2014 selects `"RUS-2014-2025"` and not `"RUS-1991-2014"`,
    while 2025 still selects `"RUS-2014-2025"` because no later interval
    of that compartment follows it. See
    [polities](https://eduaguilera.github.io/whep/reference/polities.md)
    for the full rule.

  A `livestock_data` row whose `area_code` has no cell in a year, but
  whose
  [polity_area_crosswalk](https://eduaguilera.github.io/whep/reference/polity_area_crosswalk.md)
  bucket does, is folded onto that bucket for that year (and summed with
  any row already there), with a message. A bucket-keyed grid holds
  Sudan and South Sudan only as 206, while the livestock table keys them
  on 276 and 277 from 2012. A code with cells of its own is never
  folded.

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
  (Gilbert et al. 2018). Required when `proxy_method = "glw3"`; ignored,
  with a warning, under `proxy_method = "luh2"`. Expected columns:

  - `lon`, `lat`: Cell centre coordinates.

  - `species_group`: Must match `livestock_data`.

  - `density`: Heads per cell (reference year ~2010).

  - `glw_variant`: Optional, `"DA"` or `"AW"`;
    [`read_glw_density()`](https://eduaguilera.github.io/whep/reference/read_glw_density.md)
    always supplies it. A table mixing the two products is refused,
    since one recorded label cannot describe both geographies. Under
    `"glw3"` it **replaces** the LUH2 proxy for every group, still
    masked by that year's LUH2 extent so a cell whose land use has gone
    receives nothing.

- grass_productivity:

  A tibble with grass productivity per cell (`lon`, `lat`, `grass_npp`)
  from
  [`read_lpjml_grass_productivity()`](https://eduaguilera.github.io/whep/reference/read_lpjml_grass_productivity.md).
  Optional. When provided, it multiplies the `pasture`/`rangeland`
  (grazer) proxy weights so animals follow grass production rather than
  area alone; cropland/mixed proxies are unaffected. If `NULL`, area
  proxies are used alone.

- years:

  Integer vector of years to spatialize. If `NULL` (default), all years
  present in `livestock_data` are processed. When supplied,
  `livestock_data`, `gridded_pasture`, and `gridded_cropland` are
  filtered to this set before processing.

- proxy_method:

  Which spatial proxy carries the within-country weight: `"luh2"`
  (default) or `"glw3"`, validated with
  [`rlang::arg_match()`](https://rlang.r-lib.org/reference/arg_match.html).
  They are alternatives, never fallbacks: under `"glw3"` a `NULL`
  `glw_density`, or a `species_group` that table has no positive cell
  for, aborts instead of quietly reverting to the LUH2 proxy. The
  resolved value is recorded per row in `method_livestock_proxy`. See
  *Which livestock proxy the weights come from*.

- area_key:

  Which area code the output is keyed on: `"grid"` (default, the
  reporting codes `livestock_data` and `country_grid` are keyed on) or
  `"polity_area"` (the
  [polity_area_crosswalk](https://eduaguilera.github.io/whep/reference/polity_area_crosswalk.md)
  bucket national tables are aggregated on). See
  [`build_gridded_landuse()`](https://eduaguilera.github.io/whep/reference/build_gridded_landuse.md)'s
  *Which area code the output is keyed on*.

- polity_support:

  The UNFOLDED polity support, used to reconcile `livestock_data`'s
  polity vintage with a year-aware `country_grid`'s before the match is
  judged. `NULL` (default) skips the reconciliation, which is correct
  for a snapshot grid. It cannot be recovered from `country_grid`: the
  level-0 fold summarises `polity_code` away, so a grid alone cannot say
  which polity holds a cell.
  [`read_polycell_support()`](https://eduaguilera.github.io/whep/reference/read_polycell_support.md)
  returns it, and
  [`run_spatialize()`](https://eduaguilera.github.io/whep/reference/run_spatialize.md)
  passes it automatically.

## Value

A tibble with gridded livestock data. Columns:

- `lon`, `lat`: Cell centre coordinates.

- `area_code`: WHEP polity code for this cell compartment.

- `polity_area_code`, `reporting_polity_code`, `reporting_polity_name`,
  `reporting_polity_has_geometry`: Polity metadata for `area_code`.

- `grid_area_code`: Only under `area_key = "polity_area"`; the reporting
  code the engine allocated on.

- `polycell_id`, `cell_id`: Preserved when supplied in `country_grid`.

- `year`: Integer year.

- `species_group`: Livestock functional type.

- `heads`: Allocated live animal count.

- Any additional numeric columns from `livestock_data` (e.g.
  `enteric_ch4_kt`, `manure_ch4_kt`).

- `method_livestock_proxy`: Which proxy produced the weights for this
  row, one of `"luh2_area"`, `"luh2_grass"`, `"glw3_da"`, `"glw3_aw"`
  (or plain `"glw3"` for a `glw_density` table carrying no
  `glw_variant`). Constant within a `(year, species_group)` block.

## Which livestock proxy the weights come from

`proxy_method` selects the within-country weight, and every output row
records the resolved value in `method_livestock_proxy`:

- `"luh2_area"`: LUH2 extent alone (`proxy_method = "luh2"`).

- `"luh2_grass"`: LUH2 extent times grass NPP (`proxy_method = "luh2"`
  with `grass_productivity` supplied). The grass weighting reaches the
  `pasture` and `rangeland` proxies only, so `cropland` and `mixed`
  groups in the same call stay `"luh2_area"`. The label is per species
  group, not per cell: a grazer cell with no `grass_npp` keeps its area
  weight but still travels under `"luh2_grass"`, because what the column
  records is the weighting regime the group ran under.

- `"glw3_da"` / `"glw3_aw"`: GLW3 density masked by that year's LUH2
  extent (`proxy_method = "glw3"`), from the dasymetric or the
  areal-weighted product respectively. The two are different
  within-country geographies, so the label names which one: it is read
  off `glw_density`'s `glw_variant` column, which
  [`read_glw_density()`](https://eduaguilera.github.io/whep/reference/read_glw_density.md)
  stamps, and therefore cannot disagree with the raster the weights came
  from. A `glw_density` built by hand, carrying no such column, travels
  as plain `"glw3"`.

The default stays `"luh2"` even though `"glw3"` is the better-informed
proxy. GLW3 now has a data mechanism –
[`read_glw_density()`](https://eduaguilera.github.io/whep/reference/read_glw_density.md),
the `WHEP_GLW3_DIR` environment variable and
`inst/scripts/download/download_glw3.R` (whep#1000, task T15a-ii) – but
it is an opt-in local raster set, so a `"glw3"` default would abort
every run on a machine that has not fetched it, and GLW3 covers nine of
the eleven species groups (not `camels`, not `other`). This is a
deliberate, documented deviation from "the default is the most rigorous
available method", of the same shape as the interim `area_key = "grid"`
default in `R/spatialize_compartments.R`; whep#1000 task T20 is the gate
that revisits it.

## Species groups must be mapped, not guessed

Every `species_group` in `livestock_data` must have a row in
`species_proxy`; an unmapped group aborts naming it. It used to fall
back to the `"pasture"` proxy silently, so a typo or a new FAOSTAT item
was given a grazing distribution with no trace in the output.

The catch-all is explicit, not implicit.
`inst/extdata/livestock_mapping.csv` maps FAOSTAT items 1140 and 1150
(rabbits and hares, other rodents) and 1171 (live animals nes) onto the
group `"other"` with the `cropland` and `mixed` proxies, so a catch-all
group is reached by an explicit item mapping upstream and never by
name-matching here. A group carrying several proxies keeps the first, as
before.

## Which area code the output is keyed on

The chain allocates *from* a national table keyed on `area_code` and
*into* a `country_grid` keyed the same way, so both sides speak the raw
reporting vocabulary the grid was rasterized in. WHEP's polity-keyed
national tables are aggregated on `polity_area_code` instead, a bucket
that a reporting code need not equal: `276` Sudan and `277` South Sudan
both fall in bucket `206`. Every such output row therefore carries two
territorial keys that disagree, and whether a consumer joins on
`area_code` or on `polity_area_code` decides whether Sudan exists in its
result (whep#582).

`area_key` selects which of the two the output carries. It is not a
fallback: `"grid"` is the default, reproduces today's codes bit-for-bit,
and warns naming the codes that cannot join; `"polity_area"` resolves
each code to its bucket through
[polity_area_crosswalk](https://eduaguilera.github.io/whep/reference/polity_area_crosswalk.md)
before the polity columns are attached, so `area_code` and
`polity_area_code` agree in every row. It respects
`options(whep.unfold_rest_of_world)` (see
[`folded_reporting_areas()`](https://eduaguilera.github.io/whep/reference/folded_reporting_areas.md)),
so the output and the national tables agree about where a Rest-of-World
member's rows belong.

Under `"polity_area"` the raw reporting code is **carried, not
replaced**: the output gains `grid_area_code`, joined with `+` where two
reporting areas of one bucket meet in a cell and their rows collapse. So
the fold stays recoverable at the join rather than baked into the
output, the shape
[`build_cell_polity()`](https://eduaguilera.github.io/whep/reference/build_cell_polity.md)
adopted for the same reason (whep#579).

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
  ~lon,  ~lat, ~area_code, ~cell_area_frac,
   0.25, 50.25,         1L,               1,
   0.75, 50.25,         1L,               1
)
build_gridded_livestock(
  livestock_data, gridded_pasture, gridded_cropland, country_grid
)
#> ℹ Spatializing 1 groups over 1 years
#> # A tibble: 2 × 11
#>    year area_code polity_area_code reporting_polity_code reporting_polity_name
#>   <int>     <int>            <int> <chr>                 <chr>                
#> 1  2000         1                1 ARM-1991-2025         Armenia              
#> 2  2000         1                1 ARM-1991-2025         Armenia              
#> # ℹ 6 more variables: reporting_polity_has_geometry <lgl>, species_group <chr>,
#> #   lon <dbl>, lat <dbl>, heads <dbl>, method_livestock_proxy <chr>
```
