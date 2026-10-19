# Build the IPCC 2019 wet/dry moisture regime of WHEP's crop land.

Classify each cell, polity, commodity balance item, water regime and
year of WHEP's own crop land use as IPCC 2019 wet or dry, the classes
the Tier 1 managed-soil factors are keyed on.

A zone is tropical where the mean annual temperature exceeds 18 degC and
there are at most 7 frost days per year (IPCC 2019 Refinement, Volume 4,
Chapter 3, Figure 3A.5.2, p. 3.48). A tropical row is wet where its
annual water exceeds 1000 mm; any other row is wet where its annual
water divided by potential evapotranspiration (PET) exceeds 1; every
other row is dry. Both tests are strict, so a value of exactly 1 or
exactly 1000 mm is dry (Volume 4, Chapter 11, Table 11.1, footnote 4, p.
11.13). Footnote 4 bases the disaggregation on long-term averages, so by
default the inputs are averaged over the 30 years ending in each year
before they are classified.

The water term is precipitation plus, on irrigated rows, the irrigation
depth of the row's crops (`water = "precip_irrig"`, the default), or
precipitation alone (`water = "precip"`). Rainfed rows get no
irrigation.

## Usage

``` r
build_cell_moisture_regime(
  years = NULL,
  window = c("trailing_30", "annual", "normal"),
  water = c("precip_irrig", "precip"),
  data = NULL,
  config = list(),
  example = FALSE
)
```

## Source

IPCC (2019), 2019 Refinement to the 2006 IPCC Guidelines for National
Greenhouse Gas Inventories, Volume 4: Chapter 3, Figure 3A.5.2 (p.
3.48); Chapter 11, Table 11.1, footnote 4 (p. 11.13).

CRU TS version 4.09 (files `cru_ts4.09.1901.2024.<var>.dat.nc`),
Climatic Research Unit, University of East Anglia.

## Arguments

- years:

  Integer vector of the years to classify.

- window:

  How the inputs are averaged before they are classified:
  `"trailing_30"` (default, the 30 years ending in each year),
  `"annual"` or `"normal"` (one fixed period, `config$normal_period`).

- water:

  The water term: `"precip_irrig"` (default, precipitation plus the
  irrigation depth of the row's crops and regime) or `"precip"`.

- data:

  Optional list of inputs used in place of reading them. Each element is
  optional:

  - `climate`: `lon`, `lat`, `year`, `mean_annual_temp_c`,
    `annual_precip_mm`, `annual_pet_mm`, `annual_frost_days`; its years
    are the climate record.

  - `crop_depth`: the LPJmL irrigation depth per cell, crop and year
    (`lon`, `lat`, `year`, `lpjml_crop`, `irrig_mm_irrigated`,
    `irrig_mm_rainfed`, `stand_frac_irrigated`, `stand_frac_rainfed`,
    `lpjml_version`, `lpjml_git_hash`, `lpjml_run`).

  - `lpjml_years`: the years the LPJmL run covers (default: the years of
    `crop_depth`).

  - `lpjml_grid`: the cells (`lon`, `lat`) the LPJmL run's output
    covers. Needed with `crop_depth` when an irrigated row's crop has no
    irrigated LPJmL stand in its cell; otherwise read from the run.

  - `regime_areas`: WHEP's rainfed and irrigated hectares (`lon`, `lat`,
    `area_code`, `item_prod_code`, `year`, `rainfed_ha`,
    `irrigated_ha`).

  - `country_grid`: the cell support the regime areas are built on when
    they are not supplied.

- config:

  Optional list: `cru_dir` (default `WHEP_CRU_DIR`), `run_dir` (default
  `WHEP_LPJML_RUN_DIR`, else the pinned depth layer), `normal_period`
  (default `1991:2020`) and `max_rows` (default `2e6`, the classifier
  input rows per block of cells, about 1.3 KB each at peak).

- example:

  If `TRUE`, return a small fixture instead of reading data. Defaults to
  `FALSE`.

## Value

A tibble with one row per `lon`, `lat`, `area_code`, `item_cbs_code`,
`water_regime` and `year` where WHEP has hectares of that regime,
carrying the window-mean `mat_c`, `frost_days`, `prec_mm`, `pet_mm` and
`irrig_mm`, `water_mm`, `moisture_ratio`, `tropical`, `moisture_regime`
(`"wet"`, `"dry"` or `NA`), `method_moisture_regime`, `climate_fill_km`
(the distance to the CRU cell whose climate variables a cell lacking
some took, `NA` for a cell with all of its own), `method_irrig_depth`,
`lpjml_crop` and the LPJmL vintage columns `lpjml_version`,
`lpjml_git_hash` and `lpjml_run`, plus the polity columns below.

An irrigated row's irrigation is averaged over the window years in which
WHEP irrigates that land, its climate over the whole window; a row
irrigated in fewer than all of them carries `irrig_years_<n>of<m>` in
`method_moisture_regime`. Where WHEP irrigates a crop the LPJmL run has
no irrigated stand of in the cell, in a year the run covers, the depth
is the crop's LPJmL depth averaged over the polity's cells that have the
stand, weighted by WHEP's irrigated hectares, and `method_irrig_depth`
says why: `lpjml_landuse_vintage` for a cell on the LPJmL grid (the
run's land-use forcing differs from WHEP's there) or `no_lpjml_cell` for
one outside it, with the suffix `_world_mean` where the polity has no
such stand and the world's mean is used. A message of class
`whep_moisture_regime_stand_gap` counts these rows and hectares by
cause; its `diagnosis` field lists every case.

## Details

Inputs:

- Climate comes from CRU TS: annual mean temperature, precipitation, PET
  and ground-frost days. CRU ships PET as a mean daily rate in mm per
  day, which is multiplied by the days of each month and summed over the
  year. CRU's frost count is synthetic: CRU derives it from the gridded
  minimum temperature. Years before the CRU record (1901) hold the
  1901-1930 climatology, and years after it hold the climatology of its
  last 30 years. A cell of WHEP's crop land that holds no value of a CRU
  variable in any year (all four for a cell off CRU's land mask, mostly
  coastal and island cells; fewer for a cell CRU maps for some variables
  only) takes that variable's whole record from its nearest CRU land
  cell by great-circle distance between cell centres, among the cells
  that hold it in every year read. One donor supplies every variable the
  cell lacks, and the cell keeps the variables it has; a variable with
  gaps is not lent but left to the classifier, which reports the years
  without it. The distance is `climate_fill_km`, and a message of class
  `whep_moisture_regime_nearest_climate` counts the cells by the
  variables they took. The cell's own water term, irrigation included,
  is then applied.

- Irrigation depth is LPJmL's applied irrigation per crop stand, pooled
  to the commodity balance item by WHEP's irrigated hectares. LPJmL's
  paddy water on its rainfed rice stand is not counted: rainfed rows get
  none.

## Irrigation depth stamps

`method_irrig_depth` holds the least direct method over the window years
of the row's irrigation record, one of:

- `lpjml_airrig_stand`: the item's own LPJmL crop has an irrigated stand
  in the cell.

- `lpjml_crop_proxy`: another named LPJmL crop stands in for the item,
  as mapped in `regime_yield_crop_mapping` (fodder crops, for example).

- `lpjml_others_proxy`: the item sits on LPJmL's composite "others"
  stand of minor crops.

- `lpjml_landuse_vintage`: WHEP irrigates the crop in a cell of the
  LPJmL grid where the run has no irrigated stand of it, because the
  run's land-use forcing predates WHEP's current land use. The depth is
  the crop's mean over the polity's cells that have the stand, weighted
  by WHEP's irrigated hectares.

- `no_lpjml_cell`: the same fill for a cell outside the LPJmL grid.

- `lpjml_landuse_vintage_world_mean` and `no_lpjml_cell_world_mean`: the
  two fills above where the polity has no such stand that year, so the
  world's mean over the cells that have it is used.

- `rainfed_regime`: a rainfed row, zero by definition.

- `NA`: `water = "precip"`, where there is no irrigation term.

A stamp carries two optional additions. The suffix `_recycled_climate`
follows any of the seven LPJmL-derived methods above when a window year
precedes 1901, because LPJmL recycles the 1901-1930 weather before its
forcing starts. The field `;outside_run_<n>of<m>_<fill>` follows it when
`n` of the window's `m` years lie outside the LPJmL run: those years
take the mean of the series' own window years inside the run (`<fill>`
is `series_mean`) or, where it has none, its country's mean for the
balance item (`country_mean`).

## Moisture regime stamps

`method_moisture_regime` joins these fields with `;`, in this order:

- `ipcc2019_wet_dry`: the rule.

- The window: `annual`, `trailing_30` or `normal_<first>_<last>`.

- The water term: `precip` or `precip_irrig`.

- Optional `climatology_<first>_<last>_<n>of<m>`: `n` of the window's
  `m` years lie outside the CRU record and took the held climatology of
  the years `<first>` to `<last>`.

- Optional `irrig_years_<n>of<m>`: an irrigated row whose land is
  irrigated in `n` of the window's `m` years. Its irrigation is the mean
  of those `n` years and its climate the mean of all `m`.

- Optional `nearest_cru_cell=<col>[+<col>...]`: the cell lacks these CRU
  climate variables (`mat_c`, `frost_days`, `prec_mm`, `pet_mm`) and
  took them from its nearest CRU land cell, `climate_fill_km` away; its
  other variables are its own.

- Optional, on an unclassified row (`moisture_regime` is `NA`):
  `missing=<col>[+<col>...]`, the inputs whose window mean is
  unavailable, and `pet_not_positive`, a window-mean PET of zero.
  Nothing is zero-filled.

## Errors

The function aborts, with these condition classes, when:

- `whep_moisture_regime_years`: `years` is `NULL`, empty or has a
  missing value.

- `whep_moisture_regime_config`: `config` has an unknown entry, or
  `max_rows` is not one positive number.

- `whep_moisture_regime_period`: `normal_period` is not a run of
  consecutive whole years.

- `whep_moisture_regime_columns`, `whep_moisture_regime_empty`,
  `whep_moisture_regime_key`, `whep_moisture_regime_climate`: an input
  table lacks a required column, has no rows, has a duplicated or
  missing key, or carries more than one climate for a cell-year; or no
  cell of the climate holds, in every year read, a variable that a cell
  lacks.

- `whep_moisture_regime_domain`: an input holds a negative
  precipitation, PET, frost count or irrigation depth, more than 366
  frost days, a `water_regime` other than `"rainfed"` or `"irrigated"`,
  or irrigation on a rainfed row.

- `whep_moisture_regime_lpjml_grid`: `data$crop_depth` has stand gaps
  but no `data$lpjml_grid` is supplied.

- `whep_moisture_regime_stand_mapping`: an irrigated production item has
  no LPJmL crop band.

- `whep_moisture_regime_stand_no_mean`: a crop has no irrigated LPJmL
  stand anywhere that year, so there is no depth to fill from.

- `whep_moisture_regime_outside_run`: an irrigated series-year outside
  the LPJmL run has no depth of its own or of its country to fill from.

- `whep_absent_input`: precipitation, PET, the irrigated hectares or the
  irrigation depth of the irrigated rows is absent, missing or all zero.

- `whep_lpjml_irrig_depth_*`: the LPJmL irrigation depth layer rejects
  the regime areas, the run or its output cubes, or the pinned layer.

Messages of class `whep_moisture_regime_unclassified`,
`whep_moisture_regime_area_years` and
`whep_moisture_regime_nearest_climate` report rows left without a
regime, window years the regime areas do not cover and cells that took
climate variables from their nearest CRU cell.

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
build_cell_moisture_regime(example = TRUE)
#> # A tibble: 10 × 26
#>     year area_code polity_area_code reporting_polity_code reporting_polity_name
#>    <int>     <int>            <int> <chr>                 <chr>                
#>  1  2009        79               79 DEU-1990-2025         Germany              
#>  2  2009        79               79 DEU-1990-2025         Germany              
#>  3  2009        79               79 DEU-1990-2025         Germany              
#>  4  2009        79               79 DEU-1990-2025         Germany              
#>  5  2009       211              211 CHE-1800-2025         Switzerland          
#>  6  2009        79               79 DEU-1990-2025         Germany              
#>  7  2010        79               79 DEU-1990-2025         Germany              
#>  8  2010        79               79 DEU-1990-2025         Germany              
#>  9  2010        79               79 DEU-1990-2025         Germany              
#> 10  2010       211              211 CHE-1800-2025         Switzerland          
#> # ℹ 21 more variables: reporting_polity_has_geometry <lgl>, lon <dbl>,
#> #   lat <dbl>, item_cbs_code <int>, water_regime <chr>, mat_c <dbl>,
#> #   frost_days <dbl>, prec_mm <dbl>, pet_mm <dbl>, irrig_mm <dbl>,
#> #   water_mm <dbl>, moisture_ratio <dbl>, tropical <lgl>,
#> #   moisture_regime <chr>, method_moisture_regime <chr>, climate_fill_km <dbl>,
#> #   method_irrig_depth <chr>, lpjml_crop <chr>, lpjml_version <chr>,
#> #   lpjml_git_hash <chr>, lpjml_run <chr>
```
