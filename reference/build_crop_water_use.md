# Build per-crop monthly applied irrigation from an LPJmL run.

Applied irrigation per cell, crop and month, from `cft_airrig_month.nc`
(first written by the 2026-09-01 v2 run), joined with each crop's stand
area fraction from `cftfrac.nc`. This is the disaggregated layer a
per-crop water footprint needs; the cell-level water budget stays with
[`build_water_balance()`](https://eduaguilera.github.io/whep/reference/build_water_balance.md),
which reports whole-cell terms.

Both unit conventions are returned side by side, because both are needed
and confusing them is the characteristic error of per-CFT data:
`airrig_stand_mm` is the depth applied per square metre of THE CROP'S
OWN STAND (an irrigation intensity), and `airrig_cell_mm` is the same
water as a whole-cell depth (`airrig_stand_mm * stand_frac`), which is
what sums – over crops it reproduces the crop-less `irrig` cube to
0.999.

Rows are crop stands present in the cell (`stand_frac > 0`), rainfed
bands included: LPJmL books paddy water on *rainfed rice* (36% of the
stand-weighted applied total at July 2010), so dropping rainfed bands
drops real water.

## Usage

``` r
build_crop_water_use(
  resolution = c("grid", "polity"),
  years = NULL,
  run_dir = NULL,
  data = list(),
  example = FALSE
)
```

## Source

LPJmL run outputs `cft_airrig_month.nc` and `cftfrac.nc`; see Schaphoff,
S. et al. (2018). LPJmL4 - a dynamic global vegetation model with
managed land - Part 1: Model description. *Geoscientific Model
Development*, 11, 1343-1375.
[doi:10.5194/gmd-11-1343-2018](https://doi.org/10.5194/gmd-11-1343-2018)
.

## Arguments

- resolution:

  `"grid"` (default, per cell) or `"polity"` (aggregated to `area_code`,
  `airrig_stand_mm` area-weighted by stand area, `airrig_cell_mm` by
  cell area).

- years:

  Integer vector of calendar years to keep. Required when the monthly
  per-CFT cube is read from a run: it is ~7.7e7 long-form rows per year,
  so `NULL` (every year the run covers) is a request no machine can
  serve and aborts rather than exhausting memory. `NULL` is fine when
  `data$airrig_month` is supplied, since the years are then taken from
  the cube itself.

- run_dir:

  Path to the LPJmL run output directory. `NULL` (default) uses
  `WHEP_LPJML_RUN_DIR`.

- data:

  Named list of pre-loaded inputs, each falling back to its reader when
  absent: `airrig_month` (per cell, band and month,
  `read_lpjml_hydrology("cft_airrig_month")` output), `stand_frac` (per
  cell, band and year,
  `read_lpjml_hydrology("stand_frac", monthly = FALSE)` output) and
  `country_grid` (the cell-polity support).

- example:

  If `TRUE`, return a small fixture instead of reading a run. Defaults
  to `FALSE`.

## Value

A tibble with `lon`, `lat`, `area_code`, `cell_area_frac`, `year`,
`month`, `band`, `band_name`, `crop_group`, `stand_frac`,
`airrig_stand_mm` and `airrig_cell_mm` at `"grid"` resolution
(aggregated over cells at `"polity"`), plus the polity columns below.

`crop_group` is the soil-carbon crop group the band's water belongs to,
in the vocabulary of
[`soc_crop_group()`](https://eduaguilera.github.io/whep/reference/soc_crop_group.md)
so the water and carbon ledgers share it: LPJmL's twelve named crops are
all herbaceous, so a band is `cropland_rainfed_herbaceous` or
`cropland_irrigated_herbaceous` by its regime. The `others` bands pool
woody crops with minor herbaceous ones and cannot be resolved to a
species, and the grassland and bioenergy bands are not cropland; those
carry `NA` rather than a guess.

A border cell shared by several polities appears ONCE PER POLITY, with
`cell_area_frac` carrying that polity's share of the cell. The mm
columns are densities and do not split; the AREA does. Aggregating over
cells therefore weights by `cell_area_frac` (times stand or cell area),
and summing rows without it double-counts every border cell.

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
build_crop_water_use(example = TRUE)
#> # A tibble: 12 × 16
#>     year area_code polity_area_code reporting_polity_code reporting_polity_name
#>    <int>     <int>            <int> <chr>                 <chr>                
#>  1  2010       203              203 ESP-1800-2025         Spain                
#>  2  2010       203              203 ESP-1800-2025         Spain                
#>  3  2010       203              203 ESP-1800-2025         Spain                
#>  4  2010       203              203 ESP-1800-2025         Spain                
#>  5  2010       203              203 ESP-1800-2025         Spain                
#>  6  2010       203              203 ESP-1800-2025         Spain                
#>  7  2010       203              203 ESP-1800-2025         Spain                
#>  8  2010       203              203 ESP-1800-2025         Spain                
#>  9  2010       203              203 ESP-1800-2025         Spain                
#> 10  2010       203              203 ESP-1800-2025         Spain                
#> 11  2010       203              203 ESP-1800-2025         Spain                
#> 12  2010       203              203 ESP-1800-2025         Spain                
#> # ℹ 11 more variables: reporting_polity_has_geometry <lgl>, lon <dbl>,
#> #   lat <dbl>, month <int>, band <int>, band_name <chr>, crop_group <chr>,
#> #   cell_area_frac <dbl>, stand_frac <dbl>, airrig_stand_mm <dbl>,
#> #   airrig_cell_mm <dbl>
```
