# Read gridded yearly LUH2 land-use-class fractions and areas.

Read the LUH2 v2h gridded land-use "states" product and aggregate its 12
subgrid states into the four carbon-balance classes (cropland,
grassland, natural, urban). Per cell-year-class the `fraction` is the
sum of the member states' grid-cell fractions (0..1); `area_ha` is that
fraction times the spherical 0.5-degree cell area. At
`resolution = "polity"` the areas are summed to each overlapping polity
via the country grid; a border cell keeps every polity it overlaps.

The states grid comes from a `WHEP_LUH2_DIR` tree when there is one,
else the reference LUH2-GCB2022 `states.nc` is downloaded on demand from
Zenodo (doi:10.5281/zenodo.15556812, CC-BY-4.0), verified against its
published MD5 and cached. Whichever is read, the vintage (the NetCDF
`source_id`, e.g. `"UofMD-landState-LUH2-GCB2022"`) is recorded on the
result with
[`attach_provenance()`](https://eduaguilera.github.io/whep/reference/attach_provenance.md),
and a local tree that is not the reference vintage warns: the base v2h
release and the annual Global Carbon Budget variants cover different
years and do not agree.

## Usage

``` r
read_luh2_landuse(
  resolution = c("grid", "polity"),
  years = NULL,
  states_source = c("auto", "local", "zenodo"),
  data = NULL,
  example = FALSE
)
```

## Source

LUH2 v2h, Hurtt, G. C. et al. (2020). Harmonization of global land use
change and management for the period 850-2100 (LUH2) for CMIP6.
Geoscientific Model Development 13, 5425-5464.
[doi:10.5194/gmd-13-5425-2020](https://doi.org/10.5194/gmd-13-5425-2020)
. The reference payload is the Global Carbon Budget vintage of that
release: Chini, L. et al. (2021). Land-use harmonization datasets for
annual global carbon budgets. Earth System Science Data 13, 4175-4189.
[doi:10.5194/essd-13-4175-2021](https://doi.org/10.5194/essd-13-4175-2021)
. Data: LUH2-GCB2022,
[doi:10.5281/zenodo.15556812](https://doi.org/10.5281/zenodo.15556812)
(CC-BY-4.0).

## Arguments

- resolution:

  `"grid"` (default, per cell and class) or `"polity"` (aggregated to
  `area_code` per year and class).

- years:

  Optional integer vector of calendar years to keep. `NULL` keeps every
  year present in the source.

- states_source:

  Which states source to read: `"auto"` (default, a `WHEP_LUH2_DIR` tree
  when present, else the Zenodo download), `"local"` (`WHEP_LUH2_DIR`
  only, an error without it) or `"zenodo"` (the checksum-verified
  reference vintage only, ignoring any local tree). Recorded in the
  provenance record's `input_origin`.

- data:

  Named list of pre-loaded inputs bypassing the readers: `states` (raw
  per-cell-year-state fractions with `lon`, `lat`, `year`, `land_use`,
  `fraction`) and `country_grid` (`lon`, `lat`, `area_code`,
  `cell_area_frac`). Each falls back to its reader when absent.

- example:

  If `TRUE`, return a small fixture instead of reading remote data.
  Defaults to `FALSE`.

## Value

A tibble with columns `lon`, `lat`, `area_code`, `year`, `land_use`,
`fraction` and `area_ha` at `"grid"` resolution; at `"polity"`
resolution `lon` and `lat` are dropped and `area_ha` is summed per
`(area_code, year, land_use)`. Both resolutions carry the polity columns
below, resolved from the `area_code` the cell grid assigns and the row's
`year`; the cell-to-area assignment itself is the static present-day
grid, which is what LUH2 has, so a pre-modern year is the present-day
cell's area read at that year. When the states grid was read from a
NetCDF, a provenance record naming the vintage is attached; read it back
with
[`get_provenance()`](https://eduaguilera.github.io/whep/reference/get_provenance.md).

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
for the reasoning.

## Examples

``` r
read_luh2_landuse(example = TRUE)
#> # A tibble: 12 × 11
#>     year area_code polity_area_code reporting_polity_code reporting_polity_name
#>    <int>     <int>            <int> <chr>                 <chr>                
#>  1  2000       203              203 ESP-1800-2025         Spain                
#>  2  2000       203              203 ESP-1800-2025         Spain                
#>  3  2000       203              203 ESP-1800-2025         Spain                
#>  4  2000       203              203 ESP-1800-2025         Spain                
#>  5  2000        79               79 DEU-1990-2025         Germany              
#>  6  2000        79               79 DEU-1990-2025         Germany              
#>  7  2000        79               79 DEU-1990-2025         Germany              
#>  8  2000        79               79 DEU-1990-2025         Germany              
#>  9  2000        11               11 AUT-1919-2025         Austria              
#> 10  2000        11               11 AUT-1919-2025         Austria              
#> 11  2000        11               11 AUT-1919-2025         Austria              
#> 12  2000        11               11 AUT-1919-2025         Austria              
#> # ℹ 6 more variables: reporting_polity_has_geometry <lgl>, lon <dbl>,
#> #   lat <dbl>, land_use <chr>, fraction <dbl>, area_ha <dbl>
```
