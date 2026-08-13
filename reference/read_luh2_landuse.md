# Read gridded yearly LUH2 land-use-class fractions and areas.

Read the LUH2 v2h gridded land-use "states" product and aggregate its 12
subgrid states into the four carbon-balance classes (cropland,
grassland, natural, urban). Per cell-year-class the `fraction` is the
sum of the member states' grid-cell fractions (0..1), LUH2's own share
of the **whole** cell. `area_ha` is that class's area inside one
polycell: by default the class's share of the cell's LUH2 land, spread
over the polycell's own measured land (`area_basis = "polycell_land"`),
so the four classes tile the polycell's land exactly and the carbon path
uses the same land definition as the nitrogen path. At
`resolution = "polity"` the areas are summed to each overlapping polity;
a border cell keeps every polity it overlaps.

The cell-to-polity assignment is a **static snapshot**, because LUH2
carries no territorial history: a pre-modern year is the snapshot
polity's territory holding that year's land-use composition.

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
  area_basis = c("polycell_land", "luh2_fraction"),
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

- area_basis:

  Which land definition the class areas are measured on:
  `"polycell_land"` (default) spreads each class's share of the cell's
  LUH2 land over the polycell's measured `land_area_ha`;
  `"luh2_fraction"` keeps LUH2's own land total (`fraction` times the
  spherical cell area) and splits it between the polycells of a cell by
  their share of the cell's land. Both partition the cell identically
  and differ only in the total spread (~12.78 Gha against ~12.99 Gha
  globally). The choice is recorded in the `method_land_area` output
  column.

- data:

  Named list of pre-loaded inputs bypassing the readers: `states` (raw
  per-cell-year-state fractions with `lon`, `lat`, `year`, `land_use`,
  `fraction`) and `country_grid`, the polycell support resolved to one
  row per cell and `area_code` (`lon`, `lat`, `area_code`,
  `cell_area_frac` and, for `area_basis = "polycell_land"`,
  `land_area_ha`). Each falls back to its reader when absent. A support
  carrying more than one row per cell and `area_code`, or an `NA` one,
  is refused rather than folded (DA-23).

- example:

  If `TRUE`, return a small fixture instead of reading remote data.
  Defaults to `FALSE`.

## Value

A tibble with columns `lon`, `lat`, `area_code`, `year`, `land_use`,
`fraction`, `area_ha` and `method_land_area` at `"grid"` resolution; at
`"polity"` resolution `lon` and `lat` are dropped and `area_ha` is
summed per `(area_code, year, land_use)`. `fraction` stays LUH2's share
of the whole cell and is repeated on every polycell of that cell, so
under `"polycell_land"` it is a source datum rather than a factor
`area_ha` can be recovered from. Both resolutions carry the polity
columns below, resolved from the `area_code` the support assigns and the
row's `year`; the cell-to-area assignment itself is a static snapshot,
which is what LUH2 has, so a pre-modern year is the snapshot cell's area
read at that year. When the states grid was read from a NetCDF, a
provenance record naming the vintage is attached; read it back with
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
read_luh2_landuse(example = TRUE)
#> # A tibble: 16 × 12
#>     year area_code polity_area_code reporting_polity_code reporting_polity_name
#>    <int>     <int>            <int> <chr>                 <chr>                
#>  1  2015       203              203 ESP-1800-2025         Spain                
#>  2  2015       203              203 ESP-1800-2025         Spain                
#>  3  2015       203              203 ESP-1800-2025         Spain                
#>  4  2015       203              203 ESP-1800-2025         Spain                
#>  5  2015        79               79 DEU-1990-2025         Germany              
#>  6  2015        79               79 DEU-1990-2025         Germany              
#>  7  2015        79               79 DEU-1990-2025         Germany              
#>  8  2015        79               79 DEU-1990-2025         Germany              
#>  9  2015       211              211 CHE-1800-2025         Switzerland          
#> 10  2015       211              211 CHE-1800-2025         Switzerland          
#> 11  2015       211              211 CHE-1800-2025         Switzerland          
#> 12  2015       211              211 CHE-1800-2025         Switzerland          
#> 13  2015       114              114 KEN-1963-2025         Kenya                
#> 14  2015       114              114 KEN-1963-2025         Kenya                
#> 15  2015       114              114 KEN-1963-2025         Kenya                
#> 16  2015       114              114 KEN-1963-2025         Kenya                
#> # ℹ 7 more variables: reporting_polity_has_geometry <lgl>, lon <dbl>,
#> #   lat <dbl>, land_use <chr>, fraction <dbl>, area_ha <dbl>,
#> #   method_land_area <chr>
```
