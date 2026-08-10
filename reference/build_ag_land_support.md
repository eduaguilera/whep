# Build the gridded agricultural land support.

Assembles the physical agricultural land support that
[`build_n_inputs()`](https://eduaguilera.github.io/whep/reference/build_n_inputs.md)
allocates its non-crop-specific nitrogen terms over: per grid cell,
polity, year and CBS item, the hectares of agricultural land available
to receive nitrogen. Cropland hectares come from the LUH2-derived
`type_cropland` surface, split among crops by the static `crop_patterns`
composition (normalised within each cell, so the cell's physical
cropland area is apportioned rather than inflated by multicropping).
Grassland hectares come from
[`read_luh2_landuse()`](https://eduaguilera.github.io/whep/reference/read_luh2_landuse.md)'s
gridded grassland class and are all carried on CBS 3000, with no
intensive/extensive split inferred. Both sides are split across border
polities by the same `cell_polity` crosswalk.

Years with cropland but no grassland coverage (a grassland source that
runs short of the cropland surface, as `"luh2"` does after 2015) keep
their cropland support and raise a warning naming the affected years;
supply `data$grassland_ha` to cover them.

## Usage

``` r
build_ag_land_support(
  years = NULL,
  grassland = c("gridded_pasture", "luh2", "none"),
  polity_validity = c("keep", "flag", "drop"),
  data = list(),
  example = FALSE
)
```

## Arguments

- years:

  Optional integer vector of calendar years to keep. `NULL` (default)
  keeps every year the cropland surface covers.

- grassland:

  Grassland-support source. `"gridded_pasture"` (default) is the
  prepared per-cell `pasture_ha` + `rangeland_ha` surface, which shares
  the cropland surface's grid and 1851-2023 span. `"luh2"` reads the
  same LUH2 classes through
  [`read_luh2_landuse()`](https://eduaguilera.github.io/whep/reference/read_luh2_landuse.md)
  and agrees with it where they overlap, but stops at 2015. `"none"`
  returns cropland-only support, an explicit choice rather than a silent
  gap.

- polity_validity:

  What to do with a row whose `(area_code, year)` resolves to a polity
  that did not exist in that year (the cell-polity crosswalk has no year
  dimension, so an early-20th-century cell is labelled with its
  present-day territory). `"keep"` (default) keeps every row, which is
  the historical behaviour, and warns naming the rows, years and area
  codes involved. `"flag"` keeps them and adds the per-row logical
  `reporting_polity_out_of_span`, marking exactly which rows are
  stand-ins. `"drop"` removes them. All three warn; only `"drop"`
  changes the numbers. See
  [`polity_coverage_gaps()`](https://eduaguilera.github.io/whep/reference/polity_coverage_gaps.md),
  which reports the same rows for an already-built table.

- data:

  Optional named list of pre-loaded inputs to avoid remote reads:
  `cell_polity` (the
  [`build_cell_polity()`](https://eduaguilera.github.io/whep/reference/build_cell_polity.md)
  crosswalk), `type_cropland` (`lon`, `lat`, `year`, `luh2_type`,
  `type_ha`), `crop_patterns` (`lon`, `lat`, `item_prod_code`,
  `harvest_fraction`), `gridded_pasture` (`lon`, `lat`, `year`,
  `pasture_ha`, `rangeland_ha`), `states`
  ([`read_luh2_landuse()`](https://eduaguilera.github.io/whep/reference/read_luh2_landuse.md)'s
  raw LUH2 states) and `grassland_ha` (`lon`, `lat`, `area_code`,
  `year`, `area_ha`, bypassing the grassland read entirely). Each falls
  back to its reader when absent.

- example:

  If `TRUE`, return a small fixture instead of reading remote data.
  Defaults to `FALSE`.

## Value

A tibble with `lon`, `lat`, `area_code`, `item_cbs_code`, `year`,
`land_use` (`"cropland"` or `"grassland"`) and positive `area_ha`, plus
the polity columns below, plus `reporting_polity_out_of_span` when
`polity_validity = "flag"`.

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
build_ag_land_support(example = TRUE)
#> # A tibble: 7 × 11
#>    year area_code polity_area_code reporting_polity_code reporting_polity_name
#>   <int>     <int>            <int> <chr>                 <chr>                
#> 1  2010        10               10 AUS-1901-2025         Australia            
#> 2  2010        10               10 AUS-1901-2025         Australia            
#> 3  2010        10               10 AUS-1901-2025         Australia            
#> 4  2010        10               10 AUS-1901-2025         Australia            
#> 5  2010        10               10 AUS-1901-2025         Australia            
#> 6  2010        20               20 BWA-1966-2025         Botswana             
#> 7  2010        20               20 BWA-1966-2025         Botswana             
#> # ℹ 6 more variables: reporting_polity_has_geometry <lgl>, lon <dbl>,
#> #   lat <dbl>, item_cbs_code <int>, land_use <chr>, area_ha <dbl>
```
