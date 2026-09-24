# Build per-crop consumptive water from an LPJmL run, one row per band.

Returns the LPJmL per-crop-functional-type (CFT) consumptive blue and
green water and net irrigation requirement **per band**, rather than
summed to the cell as
[`build_water_balance()`](https://eduaguilera.github.io/whep/reference/build_water_balance.md)
reports them. Each value is the band's water expressed as a depth over
the whole cell (mm/yr): the per-stand density LPJmL writes, multiplied
by the band's stand fraction. That is the same weighting
[`build_water_balance()`](https://eduaguilera.github.io/whep/reference/build_water_balance.md)
applies before summing, so summing this output over bands per cell-year
returns its `blue_consump_mm`, `green_consump_mm` and `cft_nir_mm`
exactly.

It is the annual, consumptive counterpart of
[`build_crop_water_use()`](https://eduaguilera.github.io/whep/reference/build_crop_water_use.md),
which keeps the *applied* irrigation (`cft_airrig_month`) per crop and
month. The two read different LPJmL outputs and answer different
questions (water evaporated by a crop versus water delivered to it), and
share the band vocabulary and the `crop_group` column.

The crop dimension is LPJmL's, not WHEP's: a run distinguishes about 16
CFTs (each rainfed and irrigated), and many WHEP items share one CFT –
wheat and barley are both `temperate cereals`, and most fruit,
vegetable, fibre and stimulant crops fall into `others`. Water is
therefore resolved **per CFT band**, never per WHEP item. Mapping a band
to items (for example through `cft_mapping`) and deciding how to split
it among the items that share it are left to the caller; neither is done
here.

Two cautions carried over from the reader. On an LPJmL 6.x run without
the green/blue fix (lbm364dl/LPJmL#3) the blue/green split of the
consumptive cubes is unusable; the same data check
[`build_water_balance()`](https://eduaguilera.github.io/whep/reference/build_water_balance.md)
runs warns when rainfed bands carry blue water (class
`whep_rainfed_blue_water`), while `blue + green` stays valid. And
`cft_nir_mm` is the net irrigation *requirement*, not the gross water
applied; which of the two a footprint should charge is a methodological
choice.

## Usage

``` r
build_crop_water_consumption(
  resolution = c("grid", "polity"),
  years = NULL,
  bands = NULL,
  data = list(),
  example = FALSE
)
```

## Arguments

- resolution:

  `"grid"` (per cell and band, depths in mm/yr, default) or `"polity"`
  (per `area_code`, `year` and band, volumes in m3/yr).

- years:

  Optional integer vector of calendar years to read. `NULL` reads every
  year the files carry; each per-CFT cube is about 3 GB for a full run,
  so restrict it when reading from disk.

- bands:

  Optional character vector of band names (e.g. `"rainfed grassland"`)
  to keep. `NULL` keeps every band. Matched on the `band_name` the file
  carries, as in
  [`build_water_balance()`](https://eduaguilera.github.io/whep/reference/build_water_balance.md);
  an unknown name aborts.

- data:

  Optional named list of pre-loaded inputs, each falling back to
  [`read_lpjml_hydrology()`](https://eduaguilera.github.io/whep/reference/read_lpjml_hydrology.md)
  (restricted to `years`) when absent: `cft_consump_water_b`,
  `cft_consump_water_g` and `cft_nir` (each `lon`, `lat`, `year`, `band`
  and/or `band_name`, `value` mm/yr per stand), `stand_frac` (the same
  key, `value` the stand fraction of the cell) and, for
  `resolution = "polity"` only, the required `cell_polity` crosswalk
  (`lon`, `lat`, `area_code`, `polity_frac`, `cell_area_ha`).

- example:

  If `TRUE`, run on a small built-in fixture instead of reading data.
  Defaults to `FALSE`.

## Value

A tibble. For `resolution = "grid"`: `lon`, `lat`, `year`, `band` (when
the input carries it), `band_name`, `crop_group` (the soil-carbon crop
group of the band, as in
[`build_crop_water_use()`](https://eduaguilera.github.io/whep/reference/build_crop_water_use.md):
`NA` for the `others`, grassland and bioenergy bands), `stand_frac`, and
`blue_consump_mm`, `green_consump_mm` and `cft_nir_mm` (mm/yr over the
whole cell; `NA` where that cube does not carry the band or was not
supplied). Divide by `stand_frac` for a per-hectare-of-crop intensity.
Band rows NA in every term (cells LPJmL did not simulate) are dropped;
they carry no water, so no band sum changes. For
`resolution = "polity"`: `year`, `area_code`, the band columns,
`stand_area_ha`, and `blue_consump_m3`, `green_consump_m3` and
`cft_nir_m3`, each the depth times `polity_frac * cell_area_ha` (the
weight
[`build_water_balance()`](https://eduaguilera.github.io/whep/reference/build_water_balance.md)
uses for its polity means) summed over the polity's cells, plus the
polity columns below.

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
build_crop_water_consumption(example = TRUE)
#> # A tibble: 6 × 10
#>     lon   lat  year  band band_name        crop_group stand_frac blue_consump_mm
#>   <dbl> <dbl> <int> <int> <chr>            <chr>           <dbl>           <dbl>
#> 1  9.25  47.8  2000     1 rainfed tempera… cropland_…       0.3                0
#> 2  9.25  47.8  2000    14 rainfed grassla… NA               0.4                0
#> 3  9.25  47.8  2000    17 irrigated tempe… cropland_…       0.05               9
#> 4 -3.25  40.2  2000     1 rainfed tempera… cropland_…       0.2                0
#> 5 -3.25  40.2  2000    14 rainfed grassla… NA               0.25               0
#> 6 -3.25  40.2  2000    17 irrigated tempe… cropland_…       0.1               35
#> # ℹ 2 more variables: green_consump_mm <dbl>, cft_nir_mm <dbl>
build_crop_water_consumption(resolution = "polity", example = TRUE)
#> # A tibble: 6 × 13
#>    year area_code polity_area_code reporting_polity_code reporting_polity_name
#>   <int>     <int>            <int> <chr>                 <chr>                
#> 1  2000        79               79 DEU-1990-2025         Germany              
#> 2  2000        79               79 DEU-1990-2025         Germany              
#> 3  2000        79               79 DEU-1990-2025         Germany              
#> 4  2000       203              203 ESP-1800-2025         Spain                
#> 5  2000       203              203 ESP-1800-2025         Spain                
#> 6  2000       203              203 ESP-1800-2025         Spain                
#> # ℹ 8 more variables: reporting_polity_has_geometry <lgl>, band <int>,
#> #   band_name <chr>, crop_group <chr>, stand_area_ha <dbl>,
#> #   blue_consump_m3 <dbl>, green_consump_m3 <dbl>, cft_nir_m3 <dbl>
```
