# Build a gridded soil water balance from LPJmL hydrology.

Combines LPJmL monthly hydrology outputs into an annual per-cell water
balance that closes as
`water_input_mm = aet_mm + runoff_mm + drainage_mm + soil_water_change_mm`.
Actual evapotranspiration (`aet_mm`) is the sum of the transpiration,
evaporation and interception components (LPJmL has no direct AET, PET or
temperature output). Water input is precipitation plus irrigation.
`drainage_mm` is LPJmL deep seepage, the leaching driver downstream;
`runoff_mm` is LPJmL total runoff (surface plus lateral), which leaves
the cell laterally and is not a leaching term but must appear in the
budget. Drainage defaults to native seepage; the `"residual"` method
instead reconstructs seepage as
`water_input - aet - runoff - soil_water_change`, for use only when the
seepage file is absent (it equals seepage when the balance closes).
Evapotranspiration is split into a blue (irrigation-sourced) and green
(rain-sourced) part. The result is returned per grid cell, or aggregated
to polity totals when `resolution = "polity"`.

The output also exposes the footprint-relevant terms folded into the
budget: `prec_mm` (precipitation) and `irrig_mm` (applied irrigation,
the gross blue-water volume), which satisfy
`water_input_mm = prec_mm + irrig_mm`; `blue_consump_mm` and
`green_consump_mm`, the LPJmL-native consumptive blue and green water
(the per-CFT `cft_consump_water_b` / `cft_consump_water_g` cubes when
supplied, summed over the bands `bands` selects, otherwise the blue and
green AET); and `cft_nir_mm`, the net irrigation requirement (LPJmL
`cft_nir`), the net blue-water demand, summed to cell level when
`data$cft_nir` is supplied and `NA` otherwise. Potential
evapotranspiration (`pet_mm`) comes from the CRU climate forcing that
drives the LPJmL run and is `NA` until that forcing is wired (see
`data$pet`); no PET formula is fabricated here.

## Usage

``` r
build_water_balance(
  method = list(),
  resolution = c("grid", "polity"),
  data = list(),
  bands = NULL,
  example = FALSE
)
```

## Arguments

- method:

  Named list selecting the estimation method for each term: `aet`
  (`"components"`, the only method), `drainage` (`"seepage"` default,
  LPJmL native seepage, or `"residual"`, a seepage reconstruction from
  the budget residual usable only when the seepage file is absent) and
  `blue_green` (`"cft_native"` default, per-crop blue/green consumptive
  water, or `"irrig_share"`, the irrigation share of water input).
  Members left out take their default.

- resolution:

  `"grid"` (per cell, default) or `"polity"` (aggregated to `year` and
  `area_code`).

- data:

  Optional named list of pre-loaded inputs to avoid NetCDF reads:
  hydrology tibbles `transp`, `evap`, `interc`, `prec`, `irrig`,
  `runoff` and `seepage` (each `lon`, `lat`, `year`, `value`;
  annual-summed automatically when a `month` column is present), `swc`
  (`lon`, `lat`, `year`, `month`, `layer`, `value` fractional
  saturation), optional per-crop consumptive water `cft_consump_water_b`
  and `cft_consump_water_g` (each `lon`, `lat`, `year`, `value` mm/yr),
  an optional `cft_nir` net-irrigation-requirement input (`lon`, `lat`,
  `year`, `value` mm/yr, summed to cell level when supplied; exposed as
  `cft_nir_mm`, else `NA`) and a `cell_polity` crosswalk (`lon`, `lat`,
  `area_code`, `polity_frac`, `cell_area_ha`). Each falls back to
  [`read_lpjml_hydrology()`](https://eduaguilera.github.io/whep/reference/read_lpjml_hydrology.md)
  when absent, except `cft_nir` (see Details), `pet` and the
  consumptive-water inputs. Read the latter with
  `read_lpjml_hydrology("cft_consump_water_g", monthly = FALSE)`, which
  names their CFT bands so `bands` can select among them.

- bands:

  Optional character vector of LPJmL crop-functional-type band names
  restricting which bands the per-CFT consumptive-water and net
  irrigation terms are summed over, e.g. `"rainfed grassland"` to charge
  a grazing footprint the grassland water alone. `NULL` (default) sums
  every band, the whole-cell total. Bands are matched on the `band_name`
  the file itself carries, so an unknown name aborts rather than
  silently returning the whole-cell total; the band index is never used,
  because which crop a given index denotes is a property of how the run
  was configured. Only the consumptive-water and `cft_nir` terms are
  per-CFT, so this leaves the water budget itself (AET, runoff,
  drainage) untouched.

- example:

  If `TRUE`, return a small fixture instead of reading data. Defaults to
  `FALSE`.

## Value

A tibble. For `resolution = "grid"`: `lon`, `lat`, `area_code`, `year`,
`water_input_mm`, `prec_mm`, `irrig_mm`, `pet_mm`, `aet_mm`,
`aet_blue_mm`, `aet_green_mm`, `blue_consump_mm`, `green_consump_mm`,
`cft_nir_mm`, `drainage_mm`, `runoff_mm`, `soil_water_change_mm` and
`method_water`. For `resolution = "polity"`: the same terms aggregated
to `year` and `area_code`. Both resolutions carry the polity columns
below.

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
build_water_balance(example = TRUE)
#> # A tibble: 8 × 22
#>    year area_code polity_area_code reporting_polity_code reporting_polity_name
#>   <int>     <int>            <int> <chr>                 <chr>                
#> 1  2000        11               11 AUT-1919-2025         Austria              
#> 2  2000        11               11 AUT-1919-2025         Austria              
#> 3  2000        21               21 BRA-1909-2025         Brazil               
#> 4  2000        21               21 BRA-1909-2025         Brazil               
#> 5  2000        79               79 DEU-1990-2025         Germany              
#> 6  2000        79               79 DEU-1990-2025         Germany              
#> 7  2000       203              203 ESP-1800-2025         Spain                
#> 8  2000       203              203 ESP-1800-2025         Spain                
#> # ℹ 17 more variables: reporting_polity_has_geometry <lgl>, lon <dbl>,
#> #   lat <dbl>, water_input_mm <dbl>, prec_mm <dbl>, irrig_mm <dbl>,
#> #   pet_mm <lgl>, aet_mm <dbl>, aet_blue_mm <dbl>, aet_green_mm <dbl>,
#> #   blue_consump_mm <dbl>, green_consump_mm <dbl>, cft_nir_mm <lgl>,
#> #   drainage_mm <dbl>, runoff_mm <dbl>, soil_water_change_mm <dbl>,
#> #   method_water <chr>
```
