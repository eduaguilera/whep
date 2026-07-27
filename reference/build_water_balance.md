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
(the per-CFT `cft_consump_water_b` / `cft_consump_water_g` totals when
supplied, otherwise the blue and green AET); and `cft_nir_mm`, the net
irrigation requirement (LPJmL `cft_nir`), the net blue-water demand,
summed to cell level when `data$cft_nir` is supplied and `NA` otherwise.
Potential evapotranspiration (`pet_mm`) comes from the CRU climate
forcing that drives the LPJmL run and is `NA` until that forcing is
wired (see `data$pet`); no PET formula is fabricated here.

## Usage

``` r
build_water_balance(
  method = list(),
  resolution = c("grid", "polity"),
  data = list(),
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
  consumptive-water inputs.

- example:

  If `TRUE`, return a small fixture instead of reading data. Defaults to
  `FALSE`.

## Value

A tibble. For `resolution = "grid"`: `lon`, `lat`, `area_code`, `year`,
`water_input_mm`, `prec_mm`, `irrig_mm`, `pet_mm`, `aet_mm`,
`aet_blue_mm`, `aet_green_mm`, `blue_consump_mm`, `green_consump_mm`,
`cft_nir_mm`, `drainage_mm`, `runoff_mm`, `soil_water_change_mm` and
`method_water`. For `resolution = "polity"`: the same terms aggregated
to `year` and `area_code`.

## Examples

``` r
build_water_balance(example = TRUE)
#> # A tibble: 8 × 18
#>      lon    lat area_code  year water_input_mm prec_mm irrig_mm pet_mm aet_mm
#>    <dbl>  <dbl>     <int> <int>          <dbl>   <dbl>    <dbl> <lgl>   <dbl>
#> 1   9.25  47.8         11  2000           1200     950      250 NA        800
#> 2   9.75  47.8         11  2000           1100     880      220 NA        760
#> 3 -55.2  -12.2         21  2000           1800    1300      500 NA       1300
#> 4 -55.8  -12.2         21  2000           1750    1270      480 NA       1260
#> 5  35.8   -1.25        79  2000            900     720      180 NA        650
#> 6  35.2   -1.25        79  2000            950     760      190 NA        690
#> 7  -3.75  40.2        203  2000            600     500      100 NA        420
#> 8  -3.25  40.2        203  2000            650     540      110 NA        460
#> # ℹ 9 more variables: aet_blue_mm <dbl>, aet_green_mm <dbl>,
#> #   blue_consump_mm <dbl>, green_consump_mm <dbl>, cft_nir_mm <lgl>,
#> #   drainage_mm <dbl>, runoff_mm <dbl>, soil_water_change_mm <dbl>,
#> #   method_water <chr>
```
