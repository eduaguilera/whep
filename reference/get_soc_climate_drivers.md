# Assemble monthly SOC climate drivers from CRU climate and LPJmL hydrology.

Builds the monthly per-cell climate drivers the soil-organic-carbon
decomposition modifiers consume: air temperature, topsoil soil-water
saturation, monthly precipitation and potential evapotranspiration (the
Century modifier drivers), the monthly water-minus-potential-
evapotranspiration surplus (the RothC/HSOC driver), the annual water
balance (the AMG driver), the volumetric soil water content with its
field-capacity, wilting-point and porosity references (the ICBM moisture
drivers) and clay content. Air temperature comes from CRU TS 4.09
([`read_cru_climate()`](https://eduaguilera.github.io/whep/reference/read_cru_climate.md)
`"tmp"`, degrees Celsius); potential evapotranspiration from CRU `"pet"`
(mm/day), converted to a monthly total by multiplying by the days in the
month; the water input (precipitation plus irrigation) from the LPJmL
run so it is consistent with the hydrology that produced the soil water
content; and soil water content from LPJmL directly
([`read_lpjml_hydrology()`](https://eduaguilera.github.io/whep/reference/read_lpjml_hydrology.md)
`"swc"`, topmost layer). The soil hydraulic references (field capacity,
wilting point, porosity) come from the dominant HWSD texture class of
each cell via
[`read_soil_hydraulic()`](https://eduaguilera.github.io/whep/reference/read_soil_hydraulic.md),
and the volumetric soil water content is
`theta = swc_topsoil * porosity` (the LPJmL fractional saturation scaled
by the cell porosity). Clay content is a soil-texture covariate supplied
via `data$clay`; the polity key comes from a cell-polity crosswalk
(`data$cell_polity`).

## Usage

``` r
get_soc_climate_drivers(
  run_dir = NULL,
  years = NULL,
  data = list(),
  example = FALSE
)
```

## Arguments

- run_dir:

  Path to the LPJmL run output directory. Defaults to
  `Sys.getenv("WHEP_LPJML_RUN_DIR")` via
  [`read_lpjml_hydrology()`](https://eduaguilera.github.io/whep/reference/read_lpjml_hydrology.md).

- years:

  Optional integer vector of calendar years to keep. `NULL` keeps every
  year the inputs cover.

- data:

  Optional named list of pre-loaded inputs, each falling back to its
  reader when absent: `temp` (CRU `tmp`, `lon`, `lat`, `year`, `month`,
  `value` degrees Celsius), `pet` (CRU `pet`, same schema, mm/day),
  `prec` and `irrig` (LPJmL monthly, `lon`, `lat`, `year`, `month`,
  `value` mm/month), `swc`
  ([`read_lpjml_hydrology()`](https://eduaguilera.github.io/whep/reference/read_lpjml_hydrology.md)
  soil water content), `clay` (`lon`, `lat`, `clay_pct`, required),
  `cell_polity` (`lon`, `lat`, `area_code`, the polity crosswalk,
  required) and `soil_hydraulic` (`lon`, `lat`, `t_field`, `t_wilt`,
  `porosity`; falls back to
  [`read_soil_hydraulic()`](https://eduaguilera.github.io/whep/reference/read_soil_hydraulic.md),
  cropped to `cell_polity` when supplied).

- example:

  If `TRUE`, return a small fixture instead of reading data. Defaults to
  `FALSE`.

## Value

A tibble with `lon`, `lat`, `area_code`, `year`, `month`, `temp_c`,
`swc_topsoil`, `precip_mm` and `pet_mm` (monthly, the Century modifier
drivers), `water_minus_pet_mm` (the monthly RothC/HSOC surplus),
`water_balance_mm` (the annual sum of `water_minus_pet_mm`, the AMG
modifier driver, repeated across a cell-year's months), `clay_pct`,
`theta`, `t_field`, `t_wilt` and `porosity` (the ICBM moisture drivers:
the monthly volumetric soil water content and its static field-capacity,
wilting-point and porosity references) and `method_water_input`.

## Details

The monthly PET total is `pet_mm = pet_mm_day * days_in_month`; the
monthly water surplus is
`water_minus_pet_mm = (precip_mm + irrig_mm) - pet_mm`; and the annual
water balance `water_balance_mm` is the per-cell-year sum of that
surplus, repeated across every month of the cell-year so it can drive
the AMG modifier that expects one annual scalar per cell-year.
`precip_mm` carries precipitation only (irrigation excluded), as the
Century moisture factor expects. The volumetric soil water content
`theta = swc_topsoil * porosity` varies by month with the LPJmL
saturation, while its `t_field`, `t_wilt` and `porosity` references are
static per cell (the dominant HWSD texture class' properties), together
driving the ICBM piecewise moisture response. The water input basis is
recorded in `method_water_input` (`"lpjml_prec_irrig"`, LPJmL
precipitation plus irrigation). Air temperature (CRU) and the soil
texture products (clay, hydraulic properties) are not LPJmL outputs,
hence the mixed sources.

## Examples

``` r
get_soc_climate_drivers(example = TRUE)
#> # A tibble: 3 × 17
#>     lon   lat area_code  year month temp_c swc_topsoil precip_mm pet_mm
#>   <dbl> <dbl>     <int> <int> <int>  <dbl>       <dbl>     <dbl>  <dbl>
#> 1  9.25  47.8        11  2000     1    1.2        0.62        45     55
#> 2  9.25  47.8        11  2000     2    3.4        0.58        50     45
#> 3  9.25  47.8        11  2000     3    7.8        0.51        60     40
#> # ℹ 8 more variables: water_minus_pet_mm <dbl>, water_balance_mm <dbl>,
#> #   clay_pct <dbl>, theta <dbl>, t_field <dbl>, t_wilt <dbl>, porosity <dbl>,
#> #   method_water_input <chr>
```
