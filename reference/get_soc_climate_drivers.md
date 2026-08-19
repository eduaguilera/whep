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
  polity_validity = c("keep", "flag", "drop"),
  data = list(),
  example = FALSE
)
```

## Arguments

- run_dir:

  Path to the LPJmL run output directory. `NULL` (default) uses
  `WHEP_LPJML_RUN_DIR` when set, and the pinned `lpjml-soc-hydrology`
  artifact otherwise, so running LPJmL is not a prerequisite. That
  artifact holds only the three LPJmL monthly drivers (topsoil
  saturation, precipitation, irrigation); air temperature still comes
  from CRU and the texture products from HWSD, both downloadable, so
  neither is pinned.

- years:

  Optional integer vector of calendar years to keep. `NULL` keeps every
  year the inputs cover.

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

  `cell_polity` is used only to **label** each cell with an `area_code`
  and to restrict the grid to the cells it covers; no quantity here is
  ever multiplied by an area. It therefore decides this function's
  **footprint**, and callers that pass different crosswalks get
  different footprints from one function. The carbon path passes the
  polycell support
  ([`build_polycell_support()`](https://eduaguilera.github.io/whep/reference/build_polycell_support.md),
  via
  [`read_polycell_support()`](https://eduaguilera.github.io/whep/reference/read_polycell_support.md));
  the water path still passes
  [`build_cell_polity()`](https://eduaguilera.github.io/whep/reference/build_cell_polity.md)
  until it migrates, so the two footprints differ by the crosswalks' own
  difference until then.

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
wilting-point and porosity references) and `method_water_input`, plus
the polity columns below, plus `reporting_polity_out_of_span` when
`polity_validity = "flag"`.

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
get_soc_climate_drivers(example = TRUE)
#> # A tibble: 3 × 21
#>    year area_code polity_area_code reporting_polity_code reporting_polity_name
#>   <int>     <int>            <int> <chr>                 <chr>                
#> 1  2000        79               79 DEU-1990-2025         Germany              
#> 2  2000        79               79 DEU-1990-2025         Germany              
#> 3  2000        79               79 DEU-1990-2025         Germany              
#> # ℹ 16 more variables: reporting_polity_has_geometry <lgl>, lon <dbl>,
#> #   lat <dbl>, month <int>, temp_c <dbl>, swc_topsoil <dbl>, precip_mm <dbl>,
#> #   pet_mm <dbl>, water_minus_pet_mm <dbl>, water_balance_mm <dbl>,
#> #   clay_pct <dbl>, theta <dbl>, t_field <dbl>, t_wilt <dbl>, porosity <dbl>,
#> #   method_water_input <chr>
```
