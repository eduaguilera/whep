# Build the pathway-mode critical-nitrogen boundary exceedance.

Compares a
[`build_nitrogen_balance()`](https://eduaguilera.github.io/whep/reference/build_nitrogen_balance.md)
output's process-based nitrogen losses against the medium-specific
critical loads of Schulte-Uebbing et al. (2022), per grid cell and per
crop (Mode B). The air medium compares field ammonia emission
(`nh3_n_t`, converted to a per-hectare rate) to the critical ammonia
emission; the water medium compares nitrate loss (`no3_n_t`) to the
tighter (lower) of the critical groundwater leaching and critical
surface-water load, recording which sub-medium binds. Each medium is
split into an `exceedance` part (above the load) and a `within` part
with the shared exceed-share formula, as a per-hectare intensity and a
mass, so `exceedance + within == actual` per medium. `binding_boundary`
names the medium (`"air"`, `"water"`, `"both"` on an exact positive tie,
or `"none"`) with the highest exceedance share. `resolution = "grid"`
keeps the full per-crop grid key and per-medium columns; `"polity"` and
`"country"` sum the mass terms over cells to `area_code`,
`item_cbs_code`, `year`. Climate / nitrous oxide is not a pathway (the
archive ships no critical climate-N load); it stays in the balance's
`total_gwp_co2e_kg`, and a climate-N pathway is a documented future
hook.

## Usage

``` r
build_n_pathway_exceedance(
  balance,
  critical_loads,
  nh3_source = c("soil", "total_agricultural"),
  resolution = c("grid", "polity", "country"),
  data = list(),
  example = FALSE
)
```

## Arguments

- balance:

  A
  [`build_nitrogen_balance()`](https://eduaguilera.github.io/whep/reference/build_nitrogen_balance.md)
  output keyed by `lon`, `lat`, `area_code`, `item_cbs_code`, `year`,
  carrying `area_ha`, `nh3_n_t` and `no3_n_t`.

- critical_loads:

  A named list of
  [`read_critical_n()`](https://eduaguilera.github.io/whep/reference/read_critical_n.md)
  outputs (each `lon`, `lat`, `value` in kg N per hectare) with elements
  `crit_nh3_emission`, `crit_leaching_gw` and `crit_load_sw`. Every
  positive-area balance cell must have a non-missing value in all three
  layers; incomplete coverage aborts instead of silently dropping the
  cell.

- nh3_source:

  Air-pressure scope: `"soil"` (default, field `nh3_n_t` only,
  consistent with the surplus boundary) or `"total_agricultural"` (also
  adds manure housing and storage ammonia from
  `data$manure_mgmt_nh3_n_t`, keyed to the exact balance grid; if
  absent, aborts rather than mislabelling soil-only pressure as total
  agricultural).

- resolution:

  Output grain: `"grid"` (default, per crop per cell) or `"polity"` /
  `"country"` (per crop per country, summing the mass terms).

- data:

  Optional named list of injected inputs. `manure_mgmt_nh3_n_t` (a
  tibble keyed to the balance grid with a `manure_mgmt_nh3_n_t` column)
  supplies the housing and storage ammonia for
  `nh3_source = "total_agricultural"`. Defaults to
  [`list()`](https://rdrr.io/r/base/list.html).

- example:

  If `TRUE`, return a small fixture instead of computing. Defaults to
  `FALSE`.

## Value

For `resolution = "grid"`, a tibble keyed `lon`, `lat`, `area_code`,
`item_cbs_code`, `year` with `area_ha`, the air-medium columns
(`critical_air_kgn_ha`, `actual_air_kgn_ha`, `exceed_share_air`,
`exceedance_air_kgn_ha`, `within_air_kgn_ha`, `exceedance_air_n_t`,
`within_air_n_t`, `actual_air_n_t`), the water-medium columns (the same
set with a `water` suffix, plus `critical_gw_kgn_ha`,
`critical_sw_kgn_ha` and `binding_water_medium`), `binding_boundary`,
and the `nh3_source` / `method_boundary` stamps. For the aggregate
resolutions, the grouping key with the summed per-medium mass terms and
the same stamps.

## Examples

``` r
build_n_pathway_exceedance(example = TRUE)
#> # A tibble: 4 × 28
#>     lon   lat area_code item_cbs_code  year area_ha critical_air_kgn_ha
#>   <dbl> <dbl>     <int>         <int> <int>   <dbl>               <dbl>
#> 1  0.25  0.25         1          2511  2010     100                  20
#> 2  0.25  0.25         1          2513  2010      50                  20
#> 3  0.75  0.25         1          2511  2010     200                  25
#> 4  0.25  0.75         1          2555  2010      40                  15
#> # ℹ 21 more variables: actual_air_kgn_ha <dbl>, exceed_share_air <dbl>,
#> #   exceedance_air_kgn_ha <dbl>, within_air_kgn_ha <dbl>,
#> #   exceedance_air_n_t <dbl>, within_air_n_t <dbl>, actual_air_n_t <dbl>,
#> #   critical_gw_kgn_ha <dbl>, critical_sw_kgn_ha <dbl>,
#> #   critical_water_kgn_ha <dbl>, actual_water_kgn_ha <dbl>,
#> #   exceed_share_water <dbl>, exceedance_water_kgn_ha <dbl>,
#> #   within_water_kgn_ha <dbl>, exceedance_water_n_t <dbl>, …
```
