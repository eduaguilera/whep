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
the same stamps. Every grain also carries the polity columns below.

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
build_n_pathway_exceedance(example = TRUE)
#> # A tibble: 4 × 32
#>    year area_code polity_area_code reporting_polity_code reporting_polity_name
#>   <int>     <int>            <int> <chr>                 <chr>                
#> 1  2010         1                1 ARM-1991-2025         Armenia              
#> 2  2010         1                1 ARM-1991-2025         Armenia              
#> 3  2010         1                1 ARM-1991-2025         Armenia              
#> 4  2010         1                1 ARM-1991-2025         Armenia              
#> # ℹ 27 more variables: reporting_polity_has_geometry <lgl>, lon <dbl>,
#> #   lat <dbl>, item_cbs_code <int>, area_ha <dbl>, critical_air_kgn_ha <dbl>,
#> #   actual_air_kgn_ha <dbl>, exceed_share_air <dbl>,
#> #   exceedance_air_kgn_ha <dbl>, within_air_kgn_ha <dbl>,
#> #   exceedance_air_n_t <dbl>, within_air_n_t <dbl>, actual_air_n_t <dbl>,
#> #   critical_gw_kgn_ha <dbl>, critical_sw_kgn_ha <dbl>,
#> #   critical_water_kgn_ha <dbl>, actual_water_kgn_ha <dbl>, …
```
