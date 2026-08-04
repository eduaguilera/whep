# Build grassland and natural-land soil carbon inputs from LPJmL.

Assemble the carbon returned to soil under grassland and natural
vegetation as the layer the soil-organic-carbon turnover models consume.
The LPJmL-derived net carbon density is read from the pinned
`lpjml-grass-natural-net-c` artifact by default, so running LPJmL is not
a prerequisite; pass `run_dir` (or set `WHEP_LPJML_RUN_DIR`) to derive
it from a finished local run instead, or `data$net_c` to supply it
directly. The pin holds only LPJmL-derived quantities: the grazing
excreta, both humification fractions and the polity attachment are
always computed here, so they never differ between the pinned and the
run-derived path. The class carbon input is the net primary production
minus harvested carbon (both per-plant-functional-type,
[`read_lpjml_npp()`](https://eduaguilera.github.io/whep/reference/read_lpjml_npp.md)),
floored at zero and converted to megagrams of carbon per hectare per
year (1 gC/m2 = 0.01 MgC/ha). Natural land sums the eleven natural
plant-functional-types (they coexist in one stand); grassland takes the
stand-area-weighted mean of the rainfed and irrigated grassland net
inputs and adds the grazing-excreta carbon from
[`build_livestock_nutrient_flows()`](https://eduaguilera.github.io/whep/reference/build_livestock_nutrient_flows.md)'s
`applied` stream spread uniformly over the polity's grassland area. The
humification fraction is the spontaneous-grass value for grassland and
the woody-residue value for natural land (both from
[residue_humification](https://eduaguilera.github.io/whep/reference/residue_humification.md)).

## Usage

``` r
build_grass_natural_carbon_inputs(
  resolution = c("grid", "polity"),
  data = list(),
  years = NULL,
  run_dir = NULL,
  example = FALSE
)
```

## Source

LPJmL run net primary production and harvested carbon; grassland and
natural carbon inputs per the WHEP historical carbon-balance design.

## Arguments

- resolution:

  `"grid"` (default, per cell and class) or `"polity"` (aggregated to
  `area_code`, area-weighting the per-hectare densities).

- data:

  Named list of pre-loaded inputs, each falling back to its reader when
  absent: `net_c` (the LPJmL net carbon density, `lon`, `lat`, `year`,
  `land_use`, `npp_c_mgc_ha_yr`; takes precedence over both `run_dir`
  and the pin); `npp` and `harvestc` (per cell, PFT and year, the
  [`read_lpjml_npp()`](https://eduaguilera.github.io/whep/reference/read_lpjml_npp.md)
  output); `stand_frac` (per cell, year and PFT name the
  managed-grassland stand fractions with columns `lon`, `lat`, `year`,
  `name_pft`, `stand_frac`). Supplying all three of `npp`, `harvestc`
  and `stand_frac` derives `net_c` without needing a run directory or
  the pin. Also: `country_grid` (`lon`, `lat`, `area_code`,
  `cell_area_frac`); `land_use` (per-cell class `area_ha`, used to
  spread excreta and to area-weight polity output); `excreta` (the
  `applied` tibble of
  [`build_livestock_nutrient_flows()`](https://eduaguilera.github.io/whep/reference/build_livestock_nutrient_flows.md),
  grassland rows carry `applied_c` tonnes C); `residue_humification`
  (defaults to
  [residue_humification](https://eduaguilera.github.io/whep/reference/residue_humification.md)).

- years:

  Optional integer vector of calendar years to keep. `NULL` (default)
  keeps every year the inputs cover. Threaded into the default LPJmL
  NPP, stand-fraction and land-use readers so they slice to the
  requested years; ignored for inputs supplied via `data`.

- run_dir:

  Path to a finished LPJmL run output directory holding `pft_npp.nc`,
  `pft_harvestc.nc` and `cftfrac.nc` (the `scenario_*` output folder).
  `NULL` (default) uses `WHEP_LPJML_RUN_DIR` when set, and the pinned
  artifact otherwise.

- example:

  If `TRUE`, return a small fixture instead of reading remote data.
  Defaults to `FALSE`.

## Value

A tibble keyed by `(lon, lat, area_code, year, land_use)` at `"grid"`
resolution (or `(area_code, year, land_use)` at `"polity"`), with
`c_input_mgc_ha_yr`, `humified_fraction` and `method_c_input`, for
`land_use` in `"grassland"` and `"natural"`, plus the polity columns
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

## Examples

``` r
build_grass_natural_carbon_inputs(example = TRUE)
#> # A tibble: 4 × 12
#>    year area_code polity_area_code reporting_polity_code reporting_polity_name
#>   <int>     <int>            <int> <chr>                 <chr>                
#> 1  2000       300               NA NA                    NA                   
#> 2  2000       300               NA NA                    NA                   
#> 3  2000        32               32 CMR-1961-2025         Cameroon             
#> 4  2000        32               32 CMR-1961-2025         Cameroon             
#> # ℹ 7 more variables: reporting_polity_has_geometry <lgl>, lon <dbl>,
#> #   lat <dbl>, land_use <chr>, c_input_mgc_ha_yr <dbl>,
#> #   humified_fraction <dbl>, method_c_input <chr>
```
