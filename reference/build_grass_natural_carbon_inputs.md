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
  excreta_area_basis = c("luh2_grassland", "charged_grassland", "luh2_all_grassland"),
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
  the pin. Also: `country_grid`, the polycell support resolved to one
  row per cell and `area_code` (`lon`, `lat`, `area_code`,
  `cell_area_frac`), refused when a cell-`area_code` group is duplicated
  or `NA` (DA-23); `land_use` (per-cell class `area_ha`, used to spread
  excreta and to area-weight polity output); `excreta` (the `applied`
  tibble of
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

- excreta_area_basis:

  Which grassland hectares the polity's grazing excreta carbon is
  divided by: `"luh2_grassland"` (default), `"charged_grassland"` or
  `"luh2_all_grassland"`. See the section below; the choice is recorded
  in `method_excreta_area`.

- example:

  If `TRUE`, return a small fixture instead of reading remote data.
  Defaults to `FALSE`.

## Value

A tibble keyed by `(lon, lat, area_code, year, land_use)` at `"grid"`
resolution (or `(area_code, year, land_use)` at `"polity"`), with
`c_input_mgc_ha_yr`, `humified_fraction`, `method_c_input` and
`method_excreta_area`, for `land_use` in `"grassland"` and `"natural"`,
plus the polity columns below.

## Which grassland hectares the grazing excreta is divided by

The excreta carbon is a polity total, so it becomes a per-hectare
density by division, and `excreta_area_basis` picks the hectares to
divide by. It matters because the density is charged only to cells where
LPJmL wrote a grassland stand, while the divisor has always been the
polity's **whole** LUH2 grassland area, and
[`build_carbon_balance()`](https://eduaguilera.github.io/whep/reference/build_carbon_balance.md)
then multiplies the density by the LUH2 grassland area of each charged
cell. Whatever the uncharged hectares would have received is therefore
lost.

Measured on the `lpjml-grass-natural-net-c` pin against
[`read_luh2_landuse()`](https://eduaguilera.github.io/whep/reference/read_luh2_landuse.md),
the cells with a grassland stand hold 98.6% of the LUH2 grassland area
at 2010 (98.4% at 1960, 98.6% at 2020), leaving 46.6 Mha uncharged at
2010. How much excreta carbon that loses depends on where the herds are:
1.4% if excreta is proportional to grassland area, 13.8% under an
equal-carbon-per-polity probe, because the shortfall is concentrated in
small polities — 101 of 188 lose more than 1%, 53 more than 10%, 7 more
than half, and 13 (islands and city states, each under 0.01 Mha of
grassland) have no grassland stand at all and lose everything. The
reverse gap is negligible: 6 of 42,391 grassland cell-rows at 2010 have
no LUH2 grassland row.

The three bases are alternatives, not fallbacks, and the chosen one is
recorded in `method_excreta_area`:

- `"luh2_grassland"` (default, the published behaviour): divide by the
  polity's whole LUH2 grassland area. Does **not** conserve the polity's
  excreta carbon whenever a grassland hectare carries no LPJmL stand.

- `"charged_grassland"`: divide by the LUH2 grassland area of the cells
  the density is actually charged to, so every polity that has a
  grassland stand keeps its whole excreta carbon by construction. Raises
  the density on those cells by the reciprocal of the coverage above
  (1.4% globally on an area weighting, up to 17x in the worst measured
  polity) and leaves the uncovered hectares at zero, as they already are
  for net primary production. A polity with no grassland stand anywhere
  still loses all of its excreta — there is nowhere to charge it — which
  on the probe above is 13 polities and 6.9% of the carbon.

- `"luh2_all_grassland"`: keep the whole-area divisor and emit a
  grassland row for every LUH2 grassland cell instead, carrying zero net
  primary production where LPJmL has no stand (4,117 extra rows at 2010,
  8.8% more grassland rows). The only basis that conserves the excreta
  carbon globally, and it keeps the original spatial spread, at the cost
  of grassland rows in cells the LPJmL run does not simulate as
  grassland.

Dividing by the LPJmL grassland **stand** area is deliberately not
offered. It would conserve nothing under the area basis
[`build_carbon_balance()`](https://eduaguilera.github.io/whep/reference/build_carbon_balance.md)
actually uses, and it would buy little: measured at 1901 the stand area
and the LUH2 grassland area agree to a median 0.1% per cell (95.5% of
shared cells within 10%, global totals 1560.5 against 1576.0 Mha),
because WHEP's LPJmL land-use forcing is itself LUH2-derived.

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
build_grass_natural_carbon_inputs(example = TRUE)
#> # A tibble: 4 × 13
#>    year area_code polity_area_code reporting_polity_code reporting_polity_name
#>   <int>     <int>            <int> <chr>                 <chr>                
#> 1  2000        84               84 GRC-1947-2025         Greece (1947-2025)   
#> 2  2000        84               84 GRC-1947-2025         Greece (1947-2025)   
#> 3  2000         9                9 ARG-1902-2025         Argentina            
#> 4  2000         9                9 ARG-1902-2025         Argentina            
#> # ℹ 8 more variables: reporting_polity_has_geometry <lgl>, lon <dbl>,
#> #   lat <dbl>, land_use <chr>, c_input_mgc_ha_yr <dbl>,
#> #   humified_fraction <dbl>, method_c_input <chr>, method_excreta_area <chr>
```
