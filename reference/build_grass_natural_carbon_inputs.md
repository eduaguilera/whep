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
stand-area-weighted mean of the rainfed and irrigated grassland stands
and charges them one grazing removal and one excreta return, from WHEP's
own livestock chain by default and from LPJmL's livestock module on
request (`method_grazing`), spread uniformly over the polity's grassland
area. The humification fraction is the spontaneous-grass value for
grassland and the woody-residue value for natural land (both from
[residue_humification](https://eduaguilera.github.io/whep/reference/residue_humification.md)).

## Usage

``` r
build_grass_natural_carbon_inputs(
  resolution = c("grid", "polity"),
  method_grazing = c("whep", "lpjml"),
  method_natural_hf = c("woody_share", "woody"),
  method_natural_c = c("litterfall", "npp"),
  data = list(),
  years = NULL,
  run_dir = NULL,
  excreta_area_basis = c("luh2_grassland", "charged_grassland", "luh2_all_grassland"),
  grazed_area_basis = c("luh2_grassland", "charged_grassland"),
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

- method_grazing:

  Whose grazing removes carbon from grassland and returns it as excreta.
  `"whep"` (default) uses WHEP's own estimates: the realised grass
  intake of
  [`redistribute_feed()`](https://eduaguilera.github.io/whep/reference/redistribute_feed.md)
  is the removal, converted from dry matter at the package's carbon
  fraction
  ([`grass_access_shares()`](https://eduaguilera.github.io/whep/reference/grass_access_shares.md)`$w_c_dm`),
  and the grassland `applied` stream of
  [`build_livestock_nutrient_flows()`](https://eduaguilera.github.io/whep/reference/build_livestock_nutrient_flows.md)
  is the return. LPJmL's own grazing is backed out in full: the class
  starts from the whole grassland production (`npp_c_mgc_ha_yr`), not
  from what LPJmL left after grazing it, so no carbon is counted twice.
  Both `data$livestock_intake` and `data$excreta` are then required, and
  a requested year the intake table does not cover aborts rather than
  passing silently as an ungrazed year.

  `"lpjml"` uses the model's own livestock module instead
  (`npp_c_mgc_ha_yr - net_c_mgc_ha_yr`, the `pft_harvestc` grassland
  bands), and refuses WHEP's excreta with a warning, because that return
  is already inside the layer. LPJmL 6.1 grazes `UptakeC` off the
  managed-grassland stand, returns `FecesC` and `UrineC` to it, and
  books the difference as `pft_harvestc`: on the 1750-2023 run
  `pft_harvestc = UptakeC - FecesC - UrineC` holds cell by cell to
  machine precision (maximum relative difference 0 at 1900, 1960 and
  2010), so `NPP - pft_harvestc` already carries an excreta return worth
  5.9% of grassland production in 1900, 3.8% in 1960 and 4.5% in 2010.
  Adding WHEP's excreta on top of it, which the `excreta` argument used
  to do, books that return twice.

  The two are alternatives, not tiers: `"whep"` is the default because
  WHEP's livestock chain, not LPJmL's, is what the rest of the package
  charges for feed, excretion and manure, so a grassland input built on
  LPJmL's herd would not reconcile with the manure the same herd applies
  to cropland. Backing LPJmL's grazing out recovers the carbon MASS but
  not its dynamics: the production the model simulated was itself shaped
  by the defoliation it applied, so the whole-production starting point
  is a known approximation, not an ungrazed counterfactual. Where WHEP's
  grazing removal exceeds a cell's production the input is floored at
  zero and the floored carbon is reported in a warning. Recorded in
  `method_c_input`.

- method_natural_hf:

  How natural land's humification fraction is set. `"woody_share"`
  (default) carbon-weights the
  [residue_humification](https://eduaguilera.github.io/whep/reference/residue_humification.md)
  woody and herbaceous coefficients by the share of each cell-year's
  natural production that the woody PFTs made; `"woody"` applies the
  woody coefficient everywhere, which was the previous behaviour. Five
  of the fourteen natural PFTs are not woody and carry 28.1% of natural
  production at 2010. Falls back to `"woody"`, with a warning, when the
  net-carbon layer has no `woody_share` column.Which grassland hectares
  the grazing excreta is divided by

  The excreta carbon is a polity total, so it becomes a per-hectare
  density by division, and `excreta_area_basis` picks the hectares to
  divide by. It matters because the density is charged only to cells
  where LPJmL wrote a grassland stand, while the divisor has always been
  the polity's **whole** LUH2 grassland area, and
  [`build_carbon_balance()`](https://eduaguilera.github.io/whep/reference/build_carbon_balance.md)
  then multiplies the density by the LUH2 grassland area of each charged
  cell. Whatever the uncharged hectares would have received is therefore
  lost.

  Measured on the `lpjml-grass-natural-net-c` pin against
  [`read_luh2_landuse()`](https://eduaguilera.github.io/whep/reference/read_luh2_landuse.md),
  the cells with a grassland stand hold 98.6% of the LUH2 grassland area
  at 2010 (98.4% at 1960, 98.6% at 2020), leaving 46.6 Mha uncharged
  at 2010. How much excreta carbon that loses depends on where the herds
  are: 1.4% if excreta is proportional to grassland area, 13.8% under an
  equal-carbon-per-polity probe, because the shortfall is concentrated
  in small polities — 101 of 188 lose more than 1%, 53 more than 10%, 7
  more than half, and 13 (islands and city states, each under 0.01 Mha
  of grassland) have no grassland stand at all and lose everything. The
  reverse gap is negligible: 6 of 42,391 grassland cell-rows at 2010
  have no LUH2 grassland row.

  The three bases are alternatives, not fallbacks, and the chosen one is
  recorded in `method_excreta_area`:

  - `"luh2_grassland"` (default, the published behaviour): divide by the
    polity's whole LUH2 grassland area. Does **not** conserve the
    polity's excreta carbon whenever a grassland hectare carries no
    LPJmL stand.

  - `"charged_grassland"`: divide by the LUH2 grassland area of the
    cells the density is actually charged to, so every polity that has a
    grassland stand keeps its whole excreta carbon by construction.
    Raises the density on those cells by the reciprocal of the coverage
    above (1.4% globally on an area weighting, up to 17x in the worst
    measured polity) and leaves the uncovered hectares at zero, as they
    already are for net primary production. A polity with no grassland
    stand anywhere still loses all of its excreta — there is nowhere to
    charge it — which on the probe above is 13 polities and 6.9% of the
    carbon.

  - `"luh2_all_grassland"`: keep the whole-area divisor and emit a
    grassland row for every LUH2 grassland cell instead, carrying zero
    net primary production where LPJmL has no stand (4,117 extra rows at
    2010, 8.8% more grassland rows). The only basis that conserves the
    excreta carbon globally, and it keeps the original spatial spread,
    at the cost of grassland rows in cells the LPJmL run does not
    simulate as grassland.

  Dividing by the LPJmL grassland **stand** area is deliberately not
  offered. It would conserve nothing under the area basis
  [`build_carbon_balance()`](https://eduaguilera.github.io/whep/reference/build_carbon_balance.md)
  actually uses, and it would buy little: measured at 1901 the stand
  area and the LUH2 grassland area agree to a median 0.1% per cell
  (95.5% of shared cells within 10%, global totals 1560.5 against 1576.0
  Mha), because WHEP's LPJmL land-use forcing is itself
  LUH2-derived.Which grassland hectares the grazing removal is divided
  by

  `grazed_area_basis` is the same question for the flux that leaves the
  grassland. WHEP's grass intake is a polity total too, so it also
  becomes a density by division and is also charged only to cells with
  an LPJmL grassland stand. Under the default divisor the polity's
  grassland gives up the coverage share of what its herd ate, and the
  rest is grazed off nothing.

  The coverage is the same one measured for the excreta above: 98.6% of
  the LUH2 grassland area globally at 2010, but 0.68 for Greece, 0.77
  for Somalia, 0.82 for Indonesia and 0.87 for the United Kingdom.
  Weighting each polity by LPJmL's own grazing removal as a stand-in for
  WHEP's intake (1,100 Tg C at 2010, against the 1,188 Tg C the SOC
  branch reports at 2020), 1.8% of the grazed carbon is removed from
  nothing; 50 of 175 polities lose more than 1% of their grassland
  carbon input to the choice and 11 more than 5%.

  The two bases are alternatives, not fallbacks, and the chosen one is
  recorded in `method_grazed_area`:

  - `"luh2_grassland"` (default, the published behaviour): divide by the
    polity's whole LUH2 grassland area. The density on a charged cell is
    then the polity's true mean grazing pressure, but the polity gives
    up less carbon than its herd ate.

  - `"charged_grassland"`: divide by the LUH2 grassland area of the
    cells the density is charged to, so the grassland gives up exactly
    the carbon the herd removed. The cost is that the whole national
    herd is then charged to the modelled subset of the pasture, raising
    the removal density there by the reciprocal of the coverage (1.46x
    for Greece, 1.29x for Somalia).

  The two are not a strict improvement on one another, which is why the
  default is unchanged: the first conserves the per-hectare density, the
  second conserves the mass. On the probe above, switching lowers the
  global grassland plant carbon input by 20.2 Tg C at 2010 (-0.13%;
  -0.13% at 1960 and -0.14% at 2020) and by up to 8.2% for a single
  polity. Neither basis reaches the uncovered hectares themselves:
  [`build_carbon_balance()`](https://eduaguilera.github.io/whep/reference/build_carbon_balance.md)
  gives them a grassland pool with a zero carbon input, which is the
  larger defect of the two and is tracked separately (whep#1146).

- method_natural_c:

  Which LPJmL quantity is natural land's carbon input. `"litterfall"`
  (default) is `litfallc_nv`, what the model actually returns to the
  soil, excluding the increment retained in living biomass and what fire
  and land conversion remove; `"npp"` is the natural PFTs' whole primary
  production, the previous default. Litterfall is the physically correct
  soil input, and it is a good deal smaller than production where it
  matters: on the published pin at 2010 the per-cell ratio has a median
  of 0.844 across cells carrying natural vegetation, but weighted by
  LUH2 natural area the litter MASS is 0.648 of the production mass
  (0.632 at 2000), because the most productive cells retain the largest
  share of their production as growing biomass. The natural class's
  area-weighted mean input therefore falls from 8.1 to 5.2 MgC/ha/yr,
  and its equilibrium soil carbon with it. The per-cell ratio declines
  from 0.902 in the 1750s to 0.838 in the 2000s, so production as the
  input would carry a CO2-fertilisation trend into the soil. A tail of
  cells with almost no production carries litter above production (ratio
  above 2 on 3.2% of natural area, holding 0.7% of the litter mass); it
  is left as the run wrote it. The `lpjml-grass-natural-net-c` pin
  carries litterfall since its 2026-09-03 version (the 1750-2023 run);
  an older pin or run aborts naming the missing column rather than
  falling back. Recorded in `method_c_input`.

  Litterfall **includes root turnover**, so natural land must not
  receive a separate root term on top, and does not: LPJmL adds each
  PFT's root turnover carbon to the belowground litter pool and to the
  same `LITFALLC` accumulator as leaf litter
  (`src/grass/turnover_grass.c` lines 130-131 and
  `src/tree/turnover_tree.c` lines 162-163 at PIK-LPJmL/LPJmL, plus tree
  root exudates at lines 143-144). The run confirms it: at 1760 over
  31,889 near-pure natural cells the litterfall/NPP median is 0.894, and
  **0.965 on grass-dominated cells**, which allocate roughly half their
  production below ground – an above-ground-only litterfall would sit
  near 0.5 there. Adding fire closes the pre-industrial steady state at
  0.930. Cropland is the opposite case:
  [`build_soil_carbon_inputs()`](https://eduaguilera.github.io/whep/reference/build_soil_carbon_inputs.md)
  assembles residues, roots and manure as separate terms, because a crop
  stand's roots are not an LPJmL litterfall flux.

  It is a gross accumulator, not a residual: LPJmL increments it
  wherever carbon enters the litter pools (turnover, phenological
  shedding, root exudates, reproduction, mortality, fire-killed but
  uncombusted biomass, harvest residues). It excludes what land-use
  conversion releases, which the model books separately as
  `litfallc_luc` and which is far from small: on the same near-pure
  natural cells at 1755-1765 it is 14.3% of natural NPP against
  litterfall's 88.2%. Excluding it is what WHEP needs, because the
  balance derives its own land-use-change transfer from LUH2 areas and
  its own stocks; counting the model's conversion litter as a soil input
  as well would book that carbon twice.

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
  grassland rows carry `applied_c` tonnes C); `livestock_intake` (the
  [`redistribute_feed()`](https://eduaguilera.github.io/whep/reference/redistribute_feed.md)
  result, the same tibble the nitrogen path takes as
  `data$livestock_intake`, whose `feed_quality == "grass"` rows carry
  the grazed dry matter); `residue_humification` (defaults to
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
  in `method_excreta_area`. Only bites under `method_grazing = "whep"`;
  `"lpjml"` returns no WHEP excreta to divide.

- grazed_area_basis:

  Which grassland hectares the polity's grass intake carbon is divided
  by: `"luh2_grassland"` (default) or `"charged_grassland"`. See the
  section below; the choice is recorded in `method_grazed_area`. Only
  bites under `method_grazing = "whep"`; `"lpjml"` takes the run's own
  harvest, which is already per cell.

- example:

  If `TRUE`, return a small fixture instead of reading remote data.
  Defaults to `FALSE`.

## Value

A tibble keyed by `(lon, lat, area_code, year, land_use)` at `"grid"`
resolution (or `(area_code, year, land_use)` at `"polity"`), with
`c_input_mgc_ha_yr`, `humified_fraction`, `method_c_input`,
`method_excreta_area` and `method_grazed_area`, for `land_use` in
`"grassland"` and `"natural"`, plus the polity columns below.

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
#> # A tibble: 4 × 14
#>    year area_code polity_area_code reporting_polity_code reporting_polity_name
#>   <int>     <int>            <int> <chr>                 <chr>                
#> 1  2000        84               84 GRC-1947-2025         Greece (1947-2025)   
#> 2  2000        84               84 GRC-1947-2025         Greece (1947-2025)   
#> 3  2000         9                9 ARG-1902-2025         Argentina            
#> 4  2000         9                9 ARG-1902-2025         Argentina            
#> # ℹ 9 more variables: reporting_polity_has_geometry <lgl>, lon <dbl>,
#> #   lat <dbl>, land_use <chr>, c_input_mgc_ha_yr <dbl>,
#> #   humified_fraction <dbl>, method_c_input <chr>, method_excreta_area <chr>,
#> #   method_grazed_area <chr>
```
