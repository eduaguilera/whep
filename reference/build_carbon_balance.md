# Build the historical gridded soil-organic-carbon balance.

Reconstruct per-cell soil-organic-carbon stock trajectories: run the
selected turnover model to equilibrium under the earliest per-land-use
carbon inputs, open each land-use class at the stock `init` selects,
march forward on yearly per-cell per-land-use areas applying the model
annual update plus a carbon-conserving land-use-change transfer, and
derive the soil-organic-nitrogen change from the carbon rate via
asymmetric soil carbon-to-nitrogen ratios.

## Usage

``` r
build_carbon_balance(
  model = c("hsoc", "rothc", "icbm", "amg", "century", "lpjml"),
  init = c("own_equilibrium", "cell_average"),
  resolution = c("grid", "polity"),
  polity_validity = c("keep", "flag", "drop"),
  data = list(),
  years = NULL,
  crop_groups = list(),
  class_water = c("cell", "regime"),
  density_basis = c("renormalised", "static"),
  method_grazing = c("whep", "lpjml"),
  method_som_cn = c("justes_2009", "nicolardot_2001", "century"),
  example = FALSE
)
```

## Source

Aguilera, E., Guzman, G. I., Alvaro-Fuentes, J., Infante-Amate, J.,
Garcia-Ruiz, R., Carranza-Gallego, G., Soto, D. & Gonzalez de Molina, M.
(2018). A historical perspective on soil organic carbon in Mediterranean
cropland (Spain, 1900-2008). *Science of the Total Environment*, 621,
634-648.
[doi:10.1016/j.scitotenv.2017.11.243](https://doi.org/10.1016/j.scitotenv.2017.11.243)
; land-use-change carbon transfer ported from the Spain historical
pipeline.

## Arguments

- model:

  Turnover model: one of `"hsoc"` (default), `"rothc"`, `"icbm"`,
  `"amg"`, `"century"` or `"lpjml"`. The choice sets the equilibrium
  target, and through the time constant `soc_eq / c_input` the speed the
  stock relaxes toward it; the transient itself is a single exponential
  for every model.

- init:

  How each land-use class's opening stock is set. Neither option is the
  physical one and the default is **not** settled evidence; the
  measurements behind both sit on `.cb_init_density()` in the source.
  `"own_equilibrium"` (default) starts every class at the stock its own
  carbon input and climate support: it removes the opening transient the
  balance would otherwise report as soil nitrogen mineralization, at the
  cost of opening cropland at roughly a third of the carbon measured in
  those soils. `"cell_average"` starts every class in a cell at the
  fraction-weighted mean of the classes sharing it, the Spain historical
  behaviour: it opens cropland near its observed stock, as a proxy for
  the legacy carbon of the vegetation it replaced, at the cost of then
  draining that stock toward an equilibrium whep#799 puts several-fold
  too low. Recorded in `method_soc_init`.

- resolution:

  `"grid"` (default, per cell and land-use class) or `"polity"`
  (aggregated to `area_code` conserving carbon mass).

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

  Named list of pre-loaded inputs. Every entry named below falls back to
  its reader when absent EXCEPT the two grazing entries at the end,
  which have no reader at all and must be supplied under the default
  `method_grazing = "whep"`; the entries that do fall back are
  `c_inputs` (per cell, land-use class and year, with
  `c_input_mgc_ha_yr` and `humified_fraction`); `land_use` (yearly
  per-cell per-class `lon`, `lat`, `area_code`, `year`, `land_use`,
  `area_ha`); `climate` (either a precomputed per cell-year
  `climate_modifier`, applied to every land-use class alike, or the raw
  monthly drivers `temp_c` and `water_minus_pet_mm` keyed by `lon`,
  `lat`, `area_code`, `year`, `month`, from which the selected model's
  native modifier is computed internally per land-use class: for the
  RothC/HSOC cover term the monthly vegetated soil-cover fraction is
  taken from the generic land-use curve
  [`soc_soil_cover_curve`](https://eduaguilera.github.io/whep/reference/soc_soil_cover_curve.md)
  (a crop growth-stage canopy for cropland, sustained perennial cover
  for grassland/natural), so any `soil_cover` column supplied on the raw
  drivers is ignored); `clay` (per cell `clay_pct`); and an
  `natural_cover` (per cell and year, with `natural_cover`, the
  vegetated fraction of the natural stand, from
  [`read_lpjml_natural_cover`](https://eduaguilera.github.io/whep/reference/read_lpjml_natural_cover.md));
  when supplied it replaces
  [`soc_soil_cover_curve`](https://eduaguilera.github.io/whep/reference/soc_soil_cover_curve.md)'s
  constant for the NATURAL class only – managed grassland has no
  measured cover to use and stays on the curve – and when absent every
  class stays on the curve, which is the previous behaviour;
  `cropland_cover` (per cell, year and MONTH, from
  [`read_lpjml_crop_cover`](https://eduaguilera.github.io/whep/reference/read_lpjml_crop_cover.md)),
  which replaces the curve for the CROPLAND class with the cover its own
  crop calendar implies. The curve already gives cropland a season, but
  anchors it to the cell-year's warmest month: measured at 2010 the real
  crop mid-season falls there in only 5.2% of cropland cells and three
  or more months away in 51.0%, so the correction is one of timing
  rather than of annual mean (0.254 on the curve against 0.343 on the
  calendar); and an optional `equilibrium_climate` (the pre-industrial
  climatological normal, one representative monthly cycle per cell, used
  only for the equilibrium spin-up modifier while the forward march uses
  the year-specific drivers). Two further entries are forwarded to
  [`build_carbon_inputs`](https://eduaguilera.github.io/whep/reference/build_carbon_inputs.md)
  for the grassland grazing terms, and read only when `c_inputs` is not
  supplied: `livestock_intake` (the
  [`redistribute_feed`](https://eduaguilera.github.io/whep/reference/redistribute_feed.md)
  result) and `excreta` (the `applied` stream of
  [`build_livestock_nutrient_flows`](https://eduaguilera.github.io/whep/reference/build_livestock_nutrient_flows.md)).
  These are the two with no reader behind them – `excreta` is a
  livestock-pipeline output, not a readable input – so under the default
  `method_grazing = "whep"` a call that omits either is refused straight
  away, before any input is read. Pass `method_grazing = "lpjml"` to
  charge the grassland the model's own grazing and need neither.

- years:

  Optional integer vector of calendar years to keep. `NULL` (default)
  keeps every year the inputs cover, but reading the full LUH2 range
  (850-2015) is infeasible turnkey, so a subset is strongly recommended
  when the default readers are used. Threaded into every default reader
  ([`read_luh2_landuse`](https://eduaguilera.github.io/whep/reference/read_luh2_landuse.md),
  [`get_soc_climate_drivers`](https://eduaguilera.github.io/whep/reference/get_soc_climate_drivers.md)
  and
  [`build_carbon_inputs`](https://eduaguilera.github.io/whep/reference/build_carbon_inputs.md));
  ignored for inputs supplied via `data`.

- crop_groups:

  How cropland is resolved into land-use classes; see
  [`build_carbon_inputs()`](https://eduaguilera.github.io/whep/reference/build_carbon_inputs.md).
  [`list()`](https://rdrr.io/r/base/list.html) (default) marches crop
  GROUPS – herbaceous crops pooled per irrigation regime, woody crops
  per species, rainfed and irrigated separate. Each cell-year's LUH2
  cropland area is split over the groups in proportion to their
  crop-pattern area, so LUH2's total is kept (verified on a 2009-2010
  run: 1442.8 Mha either way, with the global stock moving 0.01%).
  Herbaceous groups follow the annual crop cover (and the crop
  calendar); woody groups take a perennial cover of 0.85, an ASSUMED
  value with no sourced constant behind it yet. Soil cover is computed
  once per cover profile and joined to the classes, so the class count
  does not multiply the monthly climate table. `list(method = "none")`
  keeps the single `cropland` class the package used before.

- class_water:

  How a cell's applied irrigation is shared among its land-use classes
  in the moisture term. `"cell"` (default) gives every class except
  natural land the cell-level water surplus, irrigation included, as
  before. `"regime"` concentrates the irrigation on the irrigated crop
  groups in proportion to their share of the cell and runs every other
  class on rain alone; the area-weighted mean over classes is the cell
  value either way. Needs `crop_groups`, because only groups carry a
  regime. Recorded in `method_class_water`.

- density_basis:

  Which crop area weights the per-crop carbon densities when they
  collapse to a class; see
  [`build_carbon_inputs()`](https://eduaguilera.github.io/whep/reference/build_carbon_inputs.md).
  `"renormalised"` (default) uses the yearly FAOSTAT-renormalised cell
  area the densities were computed on; `"static"` keeps the crop-pattern
  weights the package used before. Only read when the carbon inputs are
  built here rather than supplied.

- method_grazing:

  Whose grazing removes carbon from grassland and returns it as excreta;
  see
  [`build_grass_natural_carbon_inputs()`](https://eduaguilera.github.io/whep/reference/build_grass_natural_carbon_inputs.md).
  `"whep"` (default) charges the class WHEP's own grass intake and
  applied excreta, so it needs `data$livestock_intake` and
  `data$excreta` and aborts without them; `"lpjml"` uses the model's own
  livestock module and needs neither. Only read when the carbon inputs
  are built here rather than supplied through `data$c_inputs`.

- method_som_cn:

  Which published parameterisation sets the C:N at which soil organic
  matter forms from the carbon input that formed it,
  `CN_new = a - b / CN_input`, floored and then bounded by the IPCC 2019
  land-use range. `"justes_2009"` (default, a = 15.4, b = 76) is the
  refit on the larger combined dataset; `"nicolardot_2001"` (16.1, 123)
  is the original fit, and the citation HSOCN's own nitrogen submodel
  follows, so it is the parameterisation that puts WHEP and HSOCN on one
  basis; `"century"` (16, 120) is CENTURY/DayCent's shipped
  parameterisation. All three, each with its source, sample and floor,
  are in `inst/extdata/balances/som_marginal_cn.csv`. The choice sets
  the nitrogen of a carbon change and never the carbon: at an input C:N
  of 40 the three give a marginal C:N of 13.50, 13.03 and 13.00, so
  `son_change_kgn_ha` spans 3.8%; at a narrow (manure-like) input C:N of
  12 they give 9.07, 8.00 and 10.00, a 25% spread, and the widest
  anywhere is 29% at an input C:N of 15; above an input C:N of 70 they
  never differ by more than 1.9%. Recorded per row in `method_som_cn`,
  which instead reads `"land_use_default"` on a row whose input C:N is
  unknown and `"directional_ipcc_range"` when no input C:N is carried at
  all.

- example:

  If `TRUE`, return a small fixture instead of reading remote data.
  Defaults to `FALSE`.

## Value

A tibble keyed by `(lon, lat, area_code, land_use, year)` at `"grid"`
resolution (or `(area_code, year)` at `"polity"`), with `stock_mgc_ha`,
`mineralization_mgc_ha`, `c_input_mgc_ha`, `luc_transfer_mgc_ha`,
`luc_transfer_mgc`, `rate_mgc_ha`, `son_change_kgn_ha`, `area_ha`, and
one column per method choice that moves a number: `method_soc`,
`method_soc_init`, `method_class_water`, `method_area_basis`,
`method_grazing`, `method_som_cn` and `method_crop_groups`. All of them
survive the `"polity"` roll-up, which additionally carries
`input_land_ha` (the land the land-use input gave that polity-year) and
`modelled_land_frac` (`area_ha / input_land_ha`, the share of it the
densities in the same row are a mean over); see the coverage section
below. Plus the polity columns below, plus
`reporting_polity_out_of_span` when `polity_validity = "flag"`.

## Details

`polity_validity` governs this function's own output. The internal
[`get_soc_climate_drivers`](https://eduaguilera.github.io/whep/reference/get_soc_climate_drivers.md)
read it falls back on always keeps its rows: the march needs a climate
modifier for every cell-year it steps through, so dropping driver rows
for an anachronistic polity label would break the trajectory rather than
relabel it. The driver read therefore warns on its own key space
(whep#462) while this argument decides the fate of the balance rows.

## The land-use-change ledger closes on mass, not on density

`luc_transfer_mgc_ha` is the carbon a class received (positive) or gave
up (negative) through land-use change, per hectare of the class's
CURRENT area. A class whose area falls to zero still gives up its whole
stock – the balance carries the row at zero area and moves the carbon
into the growing classes – but at zero hectares that outflow has no
per-hectare expression, so it is reported as 0 and
`sum(luc_transfer_mgc_ha * area_ha)` over a cell-year is then positive
by exactly the vanished stock. `luc_transfer_mgc` is the same transfer
as a signed mass in Mg C, on every row including the vanished one, and
sums to zero within every cell-year (and, at `"polity"` resolution, is
the summed mass). Check conservation on the mass column.

## Soil depth

Every carbon and nitrogen density this function reports –
`stock_mgc_ha`, `mineralization_mgc_ha`, `c_input_mgc_ha`,
`luc_transfer_mgc_ha`, `rate_mgc_ha` and `son_change_kgn_ha` – is a
**0-30 cm topsoil** quantity, not a whole-profile one. The depth is a
property of the model family, not a free choice: HSOC comes from
Aguilera et al. (2018), which states that "the model was applied to the
0-30 cm layer of the soil"; the humification fractions in
[residue_humification](https://eduaguilera.github.io/whep/reference/residue_humification.md)
are that paper's Table 2; and the RothC/HSOC climate modifier rescales
RothC's own 0-23 cm maximum topsoil-moisture-deficit expression to 30 cm
(`soc_rate_modifier_rothc(soil_depth_m = 0.3)`).

Comparing this output against a whole-profile soil-carbon product is
therefore a category error. LPJmL's `soilc`, in particular, reports
carbon over its top 3 m and is roughly three times a topsoil stock;
global 0-30 cm references such as GSOCmap are the valid comparators.
Stating this is not pedantry – an unstated depth convention is what made
a chain of contradictory diagnoses possible (whep#799).

## Spatial support

Every default reader on the carbon path – the land-use areas, the carbon
inputs, the climate drivers and the clay – resolves its cell-to-polity
table through one polycell support
([`read_polycell_support()`](https://eduaguilera.github.io/whep/reference/read_polycell_support.md)),
read at a static reference year. A cell shared between polities
therefore delivers to each only the land it holds there, and no reader
can be left on a different crosswalk: half the path on one footprint and
half on another would surface as an ordinary climate-coverage warning
from the modifier join, not as an error. Land the reporting vocabulary
cannot key (no `area_code`) is reported and dropped, never folded into
another polity's.

## A polity total covers the modelled land, not the polity

The climate drivers are on the LPJmL run's grid, a coarser land mask
than LUH2's, so land outside it has no climate modifier and leaves the
balance. A `"polity"` row is therefore an area-weighted mean over the
hectares that survived, and `modelled_land_frac` is the share of the
polity-year's land that did: read it before quoting a national stock or
trend. It is **not** a quality flag on the hectares that are there –
they are modelled exactly as before – and no value in this output
changes because of it.

Measured at 2010 against the pinned `lpjml-soc-hydrology` grid, which
drops 296.5 Mha of LUH2 land over 7,070 cell-polity compartments: global
coverage is **0.977**, 149 of 193 polities are below 1, 51 are below
0.9, and **12 carry no row at all** – Antigua and Barbuda, Bahrain,
Barbados, Dominica, Grenada, Macao, Malta, Mauritius, Saint Kitts and
Nevis, Saint Lucia, Singapore and Tonga, whose land is entirely outside
the grid. Worst of those that survive: Cabo Verde 0.22, Rest of World
0.31, Vanuatu 0.32, Bahamas 0.32, Cyprus 0.35, Somalia 0.72 (18.0 Mha),
the Philippines 0.73 (7.7 Mha), Greece 0.76 (3.1 Mha). A warning names
the polities that vanish, because no column on the output can: they have
no row to carry one.

The land is reported rather than gap-filled, and that is a decision with
evidence behind it (whep#1166). The run grid holds 58,795 cells against
CRU TS's 67,420 land cells, and what it excludes is fractional-land
coastal and island cells: median land fraction 0.14, against 0.998
inside the grid. CRU still carries temperature, PET and precipitation
for 5,981 of the 6,847 cells that go – 291.5 of the 296.5 Mha – so a
climate *could* be assembled there without LPJmL. The carbon input could
not: 261.4 of those 296.5 Mha are natural (214.8) and grassland (46.6),
whose input is the LPJmL net carbon flux, and this package holds no
second source for it. Filling the climate alone would march 261.4 Mha on
a zero-filled carbon input, draining its whole opening stock – the
failure whep#1146 named, which today reaches 0.212 ha globally. Filling
the input as well means asserting a productivity for land the model was
never run on. Only a rerun on a land mask matching LUH2's puts real
carbon on those hectares.

## What this balance does not cover

The balance runs on the LPJmL grid, because the climate drivers do. That
grid is a coarser land mask than LUH2's, so **not every LUH2 hectare is
in the output**. On the default readers at 2010, 7,079 cell-polity
compartments carrying 296.8 Mha of LUH2 land – 30.4 Mha cropland, 46.6
Mha grassland, 214.9 Mha natural, 4.8 Mha urban – have no climate driver
and are dropped, with a warning naming the hectares and the worst-hit
polities. The loss is essentially a coastline, so it is negligible
globally (1.4% of grassland, 2.6% of natural land) and large for
maritime polities: 31.6% of Greece's grassland, 25.3% of the
Philippines', 22.7% of Somalia's, 17.8% of Indonesia's, 12.8% of the
United Kingdom's, 12.5% of Italy's. A national SOC total from this
function is a total over the modelled hectares, not over the polity.

Dropped is not the same as marched at zero. A class with LUH2 area but
no carbon-input row IS kept, at zero input (that is what makes `urban`
dilute rather than deflate the cell), and whep#1146 read the coverage
gap as that case. It is not: measured against the
`lpjml-grass-natural-net-c` pin and the real climate table, every one of
those 46.6 Mha of grassland is dropped for want of a climate driver, and
the grassland actually marching on a zero-filled input is 0.212 ha
globally at 2010. Both quantities are reported at run time rather than
left to be re-derived.

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
build_carbon_balance(example = TRUE)
#> # A tibble: 6 × 20
#>    year area_code polity_area_code reporting_polity_code reporting_polity_name
#>   <int>     <int>            <int> <chr>                 <chr>                
#> 1  2000         1                1 ARM-1991-2025         Armenia              
#> 2  2000         1                1 ARM-1991-2025         Armenia              
#> 3  2001         1                1 ARM-1991-2025         Armenia              
#> 4  2001         1                1 ARM-1991-2025         Armenia              
#> 5  2002         1                1 ARM-1991-2025         Armenia              
#> 6  2002         1                1 ARM-1991-2025         Armenia              
#> # ℹ 15 more variables: reporting_polity_has_geometry <lgl>, lon <dbl>,
#> #   lat <dbl>, land_use <chr>, area_ha <dbl>, stock_mgc_ha <dbl>,
#> #   mineralization_mgc_ha <dbl>, c_input_mgc_ha <dbl>,
#> #   luc_transfer_mgc_ha <dbl>, method_class_water <chr>,
#> #   luc_transfer_mgc <dbl>, rate_mgc_ha <dbl>, son_change_kgn_ha <dbl>,
#> #   method_soc <chr>, method_soc_init <chr>
```
