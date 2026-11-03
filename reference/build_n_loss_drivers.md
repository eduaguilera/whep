# Build the IPCC 2019 wet/dry drivers of the nitrogen loss cascade.

Put the IPCC 2019 Tier 1 wet/dry class of
[`build_cell_moisture_regime()`](https://eduaguilera.github.io/whep/reference/build_cell_moisture_regime.md),
and the Atlantic/Mediterranean drivers mapped from it, on every row of a
gridded nitrogen balance, as the two driver tables
[`build_nitrogen_balance()`](https://eduaguilera.github.io/whep/reference/build_nitrogen_balance.md)
joins: `n_balance_drivers` (per balance key and loss `fert_type`, read
by
[`calculate_nh3()`](https://eduaguilera.github.io/whep/reference/calculate_nh3.md),
[`calculate_soil_n2o()`](https://eduaguilera.github.io/whep/reference/calculate_soil_n2o.md)
and
[`calculate_indirect_n2o_nh3()`](https://eduaguilera.github.io/whep/reference/calculate_indirect_n2o_nh3.md))
and `n_balance_leaching_drivers` (per balance key, read by
[`calculate_n_leaching()`](https://eduaguilera.github.io/whep/reference/calculate_n_leaching.md)).
[`build_nitrogen_balance()`](https://eduaguilera.github.io/whep/reference/build_nitrogen_balance.md)
calls it itself at grid resolution when neither table is supplied.

A row whose cell, polity, commodity balance item and water regime has a
class of its own takes it. A row without one takes its cell's class on
precipitation alone (no crop, no irrigation): a row with no crop item,
as the soil organic matter sequestration rows, and a row of an item WHEP
holds no land of in that water regime, as grass. A cell lacking CRU
climate variables takes them from its nearest CRU land cell (see
[`build_cell_moisture_regime()`](https://eduaguilera.github.io/whep/reference/build_cell_moisture_regime.md)).
Every row ends with a class.

`climate` is `"ATL"` on wet rows and `"MED"` on dry rows. `irrig_type`,
the
[n2o_efs_disaggregated](https://eduaguilera.github.io/whep/reference/n2o_efs_disaggregated.md)
key of the Atlantic/Mediterranean options, is `"Flooded"` on every rice
row (the commodity balance item of the production items classed
`Crop_irrig == "Rice"` in
[items_prod_full](https://eduaguilera.github.io/whep/reference/items_prod_full.md))
in both climates and both water regimes, `"Med_average"` on other
irrigated MED rows, `"Rainfed"` on other rainfed MED rows and `"Tier_1"`
on every other ATL row (the ATL table defines only `"Tier_1"` and
`"Flooded"`).

## Usage

``` r
build_n_loss_drivers(
  rows = NULL,
  window = c("trailing_30", "annual", "normal"),
  water = c("precip", "precip_irrig"),
  data = NULL,
  config = list(),
  methods = list(),
  example = FALSE
)
```

## Source

IPCC (2019), 2019 Refinement to the 2006 IPCC Guidelines for National
Greenhouse Gas Inventories, Volume 4, Chapter 11, Table 11.1, footnote 4
(p. 11.13). Allen, R. G., Pereira, L. S., Raes, D. & Smith, M. (1998).
Crop evapotranspiration. FAO Irrigation and Drainage Paper 56, Table 22.
Pribyl, D. W. (2010). A critical review of the conventional SOC to SOM
conversion factor. Geoderma 156, 75-83.
[doi:10.1016/j.geoderma.2010.02.003](https://doi.org/10.1016/j.geoderma.2010.02.003)
.

## Arguments

- rows:

  The balance rows to give drivers: `lon`, `lat`, `area_code`,
  `item_cbs_code` (`NA` on rows without a crop), `year` and
  `water_regime` (`"rainfed"` or `"irrigated"`), plus optionally
  `fert_type`, the loss `fert_type` of each row. With `fert_type`, the
  loss drivers are built for its non-missing `(key, fert_type)` pairs;
  without it, for every key and the seven loss `fert_type`s
  (`"Excreta"`, `"Liquid"`, `"Solid"`, `"SOM"`, `"Synthetic"`,
  `"Human"`, `"Recycling"`). The leaching drivers cover every key.
  `n_input_t`, the N of each row, is needed wherever an irrigated row
  has no class of its own: such a row must carry no N.

- window, water, config:

  As in
  [`build_cell_moisture_regime()`](https://eduaguilera.github.io/whep/reference/build_cell_moisture_regime.md).
  The window also sets the precipitation-only class of rows without a
  crop.

- data:

  Optional list of inputs, passed to
  [`build_cell_moisture_regime()`](https://eduaguilera.github.io/whep/reference/build_cell_moisture_regime.md)
  (`climate`, `crop_depth`, `lpjml_years`, `lpjml_grid`, `regime_areas`,
  `country_grid`), plus `moisture_regime`, a
  [`build_cell_moisture_regime()`](https://eduaguilera.github.io/whep/reference/build_cell_moisture_regime.md)
  result to use instead of building it. Climate is read once and shared.
  The leaching drivers read `carbon_balance` (required) and, each read
  when absent, `lpjml_drainage` and `lpjml_runoff` (annual
  [`read_lpjml_hydrology()`](https://eduaguilera.github.io/whep/reference/read_lpjml_hydrology.md)
  tables), `soil_hydraulic`, `soil_mass`, `soil_texture_fao`,
  `soil_drainage`, `image_gnm_runoff` (their readers' results),
  `carbon_inputs` (a gridded
  [`build_soil_carbon_inputs()`](https://eduaguilera.github.io/whep/reference/build_soil_carbon_inputs.md)
  result) and `regime_areas`.

- methods:

  Named list choosing which leaching drivers are built and how:
  `leaching`, the
  [`calculate_n_leaching()`](https://eduaguilera.github.io/whep/reference/calculate_n_leaching.md)
  method (`"ipcc_fracleach"`, the default, `"fracleach_surplus"`,
  `"meisinger_drainage"` or `"image_gnm"`); `som_source`,
  `"carbon_balance"` (default) or `"hwsd"` (HWSD's observed topsoil
  carbon); `som_factor`, `"pribyl_2010"` (default, 2) or
  `"conventional"` (1.724); `root_depth`, `"by_regime"` (default),
  `"midpoint"`, `"upper"`, `"lower"`, `"topsoil_30cm"` or `"top_1m"`.

- example:

  If `TRUE`, return a small fixture instead of reading data. Defaults to
  `FALSE`.

## Value

A named list of two tibbles:

- `n_balance_drivers`: one row per balance key (`lon`, `lat`,
  `area_code`, `item_cbs_code`, `year`, `water_regime`) and `fert_type`,
  with `moisture_regime` (`"wet"` or `"dry"`), `climate` (`"ATL"` or
  `"MED"`), `irrig_type`, `method_moisture_regime`, `method_climate`
  (`"from_moisture_regime"`) and `climate_fill_km`.

- `n_balance_leaching_drivers`: one row per balance key, with
  `moisture_regime`, `method_moisture_regime` and `climate_fill_km`,
  plus the drivers of `methods$leaching` and their stamps (see Leaching
  drivers).

Both are join tables keyed on the balance key; the balance they are
joined to carries the polity columns.

## Details

Which driver columns each loss method reads, and which this function
supplies:

|  |  |  |
|----|----|----|
| Method | Driver columns | Supplied |
| `nh3`: `"ipcc"` | `fert_type` | yes |
| `nh3`: `"manner"`, `"manner_default"` | MANNER's, see below | no |
| `n2o`: `"ipcc2019"` | `moisture_regime` | yes |
| `n2o`: `"ipcc2019_atl_med"` | `climate` | yes |
| `n2o`: `"aguilera"`, `"ipcc2006"` | `climate`, `irrig_type` | yes |
| `n2o_indirect`: `"ipcc2019"` | `moisture_regime` | yes |
| `n2o_indirect`: `"ipcc2019_atl_med"` | `climate`, `irrig_type` | yes |
| `leaching`: `"ipcc_fracleach"` | `moisture_regime`, `water_regime` | yes |
| `leaching`: `"fracleach_surplus"` | same as `"ipcc_fracleach"` | yes |
| `leaching`: `"meisinger_drainage"` | Meisinger's, see below | yes |
| `leaching`: `"image_gnm"` | IMAGE-GNM's, see below | yes |

MANNER's drivers are later work: it reads `manner_fertiliser`,
`soil_ph`, `rate_kg_ha`, `rainfall_mm`, `irrigated`, `temp_c`,
`temp_c_annual_mean`, `windspeed_ms`, `technique`, `system`,
`incorporation_delay_h` and `species` (see
[`calculate_nh3()`](https://eduaguilera.github.io/whep/reference/calculate_nh3.md)),
and
[`build_nitrogen_balance()`](https://eduaguilera.github.io/whep/reference/build_nitrogen_balance.md)
refuses it before assembling any input unless its driver table is
supplied.

The leaching table carries the drivers of `methods$leaching` only.

`method_moisture_regime` is
[`build_cell_moisture_regime()`](https://eduaguilera.github.io/whep/reference/build_cell_moisture_regime.md)'s
stamp. A row that took its cell's precipitation-only class carries the
extra field `cell_rainfed_class=no_crop_item` (no crop item) or
`cell_rainfed_class=no_regime_area` (an item without regime land of the
row's water regime), and a row in a cell that took climate variables
from its nearest CRU cell the field `nearest_cru_cell=<col>[+<col>...]`,
naming them.

## Leaching drivers

Under `methods$leaching = "meisinger_drainage"` or `"image_gnm"` every
driver is a source layer joined on the cell, polity, commodity balance
item and year, never on a name, and stamped:

- `land_use`: `"Grassland"` on the grass item (3000), `"Cropland"`
  otherwise.

- `drainage_mm` (Meisinger) and `runoff_mm` (IMAGE-GNM): the year's
  LPJmL deep seepage and total runoff
  ([`read_lpjml_hydrology()`](https://eduaguilera.github.io/whep/reference/read_lpjml_hydrology.md)
  `"drainage"` and `"runoff"`, from `WHEP_LPJML_RUN_DIR`), whole-cell
  quantities both water regime rows of a cell share (`method_drainage`,
  `method_runoff`).

- `soil_field_capacity`, `soil_wilting_point` and `soil_porosity`:
  [`read_soil_hydraulic()`](https://eduaguilera.github.io/whep/reference/read_soil_hydraulic.md)
  (`method_soil_hydraulic`).

- `som_share` (Meisinger, a fraction) and `soil_organic_carbon_pct`
  (IMAGE-GNM): the 0-30 cm soil organic carbon stock of the row's land
  use in `data$carbon_balance`
  ([`build_carbon_balance()`](https://eduaguilera.github.io/whep/reference/build_carbon_balance.md);
  cropland classes area-weighted), over the HWSD topsoil mass of
  [`read_hwsd_topsoil_mass()`](https://eduaguilera.github.io/whep/reference/read_hwsd_topsoil_mass.md);
  `som_share` multiplies the carbon fraction by the SOM:SOC factor,
  `som_soc_factor_pribyl` (2) of
  [n_attenuation_constants](https://eduaguilera.github.io/whep/reference/n_attenuation_constants.md)
  by default (`method_som`). A cell-polity the carbon balance lacks
  takes the cell's stock in its other polities, stamped
  `cell_other_polity`; a cell it lacks altogether (the carbon balance
  runs on the CRU and LPJmL grids only) takes the stock of the nearest
  cell holding the row's land use, stamped
  `nearest_cell_same_land_use=<km>km`.

- `c_input_t` (Meisinger): the C the row's inputs carry to the soil, t C
  per year: the crop's
  [`build_soil_carbon_inputs()`](https://eduaguilera.github.io/whep/reference/build_soil_carbon_inputs.md)
  carbon, pooled over the item's production items, on crop rows, and
  `data$carbon_balance`'s grassland input on the grass item
  (`method_c_input`). Both regime rows of an item carry the item's whole
  C;
  [`build_nitrogen_balance()`](https://eduaguilera.github.io/whep/reference/build_nitrogen_balance.md)
  divides it by the item's whole `n_input_full_nosom_t` into `cn_input`.
  A crop row without a carbon input of its item in its cell and polity
  keeps `c_input_t` missing, stamped `no_carbon_input`, and the balance
  gives it its item's C:N in the polity, else globally
  (`polity_item_cn`, `global_item_cn`). A grass row off the carbon
  balance carries the nearest grassland cell's C input density as
  `c_input_mgc_ha` (Mg C/ha/yr), which the balance puts on the key's
  grass area, stamped `nearest_cell_same_land_use=<km>km`.

- `root_depth_cm` (Meisinger): FAO-56 Table 22's maximum effective
  rooting depth
  ([fao56_root_depth](https://eduaguilera.github.io/whep/reference/fao56_root_depth.md))
  of the row's production items, pooled by WHEP's hectares of each in
  the row's water regime: by default the upper end of each range on
  rainfed rows and the lower end on irrigated rows (Table 22 footnote
  1); an item with no hectares in the regime takes the plain mean over
  its production items (`method_root_depth`).

- `climate` (ATL on wet rows, MED on dry), `irrig_cat` (`"Irrigated"` or
  `"Rainfed"`, from `water_regime`) and `tillage` (`"Tillage"`, the only
  class the Meisinger synthetic block holds; `method_tillage`).

- `surface_runoff_fraction` and `slope_m_per_km` (IMAGE-GNM):
  [`read_image_gnm_runoff()`](https://eduaguilera.github.io/whep/reference/read_image_gnm_runoff.md)
  (`method_image_gnm_runoff`).

- `temp_c_annual_mean` (IMAGE-GNM): the year's CRU mean annual
  temperature, from the climate the classes are built on.

- `soil_texture_class` and `soil_drainage_class` (IMAGE-GNM):
  [`read_soil_texture_fao()`](https://eduaguilera.github.io/whep/reference/read_soil_texture_fao.md)
  and
  [`read_soil_drainage()`](https://eduaguilera.github.io/whep/reference/read_soil_drainage.md)
  (`method_soil_texture_fao`, `method_soil_drainage`).

A balance cell a layer lacks takes the value of the nearest cell that
holds it, by great-circle distance between cell centres, and its stamp
carries the distance (`nearest_lpjml_cell=<km>km`,
`nearest_hwsd_cell=<km>km`, `nearest_image_gnm_cell=<km>km`). A row
without a crop item carries no N: it takes the HWSD topsoil depth (30
cm) and no C input, and a row of that kind carrying N is refused. A row
carrying no N whose soil carbon or root depth is otherwise missing takes
HWSD's topsoil carbon or the topsoil depth, stamped `zero_n_row`; it
leaches nothing.

## Errors

- `whep_n_loss_drivers_rows`: `rows` is missing, empty, lacks a key
  column, or holds a missing coordinate or year, or a `water_regime`
  other than `"rainfed"` or `"irrigated"`.

- `whep_n_loss_drivers_unclassified`: a row is still without a class
  (for example a window-mean PET of zero), named by its stamp.

- `whep_n_loss_drivers_irrigated_cell_class`: an irrigated row carrying
  N, or one whose N is not supplied, has no class of its own and would
  take its cell's precipitation-only class. The rainfed/irrigated split
  gives an irrigated row N only where the item holds irrigated land, so
  such a row means the balance and the moisture regime disagree on the
  item's regime land; the error names the count and the items carrying
  most N.

- Every error of
  [`build_cell_moisture_regime()`](https://eduaguilera.github.io/whep/reference/build_cell_moisture_regime.md),
  including `whep_absent_input` for an absent climate or irrigation
  layer.

- For the leaching drivers, all also `whep_absent_input`:
  `whep_n_loss_drivers_carbon_balance` (no `data$carbon_balance`),
  `whep_n_loss_drivers_soc` and `whep_n_loss_drivers_root_depth` (a row
  carrying N without soil carbon of its land use in any cell, or without
  root depth), `whep_n_loss_drivers_temperature` and
  `whep_n_loss_drivers_layer` (a layer with no complete cell, or without
  a year asked for); a layer whose values are all zero or missing aborts
  as
  [`check_inputs_supplied()`](https://eduaguilera.github.io/whep/reference/check_inputs_supplied.md)
  does. `whep_n_loss_drivers_no_item_n`: a row without a crop item
  carries N.

## Examples

``` r
build_n_loss_drivers(example = TRUE)
#> $n_balance_drivers
#> # A tibble: 5 × 13
#>     lon   lat area_code item_cbs_code  year water_regime fert_type
#>   <dbl> <dbl>     <int>         <int> <int> <chr>        <chr>    
#> 1  9.25  47.8        79          2514  2010 irrigated    Synthetic
#> 2  9.25  47.8        79            NA  2010 rainfed      Synthetic
#> 3  9.75  47.8        79          3000  2010 rainfed      Synthetic
#> 4  9.75  47.8        79          2807  2010 irrigated    Synthetic
#> 5 10.2   47.8        79          2514  2010 rainfed      Synthetic
#> # ℹ 6 more variables: moisture_regime <chr>, climate <chr>, irrig_type <chr>,
#> #   method_moisture_regime <chr>, method_climate <chr>, climate_fill_km <dbl>
#> 
#> $n_balance_leaching_drivers
#> # A tibble: 5 × 9
#>     lon   lat area_code item_cbs_code  year water_regime moisture_regime
#>   <dbl> <dbl>     <int>         <int> <int> <chr>        <chr>          
#> 1  9.25  47.8        79          2514  2010 irrigated    dry            
#> 2  9.25  47.8        79            NA  2010 rainfed      dry            
#> 3  9.75  47.8        79          3000  2010 rainfed      wet            
#> 4  9.75  47.8        79          2807  2010 irrigated    wet            
#> 5 10.2   47.8        79          2514  2010 rainfed      wet            
#> # ℹ 2 more variables: method_moisture_regime <chr>, climate_fill_km <dbl>
#> 
```
