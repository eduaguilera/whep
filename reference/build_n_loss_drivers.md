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
  water = c("precip_irrig", "precip"),
  data = NULL,
  config = list(),
  example = FALSE
)
```

## Source

IPCC (2019), 2019 Refinement to the 2006 IPCC Guidelines for National
Greenhouse Gas Inventories, Volume 4, Chapter 11, Table 11.1, footnote 4
(p. 11.13).

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
  `moisture_regime`, `method_moisture_regime` and `climate_fill_km`.

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
| `leaching`: `"meisinger_drainage"` | Meisinger's, see below | no |

The two methods it does not supply are later work. MANNER reads
`manner_fertiliser`, `soil_ph`, `rate_kg_ha`, `rainfall_mm`,
`irrigated`, `temp_c`, `temp_c_annual_mean`, `windspeed_ms`,
`technique`, `system`, `incorporation_delay_h` and `species` (see
[`calculate_nh3()`](https://eduaguilera.github.io/whep/reference/calculate_nh3.md));
Meisinger reads `fert_type`, `climate`, `irrig_cat`, `land_use`,
`cn_input`, `tillage`, `som_share` and `drainage_mm` (see
[`calculate_n_leaching()`](https://eduaguilera.github.io/whep/reference/calculate_n_leaching.md)).
[`build_nitrogen_balance()`](https://eduaguilera.github.io/whep/reference/build_nitrogen_balance.md)
refuses either before assembling any input unless its driver table is
supplied.

`method_moisture_regime` is
[`build_cell_moisture_regime()`](https://eduaguilera.github.io/whep/reference/build_cell_moisture_regime.md)'s
stamp. A row that took its cell's precipitation-only class carries the
extra field `cell_rainfed_class=no_crop_item` (no crop item) or
`cell_rainfed_class=no_regime_area` (an item without regime land of the
row's water regime), and a row in a cell that took climate variables
from its nearest CRU cell the field `nearest_cru_cell=<col>[+<col>...]`,
naming them.

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
