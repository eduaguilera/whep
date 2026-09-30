# Read LPJmL irrigated and rainfed crop yields per cell.

Derives the harvest per unit crop area of the rainfed and the irrigated
stand of each crop in each 0.5-degree cell and year from a finished
LPJmL run, and maps the LPJmL crop functional types (CFTs) onto WHEP
production items. It is the yield ratio that weights the
rainfed/irrigated split of a crop row's synthetic nitrogen, production
nitrogen and residue removals toward the more productive hectares.

The harvest is `pft_harvestc` (harvested carbon excluding residues),
which LPJmL already writes per square metre of the band's own stand; it
is not divided by the stand fraction again. The evidence is in the
header of `R/lpjml_regime_yield.R`: weighted by `cftfrac`, the bands
reproduce the cell total in `harvestc.nc` exactly on grassland-only
cells, while their plain sum overshoots it 1.5 to 26 times.

## Usage

``` r
read_lpjml_regime_yield(
  years = NULL,
  run_dir = NULL,
  data = NULL,
  include_others = FALSE,
  example = FALSE
)
```

## Source

LPJmL 6.1.1 (Potsdam Institute for Climate Impact Research), fork
`lbm364dl/LPJmL` at git hash `e6e6c42b88354f4a0df945e55f2ccf3c4a3cb605`
(as stamped in the output metadata), run
`global_1750-2023_spinup_300_our_inputs_lpjml611_preindustrial_v2`,
outputs `pft_harvestc.nc` and `cftfrac.nc`.

## Arguments

- years:

  Optional integer vector of calendar years. `NULL` reads every year the
  run's `pft_harvestc.nc` carries. A year the run does not cover aborts,
  naming the coverage it has.

- run_dir:

  Path to a finished LPJmL run output directory holding
  `pft_harvestc.nc` and `cftfrac.nc`. `NULL` (default) uses
  `WHEP_LPJML_RUN_DIR`, and with neither set reads the pinned
  `lpjml-crop-regime-yield` layer, built from one LPJmL run together
  with the other LPJmL-derived pins. A run that is named but lacks
  either file aborts rather than falling back to the pin.

- data:

  Optional list used in place of reading a run, for testing: `harvestc`
  (as
  [`read_lpjml_npp()`](https://eduaguilera.github.io/whep/reference/read_lpjml_npp.md)
  returns it for `"harvestc"`) and `stand_frac` (as
  [`read_lpjml_hydrology()`](https://eduaguilera.github.io/whep/reference/read_lpjml_hydrology.md)
  returns it for `"stand_frac"`).

- include_others:

  If `TRUE`, also return LPJmL's `"others"` catch-all stand, expanded to
  the items
  [cft_mapping](https://eduaguilera.github.io/whep/reference/cft_mapping.md)
  puts on it. Defaults to `FALSE`, the crop-specific stands only.

- example:

  If `TRUE`, return a small fixture instead of reading a run. Defaults
  to `FALSE`.

## Value

A tibble with one row per cell, production item and year:

- `lon`, `lat`: cell centre, 0.5-degree grid.

- `year`: calendar year.

- `item_prod_code`, `item_cbs_code`: the WHEP production item and its
  commodity balance item.

- `lpjml_crop`: the LPJmL CFT the item's yield comes from.

- `yield_rainfed`, `yield_irrigated`: harvested carbon, excluding
  residues, in grams of carbon per square metre of that regime's own
  stand per year; `NA` where the regime has no stand in the cell.

- `method_regime_yield`: how the yields were obtained (see *Years before
  1901*).

## Which crops carry a yield

Only production items mapped in
[cft_mapping](https://eduaguilera.github.io/whep/reference/cft_mapping.md)
to one of LPJmL's twelve crop-specific CFTs (temperate and tropical
cereals, rice, maize, pulses, temperate and tropical roots, sugarcane,
and the soybean, groundnut, sunflower and rapeseed oil crops) get rows:
40 items. The other 113 mapped items sit on LPJmL's `"others"` catch-all
stand (fruits, vegetables, nuts, fibres, stimulants, oil palm,
cotton...), whose yield is that of a composite rather than of the crop,
and items absent from
[cft_mapping](https://eduaguilera.github.io/whep/reference/cft_mapping.md)
(fodder crops among them) have no band at all. By default neither gets a
row, so a join against this layer leaves them without a yield, and the
caller has to decide what to use instead.

`include_others = TRUE` adds the `"others"` stand as a crop of its own,
expanded to the items
[cft_mapping](https://eduaguilera.github.io/whep/reference/cft_mapping.md)
puts on it. Its yield is the composite stand's, not the item's. The
regime yield ratio uses it as the year-to-year anomaly source for crops
without a crop-specific CFT, where only the ratio of the two regimes'
yields matters; see
[`build_regime_yield_ratio()`](https://eduaguilera.github.io/whep/reference/build_regime_yield_ratio.md).

The natural key is `item_prod_code`: each production item maps to one
CFT, while four `item_cbs_code`s mix CFTs across their production items
(2520 "Cereals, Other", temperate and tropical cereals; 2534 "Roots,
Other", tropical roots and an unmapped item; 2537 "Sugar beet",
temperate roots and an unmapped item; 2605 "Vegetables, Other", where
only green maize has a band). `item_cbs_code` is carried for joining,
not as a key.

## Zero-area stands

A regime whose stand fraction is zero in a cell has no harvest to
divide, so its yield is `NA`, not zero. A cell-crop-year gets a row when
at least one of the two stands has area. A stand with area and no
harvest (a crop that failed or never matured) keeps its real zero.

## Years before 1901

The run's climate forcing starts in 1901, and LPJmL recycles the
1901-1930 window for 1750-1900 while land use and CO2 stay historical.
Those years are stamped `"lpjml_band_harvest_recycled_climate"` in
`method_regime_yield`; later years `"lpjml_band_harvest"`.

## Rainfed rice

LPJmL waters paddy rice on the rainfed stand too (see
[`read_lpjml_hydrology()`](https://eduaguilera.github.io/whep/reference/read_lpjml_hydrology.md)),
so rainfed rice yields are paddy yields and the rice irrigated:rainfed
ratio sits closer to one than for an unwatered upland stand.

## Examples

``` r
read_lpjml_regime_yield(example = TRUE)
#> # A tibble: 6 × 9
#>      lon   lat  year item_prod_code item_cbs_code lpjml_crop       yield_rainfed
#>    <dbl> <dbl> <int>          <int>         <int> <chr>                    <dbl>
#> 1   80.8  21.8  2010             56          2514 maize                     85.5
#> 2  -93.8  33.2  2010             15          2511 temperate cerea…         283. 
#> 3  113.   35.2  2010            236          2555 oil crops soybe…          11.2
#> 4  107.   39.2  2010             27          2807 rice                      70.8
#> 5 -100.   36.2  2010             15          2511 temperate cerea…         148. 
#> 6   69.2  23.8  2010             15          2511 temperate cerea…          15.5
#> # ℹ 2 more variables: yield_irrigated <dbl>, method_regime_yield <chr>
```
