# Build per-crop physical cropland extension.

Convert gridded crop *harvested* area into per-crop *physical* land area
and aggregate it to commodity-balance items, producing a land extension
keyed by `(year, area_code, item_cbs_code)` for the FABIO footprint
model.

The gridded land-use pipeline
([`build_gridded_landuse()`](https://eduaguilera.github.io/whep/reference/build_gridded_landuse.md))
distributes FAOSTAT harvested area across grid cells; per-crop totals
therefore conserve to harvested area, which over-counts multi-cropped
land and under-counts fallow. This function turns that harvested area
into physical occupied land:

- `"cropland_apportion"` (default): within each cell, the cell's
  physical cropland (`cropland_ha`, from LUH2) is split across crops in
  proportion to their share of the cell's harvested area. Per-crop
  physical area then conserves to physical cropland rather than to
  harvested area, capturing both double-cropping (scaled down) and
  fallow (resting land charged to the crops the rotation supports) at
  the resolution of the grid.

- `"intensity_divide"`: each crop's harvested area is divided by the
  cell multi-cropping intensity (`mc_rainfed`, `mc_irrigated`). Requires
  `multicropping`.

Unlike a single country-level cropping-intensity factor applied
uniformly to every crop, both methods distribute physical cropland by
the actual spatial pattern of each crop.

Coverage note: with `"cropland_apportion"` the per-country crop total is
bounded by the LUH2 `cropland` layer, which can under-represent
perennial or plantation crops (e.g. oil palm, rubber) classified outside
cropland; such crops may receive less land than a harvested-area
baseline implies.

## Usage

``` r
build_crop_land_extension(
  gridded_crops,
  gridded_cropland,
  items_prod_full = whep::items_prod_full,
  method = c("cropland_apportion", "intensity_divide"),
  multicropping = NULL
)
```

## Arguments

- gridded_crops:

  Tibble of gridded crop harvested area, the crop-level output of
  [`build_gridded_landuse()`](https://eduaguilera.github.io/whep/reference/build_gridded_landuse.md)
  (built without CFT aggregation). Must have columns `lon`, `lat`,
  `year`, `area_code`, `item_prod_code`, `rainfed_ha`, `irrigated_ha`.

- gridded_cropland:

  Tibble of physical cropland per cell. Must have columns `lon`, `lat`,
  `year`, `cropland_ha`.

- items_prod_full:

  Crosswalk from production items to commodity-balance items. Defaults
  to
  [items_prod_full](https://eduaguilera.github.io/whep/reference/items_prod_full.md).
  Must have columns `item_prod_code` and `item_cbs_code`.

- method:

  Physical-area conversion method. One of `"cropland_apportion"`
  (default) or `"intensity_divide"`.

- multicropping:

  Tibble of per-cell multi-cropping intensity, required for
  `method = "intensity_divide"`. Must have columns `lon`, `lat`,
  `mc_rainfed`, `mc_irrigated` (and optionally `year`).

## Value

A tibble with columns `year`, `area_code`, `item_cbs_code`, `impact_u`
(physical land area in hectares), and `method_land` (the chosen method).

## Examples

``` r
gridded_crops <- tibble::tribble(
  ~lon, ~lat, ~year, ~area_code, ~item_prod_code, ~rainfed_ha, ~irrigated_ha,
  0.25, 50.25, 2000L, 1L, 15L, 600, 0,
  0.25, 50.25, 2000L, 1L, 27L, 200, 0,
  0.75, 50.25, 2000L, 1L, 15L, 400, 0
)
gridded_cropland <- tibble::tribble(
  ~lon, ~lat, ~year, ~cropland_ha,
  0.25, 50.25, 2000L, 1000,
  0.75, 50.25, 2000L, 500
)
items <- tibble::tribble(
  ~item_prod_code, ~item_cbs_code,
  15L, 2511L,
  27L, 2805L
)
build_crop_land_extension(gridded_crops, gridded_cropland, items_prod_full = items)
#> # A tibble: 2 × 5
#>    year area_code item_cbs_code impact_u method_land       
#>   <int>     <int>         <int>    <dbl> <chr>             
#> 1  2000         1          2511     1250 cropland_apportion
#> 2  2000         1          2805      250 cropland_apportion
```
