# Build a per-crop physical land extension from CROPGRIDS.

Convert FAOSTAT harvested area into per-crop *physical* cropland using
CROPGRIDS, then return a land extension keyed by
`(year, area_code, item_cbs_code)` for the FABIO footprint model.

CROPGRIDS (Tang et al. 2024) reports, per crop and country, both
**harvested** area and **crop (physical) area** for 2020. Their ratio is
a genuinely per-crop multi-cropping correction — e.g. rice ~0.81
(heavily double-cropped), most other crops ~0.95-1.0 (single-cropped) —
which a single country-level cropping-intensity factor, or a cell-level
apportionment by harvested share, cannot reproduce. This function
applies that per-(area, item) physical / harvested ratio to WHEP
harvested area in each year: \\physical = harvested \times
(physical\_{cg} / harvested\_{cg})\\.

Note: CROPGRIDS physical area is the land where each crop actually
grows; it excludes fallow land (unlike fallow-inclusive
cropland-apportionment), so totals are typically a few percent below
harvested area.

Coverage: crops absent from CROPGRIDS (notably the FAOSTAT fodder items
and a few minor crops) have no physical/harvested ratio and fall back to
a ratio of 1 (physical = harvested, no multi-cropping correction). The
share of harvested area hitting this fallback is reported via a warning.

## Usage

``` r
build_cropgrids_land_extension(
  harvested = NULL,
  cropgrids = NULL,
  source = c("cropgrids", "cropgrids_fallow"),
  max_ratio = 1.5,
  min_cropgrids_ha = 100
)
```

## Arguments

- harvested:

  Tibble of harvested area with columns `year`, `area_code`,
  `item_cbs_code`, `harvested_ha`. If `NULL`, built from
  [`get_primary_production()`](https://eduaguilera.github.io/whep/reference/get_primary_production.md)
  (`unit == "ha"`).

- cropgrids:

  Tibble of national crop areas with columns `area_code`,
  `item_cbs_code`, `physical_ha`, `harvested_ha`. If `NULL`, the remote
  pin selected by `source` is read via
  [`whep_read_file()`](https://eduaguilera.github.io/whep/reference/whep_read_file.md).

- source:

  Which CROPGRIDS pin to read when `cropgrids` is `NULL`: `"cropgrids"`
  (`cropgrids-land`: physical crop area, excludes fallow) or
  `"cropgrids_fallow"` (`cropgrids-fallow-land`: physical area with
  rotational fallow attributed to crops by
  [`attribute_fallow_to_crops()`](https://eduaguilera.github.io/whep/reference/attribute_fallow_to_crops.md)).
  Also recorded in `method_land`.

- max_ratio:

  Cap on the per-area physical/harvested ratio (default `1.5`).
  CROPGRIDS occasionally pairs a normal physical area with a near-zero
  harvested area for minor/aggregate crops, yielding a spurious ratio in
  the hundreds; physical area cannot realistically exceed harvested by
  more than the fallow share, so the ratio is clamped here.

- min_cropgrids_ha:

  Minimum CROPGRIDS harvested area (ha, default `100`) for a per-area
  physical/harvested ratio to be trusted. CROPGRIDS leaves rounding
  stubs of a few hectares for marginal crop-country pairs whose ratio is
  unreliable; below this floor the crop falls through to the global
  per-item ratio instead.

## Value

A tibble with columns `year`, `area_code`, `item_cbs_code`, `impact_u`
(physical land area in hectares), and `method_land`.

## Examples

``` r
harvested <- tibble::tribble(
  ~year, ~area_code, ~item_cbs_code, ~harvested_ha,
  2000L, 33L, 2511L, 1000,
  2000L, 33L, 2807L, 500
)
cropgrids <- tibble::tribble(
  ~area_code, ~item_cbs_code, ~physical_ha, ~harvested_ha,
  33L, 2511L, 990, 1000,
  33L, 2807L, 400, 500
)
build_cropgrids_land_extension(harvested, cropgrids)
#> # A tibble: 2 × 5
#>    year area_code item_cbs_code impact_u method_land
#>   <int>     <int>         <int>    <dbl> <chr>      
#> 1  2000        33          2511      990 cropgrids  
#> 2  2000        33          2807      400 cropgrids  
```
