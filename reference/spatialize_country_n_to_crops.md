# Spatialize a polity-level nitrogen total to crops and grid cells.

Promotes a single polity-total nitrogen input (one row per `year`,
`area_code`, for one fertiliser type) to the crop level by the same
harvested-area-share logic used by
[`build_crop_soil_n2o_extension()`](https://eduaguilera.github.io/whep/reference/build_crop_soil_n2o_extension.md)
(`year`/`area_code`-matched, weighted by each crop's share of harvested
cropland area), then optionally further to the grid level by
distributing each polity-crop total across cells in proportion to the
cell's share of that polity-crop's total crop-pattern area
(`type_ha * harvest_fraction`, summed over LUH2 cropland classes; the
exact formula used by
[`make_lpjml_covariate()`](https://eduaguilera.github.io/whep/reference/make_lpjml_covariate.md)'s
`crop_pattern` weighting). A polity-crop with no crop-pattern hectares
(crop absent from the static pattern raster, or its pattern area sums to
zero in the polity) is instead spread uniformly across the polity's
cropland cells, weighted by each cell's cropland area, so the grid
output still re-aggregates to the polity total. Such reallocations emit
a warning naming the affected crops and the reallocated nitrogen.

## Usage

``` r
spatialize_country_n_to_crops(
  country_totals,
  crop_shares,
  cell_polity,
  resolution = c("polity_crop", "grid"),
  polity_validity = c("keep", "flag", "drop"),
  data = list()
)
```

## Arguments

- country_totals:

  A tibble with `year`, `area_code`, `n_t`: the polity-level nitrogen
  total for one fertiliser type.

- crop_shares:

  A tibble with `year`, `area_code`, `item_cbs_code`, `area_share`:
  harvested-area-weighted crop shares within each country-year, e.g.
  from
  [`build_crop_soil_n2o_extension()`](https://eduaguilera.github.io/whep/reference/build_crop_soil_n2o_extension.md)'s
  internal crop-area-share helper.

- cell_polity:

  The
  [`build_cell_polity()`](https://eduaguilera.github.io/whep/reference/build_cell_polity.md)-shaped
  crosswalk (`lon`, `lat`, `area_code`, `polity_frac`, `cell_area_ha`).
  Only required when `resolution` includes `"grid"`.

- resolution:

  Which resolution(s) to return: `"polity_crop"` (default,
  `year`/`area_code`/`item_cbs_code` totals only) or `"grid"` (also
  distributes to `lon`/`lat` grid cells; requires `crop_patterns` and
  `type_cropland` in `data`).

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

  Optional named list of pre-loaded grid inputs, used only when
  `resolution = "grid"`: `crop_patterns` (`lon`, `lat`,
  `item_prod_code`, `harvest_fraction`) and `type_cropland` (`lon`,
  `lat`, `year`, `luh2_type`, `type_ha`), each falling back to a lazy
  parquet read from `Sys.getenv("WHEP_CROP_PATTERNS_PATH")` /
  `Sys.getenv("WHEP_TYPE_CROPLAND_PATH")` when absent. `item_cbs_code`
  in `crop_shares`/`country_totals` is matched to the `item_prod_code`
  column of `crop_patterns` via
  [items_prod_full](https://eduaguilera.github.io/whep/reference/items_prod_full.md)
  (the same crosswalk
  [`build_crop_land_extension()`](https://eduaguilera.github.io/whep/reference/build_crop_land_extension.md)
  uses).

## Value

A tibble. For `resolution = "polity_crop"`: `year`, `area_code`,
`item_cbs_code`, `n_t`. For `resolution = "grid"`: `lon`, `lat`,
`area_code`, `year`, `item_cbs_code`, `n_t`. Either gains
`reporting_polity_out_of_span` when `polity_validity = "flag"`; this
output carries no reporting-polity columns, so the flag is attached
directly rather than derived from them.

## Examples

``` r
spatialize_country_n_to_crops(
  country_totals = tibble::tribble(
    ~year, ~area_code, ~n_t,
    2010L, 10L, 100
  ),
  crop_shares = tibble::tribble(
    ~year, ~area_code, ~item_cbs_code, ~area_share,
    2010L, 10L, 2511L, 0.7,
    2010L, 10L, 2513L, 0.3
  ),
  cell_polity = NULL,
  resolution = "polity_crop"
)
#> # A tibble: 2 × 4
#>    year area_code item_cbs_code   n_t
#>   <int>     <int>         <int> <dbl>
#> 1  2010        10          2511    70
#> 2  2010        10          2513    30
```
