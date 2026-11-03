# Spatialize a polity-level nitrogen total to crops and grid cells.

Promotes a single polity-total nitrogen input (one row per `year`,
`area_code`, for one fertiliser type) to the crop level by the same
harvested-area-share logic used by
[`build_crop_soil_n2o_extension()`](https://eduaguilera.github.io/whep/reference/build_crop_soil_n2o_extension.md)
(`year`/`area_code`-matched, weighted by each crop's share of harvested
cropland area), then optionally further to the grid level by the crop
allocation the soil carbon inputs place the same crop by
([`build_soil_carbon_inputs()`](https://eduaguilera.github.io/whep/reference/build_soil_carbon_inputs.md)'s
default `method_crop_weights = "spatialized"`): each cell's share of the
polity-crop's area as the spatialization engine
([`build_gridded_landuse()`](https://eduaguilera.github.io/whep/reference/build_gridded_landuse.md))
places it that year, a fodder crop the engine leaves out on the fodder
layer, the production items of a commodity balance item pooled by their
placed hectares. A polity-crop the allocation does not place is spread
over the polity's cropland cells, each weighted by its share of the
polity's placed crop area, so the grid output still re-aggregates to the
polity total. Such reallocations emit a warning naming the affected
crops and the reallocated nitrogen. One allocation for nitrogen and
carbon means the nitrogen a crop receives in a cell and the carbon it
returns there are spread alike (whep#1002).

The allocation is built on the cell support with the polity's share of
each cell as its `cell_area_frac`, because a border cell's cropland
belongs to more than one polity. That share is a polycell's
`land_area_ha` over the cell's total `land_area_ha`, the share of the
cell's land
[`build_polycell_support()`](https://eduaguilera.github.io/whep/reference/build_polycell_support.md)
measures geodesically. The transitional alternative is `polity_frac`,
the subcell-count share
[`build_cell_polity()`](https://eduaguilera.github.io/whep/reference/build_cell_polity.md)
carries, which is quantised to 1/36 of a cell.

The share is of the cell's **land**, never
`land_area_ha / cell_area_ha`: what it splits is already land-only, so
dividing by the whole cell would remove the inland water and ice a
second time. And it is a **share**, never an absolute area.

Because the cell weights are renormalised within each polity-crop-year,
either key redistributes the polity total and neither can change it.
Swapping the key moves **where** the nitrogen lands, never how much of
it there is. A pre-built `data$crop_allocation` is used as supplied, on
the support it was built on.

## Usage

``` r
spatialize_country_n_to_crops(
  country_totals,
  crop_shares,
  cell_polity,
  resolution = c("polity_crop", "grid"),
  split = c("auto", "land_area_ha", "polity_frac"),
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

  The cell-to-polity support (`lon`, `lat`, `area_code`, `cell_area_ha`
  and the `split` key column). Only required when `resolution` includes
  `"grid"`. Rows are keyed on `area_code`:
  [`build_polycell_support()`](https://eduaguilera.github.io/whep/reference/build_polycell_support.md)
  keys on `polity_code` and does not derive the reporting vocabulary
  (DA-23), and `polity_area_crosswalk` folds distinct polities into one
  `area_code` or leaves it `NA`, so a support must be converted to one
  row per cell and `area_code` **before** it is passed here. That
  conversion is refused rather than performed silently. The share
  denominator is the land of the rows supplied, so a caller that simply
  drops the unkeyable polycells renormalises the survivors onto the
  whole cell and hands them hectares that are not theirs, invisibly,
  because the shares still sum to 1. Keep that land in the denominator
  instead, under a bucket no `country_totals` row can join: it then
  dilutes nobody's share and receives no nitrogen.

- resolution:

  Which resolution(s) to return: `"polity_crop"` (default,
  `year`/`area_code`/`item_cbs_code` totals only) or `"grid"` (also
  distributes to `lon`/`lat` grid cells by the crop allocation).

- split:

  Which polity share of the cell weights each grid cell: `"auto"`
  (default) takes `land_area_ha` when the support carries it and
  `polity_frac` otherwise; `"land_area_ha"` and `"polity_frac"` demand
  that key and abort when it is absent. The resolved key is recorded in
  the `method_polity_split` output column, so a table's split is
  readable from the table. Read only at `resolution = "grid"`; the
  `"polity_crop"` output splits nothing across cells.

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
  `resolution = "grid"`: `crop_allocation`, the crop allocation built
  once (a list of `weights`, with `lon`, `lat`, `area_code`,
  `item_prod_code`, `year`, `crop_area_ha`, `area_weight`, and
  `fallback`, with `lon`, `lat`, `area_code`, `year`,
  `cropland_weight`); else `gridded_crops` (a
  [`build_gridded_landuse()`](https://eduaguilera.github.io/whep/reference/build_gridded_landuse.md)
  crop-level output), `engine_inputs` (its `country_areas`,
  `crop_patterns` and `gridded_cropland` inputs) and `fodder_patterns`
  (the fodder layer), each read from its pin when absent.
  `item_cbs_code` in `crop_shares`/`country_totals` is matched to the
  allocation's `item_prod_code` via
  [items_prod_full](https://eduaguilera.github.io/whep/reference/items_prod_full.md)
  (the same crosswalk
  [`build_crop_land_extension()`](https://eduaguilera.github.io/whep/reference/build_crop_land_extension.md)
  uses). Supply `primary_prod` with `year`, geography, `item_prod_code`,
  `unit` and `value` to check each production component's harvested-area
  target. Source and flag columns are retained in the coverage
  diagnostic. `component_policy = "error"` (default) rejects
  missing/conflicting component mappings, invalid or missing targets, or
  a positive crop component absent from a partly represented CBS group.
  `"legacy_pool"` explicitly retains placed-hectare pooling despite
  those gaps and warns with class `whep_crop_component_coverage`; its
  `missing_components` field identifies the affected source rows.
  Whole-CBS cropland fallback remains separately reported. Neither
  policy invents crop-specific support or fills a missing source area
  with zero. Without production-item targets, coverage is stamped
  `"targets_unavailable"`, never asserted complete.

## Value

A tibble. For `resolution = "polity_crop"`: `year`, `area_code`,
`item_cbs_code`, `n_t`. For `resolution = "grid"`: `lon`, `lat`,
`area_code`, `year`, `item_cbs_code`, `n_t`, `method_polity_split` and
`method_crop_components`. Grid output carries a
`crop_component_coverage` attribute with the supplied targets and their
coverage status. `"supplied_targets_covered"` checks only the supplied
component list, not completeness or correctness of the source inventory.
Either gains `reporting_polity_out_of_span` when
`polity_validity = "flag"`; this output carries no reporting-polity
columns, so the flag is attached directly rather than derived from them.

## Annual keys

Supplied totals, crop shares, allocation tables and primary inventories
require finite, nonmissing whole numeric years representable as R
integers. Character years are rejected rather than coerced; validate and
parse source dates before supplying them. Invalid years cannot be
truncated or filtered away to conceal a missing crop component.

## Territorial compatibility

This consumer still uses the numeric geography adapter. Distinct
supplied reporting polities cannot share an area code within the same
year; an ambiguous mapping aborts instead of pooling their observations.
Native identity-keyed crop allocations are not yet consumed by this
public path. Keep identity columns rather than removing them to bypass
these checks; the remaining consumer migration is tracked in issues
\#1483 and \#458.

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
