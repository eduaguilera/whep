# Assemble the per-land-use-class soil carbon inputs.

Build the carbon-input layer
[`build_carbon_balance()`](https://eduaguilera.github.io/whep/reference/build_carbon_balance.md)
consumes, keyed by `(lon, lat, area_code, year, land_use)`. The cropland
class aggregates the per-crop cropland inputs from
[`build_soil_carbon_inputs()`](https://eduaguilera.github.io/whep/reference/build_soil_carbon_inputs.md)
to the class grain: the class carbon density is the
harvested-area-weighted mean of the per-crop densities, and the
humification fraction is the carbon-mass-weighted mean of the per-crop
fractions (mass = density times area). The grassland and natural classes
come from
[`build_grass_natural_carbon_inputs()`](https://eduaguilera.github.io/whep/reference/build_grass_natural_carbon_inputs.md)
unchanged.

## Usage

``` r
build_carbon_inputs(
  resolution = c("grid", "polity"),
  data = list(),
  years = NULL,
  crop_groups = list(),
  density_basis = c("renormalised", "static"),
  method_grazing = c("whep", "lpjml"),
  method_unspatialized = c("reallocate", "drop"),
  example = FALSE
)
```

## Source

Cropland inputs from
[`build_soil_carbon_inputs()`](https://eduaguilera.github.io/whep/reference/build_soil_carbon_inputs.md);
grassland and natural inputs from
[`build_grass_natural_carbon_inputs()`](https://eduaguilera.github.io/whep/reference/build_grass_natural_carbon_inputs.md);
assembled per the WHEP historical carbon-balance design.

## Arguments

- resolution:

  `"grid"` (default, per cell and class) or `"polity"` (aggregated to
  `area_code`, area-weighting the cropland density by the polity crop
  area).

- data:

  Named list of pre-loaded inputs, each falling back to its builder when
  absent: `cropland` (the
  [`build_soil_carbon_inputs()`](https://eduaguilera.github.io/whep/reference/build_soil_carbon_inputs.md)
  output, per cell, crop and year, with `total_c_input_mgc_ha_yr` and
  `humified_fraction`); `crop_area` (per cell, crop and year harvested
  area with columns `lon`, `lat`, `area_code`, `item_prod_code`, `year`,
  `crop_area_ha`, used to area-weight the crop densities);
  `grass_natural` (the
  [`build_grass_natural_carbon_inputs()`](https://eduaguilera.github.io/whep/reference/build_grass_natural_carbon_inputs.md)
  output at the class grain); and optional `land_use` (per-cell class
  `area_ha`, used to area-weight grassland/natural polity output); and
  `country_grid`, the polycell support resolved to one row per cell and
  `area_code`, from which `crop_area` is derived when absent. It is the
  same support
  [`build_soil_carbon_inputs()`](https://eduaguilera.github.io/whep/reference/build_soil_carbon_inputs.md)
  reads, so the weights and the carbon they weight can never come from
  two different crosswalks. When `cropland` or `grass_natural` are
  absent the respective builder is called with the remaining members of
  `data`.

- years:

  Optional integer vector of calendar years to keep. `NULL` (default)
  keeps every year the inputs cover. Threaded into the default
  [`build_soil_carbon_inputs()`](https://eduaguilera.github.io/whep/reference/build_soil_carbon_inputs.md)
  and
  [`build_grass_natural_carbon_inputs()`](https://eduaguilera.github.io/whep/reference/build_grass_natural_carbon_inputs.md)
  builders so their readers slice to the requested years; ignored for
  inputs supplied via `data`.

- crop_groups:

  How cropland is resolved into land-use classes, a named list validated
  element-wise. `method`: `"spain_hist"` (default) resolves cropland
  into crop GROUPS – herbaceous crops pooled per irrigation regime (they
  rotate, so nothing inside the pool is a land-use change), woody crops
  per species, rainfed and irrigated separate – labelled by
  [`soc_crop_group()`](https://eduaguilera.github.io/whep/reference/soc_crop_group.md);
  `"none"` keeps the single `cropland` class the package used before,
  for comparison and for a caller that wants one cropland number.
  `irrigation`: where each crop's irrigated share of its cell area comes
  from. `"spatialized"` (default) uses
  [`build_gridded_landuse()`](https://eduaguilera.github.io/whep/reference/build_gridded_landuse.md)
  on the pinned spatialization inputs, crop-specific and yearly;
  `"none"` puts every crop in its rainfed group. Recorded in
  `method_c_input`. A pre-built share layer can be supplied as
  `data$crop_regime_share` (`lon`, `lat`, `area_code`, `item_prod_code`,
  `year`, `irrigated_share`).

- density_basis:

  Which crop area weights the per-crop densities when they collapse to a
  class. `"renormalised"` (default) uses the yearly cell crop area the
  densities were computed on – the FAOSTAT-renormalised area
  [`build_soil_carbon_inputs()`](https://eduaguilera.github.io/whep/reference/build_soil_carbon_inputs.md)
  returns as `crop_area_ha` – so the class carbon mass equals the sum of
  the crop masses that were spatialized. `"static"` uses the
  time-invariant crop-pattern area split by the polycell's share of the
  cell, which the package used before. The two differ wherever the
  spatialized cell areas of a polity-crop-year do not sum to its FAOSTAT
  harvested area: measured on the 2010 pins over 40,065 cropland cells
  the per-cell class density ratio renormalised/static has an
  area-weighted median of 0.988 (p5-p95 0.922-1.043). Recorded in
  `method_area_basis` on cropland rows.

- method_grazing:

  Whose grazing removes carbon from grassland and returns it as excreta;
  see
  [`build_grass_natural_carbon_inputs()`](https://eduaguilera.github.io/whep/reference/build_grass_natural_carbon_inputs.md).
  `"whep"` (default) charges the class WHEP's own grass intake and
  applied excreta, and so needs `data$livestock_intake` and
  `data$excreta`; `"lpjml"` uses the model's livestock module instead
  and needs neither.

- method_unspatialized:

  What happens to a polity-crop whose crop has no hectares in the
  (time-invariant) `crop_patterns` layer. `"reallocate"` (default)
  spreads that carbon over the polity's crop-pattern cropland cells in
  proportion to each cell's total cropland area, and puts it on the
  crop's FAOSTAT national harvested area, so the polity's gridded carbon
  mass equals its national mass; this is the rule the sibling nitrogen
  spatialization already applies to the same gap
  ([`spatialize_country_n_to_crops()`](https://eduaguilera.github.io/whep/reference/spatialize_country_n_to_crops.md)).
  `"drop"` discards it, which is what the package did before and what a
  caller who prefers a hole to a smear should ask for. Either way the
  mass and the affected crops are reported, and the choice is recorded
  in `method_unspatialized`. Reallocation needs a national area to put
  the carbon on, so a polity-crop with no `harvested_area` row – and a
  polity with no cell at all in the support, which no rule here can
  reach (see whep#1002) – is dropped under both methods, reported
  separately.

- example:

  If `TRUE`, return a small fixture instead of reading remote data.
  Defaults to `FALSE`.

## Value

A tibble keyed by `(lon, lat, area_code, year, land_use)` at `"grid"`
resolution (or `(area_code, year, land_use)` at `"polity"`), with
`c_input_mgc_ha_yr`, `humified_fraction`, `method_c_input` and
`method_unspatialized` (`NA` on the grassland and natural classes, which
are not spatialized from polity-crop totals), for `land_use` in
`"cropland"`, `"grassland"` and `"natural"`, plus the polity columns
below.

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
build_carbon_inputs(example = TRUE)
#> # A tibble: 3 × 13
#>    year area_code polity_area_code reporting_polity_code reporting_polity_name
#>   <int>     <int>            <int> <chr>                 <chr>                
#> 1  2000         1                1 ARM-1991-2025         Armenia              
#> 2  2000         1                1 ARM-1991-2025         Armenia              
#> 3  2000         1                1 ARM-1991-2025         Armenia              
#> # ℹ 8 more variables: reporting_polity_has_geometry <lgl>, lon <dbl>,
#> #   lat <dbl>, land_use <chr>, c_input_mgc_ha_yr <dbl>,
#> #   humified_fraction <dbl>, method_c_input <chr>, method_unspatialized <chr>
```
