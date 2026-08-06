# Assemble gridded nitrogen inputs from every WHEP N-input source.

Combines biological nitrogen fixation
([`calculate_bnf()`](https://eduaguilera.github.io/whep/reference/calculate_bnf.md)),
residue/root N recycling
([`calculate_npp_carbon_nitrogen()`](https://eduaguilera.github.io/whep/reference/calculate_npp_carbon_nitrogen.md)),
livestock manure
([`build_livestock_nutrient_flows()`](https://eduaguilera.github.io/whep/reference/build_livestock_nutrient_flows.md)),
atmospheric deposition
([`build_n_deposition()`](https://eduaguilera.github.io/whep/reference/build_n_deposition.md)),
urban/human-excreta N
([`build_urban_n()`](https://eduaguilera.github.io/whep/reference/build_urban_n.md)),
soil organic-matter mineralization
([`build_carbon_balance()`](https://eduaguilera.github.io/whep/reference/build_carbon_balance.md)'s
`son_change_kgn_ha`) and synthetic fertiliser (a country total
spatialized to crops and cells via
[`spatialize_country_n_to_crops()`](https://eduaguilera.github.io/whep/reference/spatialize_country_n_to_crops.md))
into one long-format tibble of nitrogen inputs to agricultural land.

`fert_type` values: `"bnf"`, `"recycling"`, `"manure_solid"`,
`"manure_liquid"`, `"excreta"`, `"deposition"`, `"urban"`,
`"som_mineralization"`, `"synthetic"` and `"accum_loss"`. The last is a
documented gap (perennial-crop standing-biomass N accumulation from
Spain_Hist's N_balance.R): its source computation was not available for
this task, so it is never emitted, only reserved in the vocabulary.

Terms that are fundamentally per-cell or per-land-use rather than
per-crop are allocated over the agricultural land support, either
supplied as `data$ag_land_support` or derived by
[`build_ag_land_support()`](https://eduaguilera.github.io/whep/reference/build_ag_land_support.md)
from the gridded inputs already present. Deposition uses both cropland
and grassland support. `"urban"`, `"som_mineralization"`, and manure
already assigned upstream to Cropland but lacking a crop use only local
cropland support, so manure is not reassigned to grassland after the
manure engine's capacity allocation. Forest and natural land are outside
that support and therefore outside the agricultural balance. Grassland
is represented by CBS 3000; no intensive/extensive class is inferred.

## Usage

``` r
build_n_inputs(
  years = NULL,
  resolution = c("grid", "polity"),
  synthetic_method = NULL,
  data = list(),
  example = FALSE
)
```

## Arguments

- years:

  Optional integer vector of calendar years to keep. `NULL` keeps every
  year the assembled inputs cover.

- resolution:

  `"grid"` (default, per cell/crop/year/fert_type) or `"polity"` (summed
  to `area_code`/`item_cbs_code`/`year`/`fert_type`).

- synthetic_method:

  Synthetic-N crop allocation method, `"coello"` or `"area_share"`. When
  `NULL` (default), uses `data$synthetic_method %||% "coello"` for
  backwards compatibility.

- data:

  Named list of pre-loaded, caller-supplied upstream inputs. Each of the
  following is required for its corresponding `fert_type` to be emitted
  (a missing one silently skips that source rather than erroring, since
  callers may only want a subset):

  - `bnf_input`:
    [`calculate_bnf()`](https://eduaguilera.github.io/whep/reference/calculate_bnf.md)'s
    required input tibble (`lon`, `lat`, `area_code`, `year`,
    `item_prod_code`, `crop_npp_n_t`, `product_n_t`, `weed_npp_n_t`,
    `land_use`, `legumes_seeded`, `seeded_cover_crop_share`, `area_ha`).

  - `npp_n_input`:
    [`calculate_npp_carbon_nitrogen()`](https://eduaguilera.github.io/whep/reference/calculate_npp_carbon_nitrogen.md)'s
    required input tibble (`lon`, `lat`, `area_code`, `year`,
    `item_prod_code`, `item_cbs_code`, `product_dm_t`, `residue_dm_t`,
    `root_dm_t`, optionally `residue_soil_dm_t`).

  - `livestock_intake`:
    [`build_livestock_nutrient_flows()`](https://eduaguilera.github.io/whep/reference/build_livestock_nutrient_flows.md)'s
    `intake` argument (the
    [`redistribute_feed()`](https://eduaguilera.github.io/whep/reference/redistribute_feed.md)
    realised-intake contract), plus `gridded` (its land-surface layer)
    and `resolution`/`methods` (forwarded as-is).

  - `nhx`, `noy`, `cell_polity`:
    [`build_n_deposition()`](https://eduaguilera.github.io/whep/reference/build_n_deposition.md)'s
    inputs.

  - `ag_land_support`: agricultural physical land support keyed by
    `lon`, `lat`, `area_code`, `year`, `item_cbs_code`, with `land_use`
    (`"cropland"` or `"grassland"`) and positive `area_ha`. Optional:
    when absent it is derived natively by
    [`build_ag_land_support()`](https://eduaguilera.github.io/whep/reference/build_ag_land_support.md)
    from `cell_polity`, `type_cropland` and `crop_patterns` (plus
    `states` or `grassland_ha` for the grassland side). Supply it to
    override that derivation with a better land surface. Cropland rows
    identify crop CBS items; all pasture/rangeland rows use CBS 3000.

  - `grassland_source`, `gridded_pasture`, `grassland_ha`, `states`:
    forwarded to
    [`build_ag_land_support()`](https://eduaguilera.github.io/whep/reference/build_ag_land_support.md)
    when the support is derived. `grassland_source` selects its
    `grassland` argument (`"gridded_pasture"` default, `"luh2"`, or
    `"none"` for cropland-only support).

  - `urban_population`, `cropland_ha`, `cell_polity`:
    [`build_urban_n()`](https://eduaguilera.github.io/whep/reference/build_urban_n.md)'s
    inputs.

  - `carbon_balance`:
    [`build_carbon_balance()`](https://eduaguilera.github.io/whep/reference/build_carbon_balance.md)'s
    `"grid"`-resolution output (`lon`, `lat`, `area_code`, `land_use`,
    `year`, `area_ha`, `son_change_kgn_ha`); this driver requires it
    supplied directly, it is never computed here.

  - `primary_prod`, `fertilizer`, `crop_patterns`, `type_cropland`,
    `cell_polity`: the synthetic-fertiliser assembly (country total from
    `fertilizer`, the `faostat-fertilizer-nutrients` pin, split to crops
    by the chosen crop-share method, then to cells by
    `crop_patterns`/`type_cropland`).

  - `synthetic_method`: how the synthetic-N country total is split
    across crops, `"coello"` (default; Coello 2025 rate-weighted,
    FAOSTAT- conserving) or `"area_share"` (harvested-area shares only).

  - `coello_rates`: crop-specific synthetic-N rate table shaped like
    [coello_synthetic_n](https://eduaguilera.github.io/whep/reference/coello_synthetic_n.md)
    (`year`, `area_code`, `item_cbs_code`, `kg_n_ha`); defaults to
    [`whep::coello_synthetic_n`](https://eduaguilera.github.io/whep/reference/coello_synthetic_n.md).
    Used only when `synthetic_method = "coello"`.

  - `gridded`, `resolution`, `methods`: forwarded to
    [`build_livestock_nutrient_flows()`](https://eduaguilera.github.io/whep/reference/build_livestock_nutrient_flows.md).
    `resolution` is the manure engine's own axis, not this function's:
    it defaults to `"subnational"` at `resolution = "grid"` (cell-level
    nitrogen needs cell-level manure) and to `"national"` otherwise. A
    value supplied here is always honoured.

- example:

  If `TRUE`, return a small fixture instead of assembling real data.
  Defaults to `FALSE`.

## Value

A tibble. At `resolution = "grid"`: `lon`, `lat`, `area_code`,
`item_cbs_code`, `year`, `fert_type`, `n_input_t`, `method_recycling_n`,
`method_synthetic`. At `resolution = "polity"`: `area_code`,
`item_cbs_code`, `year`, `fert_type`, `method_recycling_n`,
`method_synthetic`, `n_input_t` (summed over cells).
`method_recycling_n` records which residue basis the `"recycling"` term
used: `"residue_soil_returned"` when the upstream NPP input supplied
`residue_soil_dm_t` (residue N net of removal for feed/fuel/burning) or
`"total_residue"` when only gross residue N was available; it is `NA`
for every other `fert_type`. `method_synthetic` records the synthetic
crop-split basis (`"coello"` or `"area_share"`) on `"synthetic"` rows
and is `NA` for every other `fert_type`. Both grains also carry the
polity columns below.

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
for the reasoning.

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
build_n_inputs(example = TRUE)
#> # A tibble: 9 × 13
#>    year area_code polity_area_code reporting_polity_code reporting_polity_name
#>   <int>     <int>            <int> <chr>                 <chr>                
#> 1  2020         1                1 ARM-1991-2025         Armenia              
#> 2  2020         1                1 ARM-1991-2025         Armenia              
#> 3  2020         1                1 ARM-1991-2025         Armenia              
#> 4  2020         1                1 ARM-1991-2025         Armenia              
#> 5  2020         1                1 ARM-1991-2025         Armenia              
#> 6  2020         1                1 ARM-1991-2025         Armenia              
#> 7  2020         1                1 ARM-1991-2025         Armenia              
#> 8  2020         1                1 ARM-1991-2025         Armenia              
#> 9  2020         1                1 ARM-1991-2025         Armenia              
#> # ℹ 8 more variables: reporting_polity_has_geometry <lgl>, lon <dbl>,
#> #   lat <dbl>, item_cbs_code <int>, fert_type <chr>, n_input_t <dbl>,
#> #   method_recycling_n <chr>, method_synthetic <chr>
```
