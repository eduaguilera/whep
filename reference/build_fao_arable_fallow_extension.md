# Build a per-crop physical land extension with FAO fallow-inclusive arable land.

Turn per-crop harvested-derived physical area into a fallow-inclusive
physical land extension whose arable-crop total reconciles to FAO's
physical **Arable land** and whose perennial-crop total reconciles to
FAO's physical **Permanent crops**
([`get_arable_permanent_land()`](https://eduaguilera.github.io/whep/reference/get_arable_permanent_land.md)),
per `(area_code, year)`.

This is the FAO-land-base analogue of
[`build_cropgrids_land_extension()`](https://eduaguilera.github.io/whep/reference/build_cropgrids_land_extension.md)`(source = "cropgrids_fallow")`.
The existing method takes the fallow *magnitude* from FAOSTAT "Temporary
fallow" (item 6640, a sparse and, for many rain-fed economies, absent
series) applied to a single CROPGRIDS 2020 snapshot. Here the fallow
magnitude is the physical arable land that carried no harvest in that
specific year, `FAO Arable land - sum(cropped arable physical)`, so a
drought year's resting cropland is charged to the crops whose rotation
it supports and the arable-crop footprint totals match FAO's land survey
in every year (see the Tunisia/Portugal motivation in
[`get_arable_permanent_land()`](https://eduaguilera.github.io/whep/reference/get_arable_permanent_land.md)).

Reconciliation, per `(area_code, year)`:

- **Arable crops** (`items_prod_full$Herb_Woody != "Woody"`): rotational
  fallow `max(0, arable_ha - S)` (with `S` the cropped arable physical
  total) is distributed with
  [`attribute_fallow_to_crops()`](https://eduaguilera.github.io/whep/reference/attribute_fallow_to_crops.md)
  using `fallow_weights`, so the arable total reaches `arable_ha`. Where
  the cropped physical already exceeds `arable_ha` (heavy
  multi-cropping, or inflated fodder harvested area) there is no fallow
  to add and the arable crops are scaled down to `arable_ha` instead,
  the physical-container correction. Either way the arable total equals
  FAO `arable_ha` by construction.

- **Perennial crops** (`Herb_Woody == "Woody"`) receive no fallow and
  are scaled so their total equals FAO `permanent_ha`, preserving the
  within-group physical pattern. A positive target without a
  corresponding arable crop row or positive perennial base area is
  reported as an error because it cannot be reconciled without inventing
  a crop allocation.

This is the crop-side default of the land-balance footprint
([`build_land_balance_footprint()`](https://eduaguilera.github.io/whep/reference/build_land_balance_footprint.md)).

## Usage

``` r
build_fao_arable_fallow_extension(
  harvested = NULL,
  arable_permanent = NULL,
  base_extension = NULL,
  fallow_weights = NULL,
  temporary_grassland = NULL,
  items_prod_full = whep::items_prod_full
)
```

## Arguments

- harvested:

  Tibble of harvested area with columns `year`, `area_code`,
  `item_cbs_code`, `harvested_ha`. If `NULL`, built from
  [`get_primary_production()`](https://eduaguilera.github.io/whep/reference/get_primary_production.md)
  (`unit == "ha"`); passing a cached harvested table avoids that
  rebuild.

- arable_permanent:

  Tibble of FAO physical land base with columns `area_code`, `year`,
  `arable_ha`, `permanent_ha`. If `NULL`,
  [`get_arable_permanent_land()`](https://eduaguilera.github.io/whep/reference/get_arable_permanent_land.md)
  is called for the years present in `base_extension`.

- base_extension:

  Tibble of cropped (fallow-excluding) per-crop physical area with
  columns `year`, `area_code`, `item_cbs_code`, `impact_u`. If `NULL`,
  built with
  [`build_cropgrids_land_extension()`](https://eduaguilera.github.io/whep/reference/build_cropgrids_land_extension.md)`(source = "cropgrids")`
  from `harvested`.

- fallow_weights:

  Tibble of `area_code`, `item_cbs_code`, `weight` giving the
  within-country fallow allocation weight, e.g. from
  [`gridded_fallow_weights()`](https://eduaguilera.github.io/whep/reference/gridded_fallow_weights.md)
  (the recommended agro-climatic, rainfed-gated weight). If `NULL`,
  fallow is distributed in proportion to each arable crop's cropped
  physical area (perennials always excluded). The cropped-area fallback
  is used independently for an area when it has no usable supplied
  weights, a non-finite or negative supplied weight, or a non-positive
  total.

- temporary_grassland:

  Tibble of grassland occupation in the
  [`build_grassland_land_extension()`](https://eduaguilera.github.io/whep/reference/build_grassland_land_extension.md)
  schema (`area_code`, `year`, `item_cbs_code`, `impact_u`); its CBS
  3002 rows are the temporary grassland netted out of the arable target
  so ordinary crops plus CBS 3002 reconcile to FAO Arable land (see the
  temporary-grassland section). If `NULL` (default) no netting is
  applied and arable crops reconcile to the full FAO Arable land; the
  land-balance footprint supplies the grassland occupation here.

- items_prod_full:

  Crosswalk used to classify `item_cbs_code` as arable or perennial via
  `Herb_Woody`. Defaults to
  [items_prod_full](https://eduaguilera.github.io/whep/reference/items_prod_full.md).

## Value

A tibble with columns `year`, `area_code`, `item_cbs_code`, `impact_u`
(fallow-inclusive physical land in hectares), and `method_land`
(`"fao_arable_fallow"`).

## Temporary grassland (no double-count)

FAO's **Arable land** total includes *temporary meadows and pastures* —
temporary grassland is part of cropland, not grassland. That land is
also reported separately as CBS 3002 (`Temporary grassland`) by
[`build_grassland_land_extension()`](https://eduaguilera.github.io/whep/reference/build_grassland_land_extension.md),
so summing both extensions naively would count it twice. Pass that
grassland occupation as `temporary_grassland` and its CBS 3002 is netted
out of the arable target before reconciling ordinary crops, enforcing
the invariant per `(area_code, year)`
`ordinary crop occupation (incl. fallow) + CBS 3002 = FAO Arable land`.
The land-balance footprint
([`build_land_balance_footprint()`](https://eduaguilera.github.io/whep/reference/build_land_balance_footprint.md))
does exactly this. Where modelled CBS 3002 exceeds FAO Arable land
(survey vs. fodder-reconstruction mismatch) the arable target is clamped
at 0 and a warning is emitted. Called standalone
(`temporary_grassland = NULL`) no netting is applied and the arable
crops reconcile to the full FAO Arable land.

## Examples

``` r
harvested <- tibble::tribble(
  ~year, ~area_code, ~item_cbs_code, ~harvested_ha,
  2020L, 1L, 2511L, 300, # wheat (arable)
  2020L, 1L, 2560L, 100 # coconuts (perennial)
)
base_extension <- tibble::tribble(
  ~year, ~area_code, ~item_cbs_code, ~impact_u,
  2020L, 1L, 2511L, 300,
  2020L, 1L, 2560L, 100
)
arable_permanent <- tibble::tribble(
  ~area_code, ~year, ~arable_ha, ~permanent_ha,
  1L, 2020L, 500, 100
)
items <- tibble::tribble(
  ~item_cbs_code, ~Herb_Woody,
  2511L, "Herbaceous",
  2560L, "Woody"
)
temporary_grassland <- tibble::tribble(
  ~area_code, ~year, ~item_cbs_code, ~impact_u,
  1L, 2020L, 3002L, 100 # temporary grassland netted out of arable
)
build_fao_arable_fallow_extension(
  harvested, arable_permanent, base_extension,
  temporary_grassland = temporary_grassland,
  items_prod_full = items
)
#> # A tibble: 2 × 5
#>    year area_code item_cbs_code impact_u method_land      
#>   <int>     <int>         <int>    <dbl> <chr>            
#> 1  2020         1          2511      400 fao_arable_fallow
#> 2  2020         1          2560      100 fao_arable_fallow
```
