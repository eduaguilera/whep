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
  items_prod_full = whep::items_prod_full,
  temp_grassland_basis = c("modelled", "modelled_then_fao", "fao_official", "fao_all",
    "none"),
  fodder_gap = c("as_reported", "carry_forward", "drop")
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
  temporary-grassland section). If `NULL` (default) it is built with
  [`build_grassland_land_extension()`](https://eduaguilera.github.io/whep/reference/build_grassland_land_extension.md)`(grassland_metric = "occupation")`
  so netting still applies (correct but slow); supply the table to skip
  that rebuild, or pass one with no CBS 3002 rows to opt out.

- items_prod_full:

  Crosswalk used to classify `item_cbs_code` as arable or perennial via
  `Herb_Woody`. Defaults to
  [items_prod_full](https://eduaguilera.github.io/whep/reference/items_prod_full.md).

- temp_grassland_basis:

  Which measurement of temporary grassland is netted out of the arable
  target. `"modelled"` (default) is the published behaviour: WHEP's own
  CBS 3002, which exists for 26 EU polities and stops in 2019.
  `"modelled_then_fao"` keeps that and fills every other country-year
  from official FAO 6633. `"fao_official"` uses official FAO 6633
  everywhere, `"fao_all"` uses FAO 6633 on every observation-status flag
  (~81% of it is FAO-imputed), and `"none"` nets nothing, the behaviour
  before whep#349. See the netting-basis section.

- fodder_gap:

  How the FAOSTAT fodder items (CBS `2000`-`2003`) are treated where
  their sources have run out. `"as_reported"` (default) is the published
  behaviour: no fodder from 2020, so ordinary crops absorb its land.
  `"carry_forward"` extends each fodder series' last observed physical
  area over the rest of that country's panel. `"drop"` removes fodder
  from the whole panel. See the fodder-gap section.

## Value

A tibble with columns `year`, `area_code`, `item_cbs_code`, `impact_u`
(fallow-inclusive physical land in hectares), `method_land`
(`"fao_arable_fallow"`), `temp_grassland_netted_ha` (hectares netted out
of that country-year's arable target, `0` where the netting term is
structurally absent), `method_temp_grassland` (the
`temp_grassland_basis` in force) and `method_fodder` (the `fodder_gap`
in force).

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
does exactly this, passing the grassland occupation it has already
built. When `temporary_grassland` is `NULL` (default) the grassland
occupation extension is built internally so netting still happens —
correct but slow, since that build reruns much of the pipeline; supply
the table to avoid the rebuild. Where modelled CBS 3002 exceeds FAO
Arable land (survey vs. fodder-reconstruction mismatch) the arable
target is clamped at 0 and a warning is emitted.

## Netting basis, and the 2019/2020 seam

Modelled CBS 3002 comes from EU AgriDB alone — FAOSTAT production item
996 is in neither production pin, and the EU AgriDB fodder source runs
1961-2019 for all 28 of its region keys. So over 2001-2023 the netting
term exists for **26 EU polities and the years 2001-2019 only**, and is
identically zero everywhere else. For those 26 polities the arable
target therefore steps **from 96.4 Mha in 2019 to 103 Mha in 2020**
while their own FAO arable land *falls*, so ordinary arable crops there
gain land with no land-use change behind it. Measured over 2001-2023 on
the real inputs, the land the netting removes is 8.2-9.8 Mha a year to
2019 and **exactly 0** from 2020. `temp_grassland_basis` exposes the
alternatives measured in whep#937 and whep#354; `"modelled"` remains the
default so this argument changes no published number until a basis is
chosen deliberately. `temp_grassland_netted_ha` in the output, and
[`check_arable_composition()`](https://eduaguilera.github.io/whep/reference/check_arable_composition.md),
make the switch-off visible either way.

FAO's own item 6633 "Temporary meadows and pastures" measures the same
concept, runs 2001-2023, and is what the `"fao_*"` bases read. It is not
a drop-in replacement: only ~19% of it is an official value, Greece and
Poland are imputed zeros throughout while WHEP models 2.10 and 4.78 Mha
there, and its scope is country-dependent — for Ireland, Sweden, the
United Kingdom, the Netherlands, Belgium, Luxembourg and Czechia it
equals WHEP's CBS 3002 to the digit, while for Germany, Italy, Romania,
Spain, Denmark, Austria and Bulgaria it is 3-40 times larger and lands
near the whole green-fodder group. `validation/temp_grassland_6633.R`
records that comparison.

## Fodder gap

FAOSTAT's fodder tonnage (`faostat-production-old`, production only, no
harvested area at all) runs to 2013, whose rows `.combine_fodder()`
drops, so it effectively ends in 2012; EU AgriDB, the only other source,
ends in 2019. Fodder harvested area is reconstructed from those two, so
a build reaching 2020 has **no fodder at all** from that year: measured
over 2001-2023 on the real inputs, fodder is 9.2% of the reconciled
arable land extension in 2001 and 7.6% in 2019, then **0%** from 2020,
with ordinary arable crops absorbing the difference (whep#938). A
second, earlier composition change sits inside the covered window: from
2013 the FAOSTAT-derived fodder area disappears and the dry-matter-yield
estimate jumps from 2.3 to 75.0 Mha, held flat to 2019. `fodder_gap`
exposes the treatments; `"as_reported"` remains the default.

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
#> # A tibble: 2 × 8
#>    year area_code item_cbs_code impact_u method_land       method_temp_grassland
#>   <int>     <int>         <int>    <dbl> <chr>             <chr>                
#> 1  2020         1          2511      400 fao_arable_fallow modelled             
#> 2  2020         1          2560      100 fao_arable_fallow modelled             
#> # ℹ 2 more variables: method_fodder <chr>, temp_grassland_netted_ha <dbl>
```
