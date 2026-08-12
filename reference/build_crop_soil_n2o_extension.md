# Build the crop/soil N2O extension.

Estimate IPCC 2019 Tier 1 nitrous-oxide emissions from nitrogen applied
to managed soils, as a footprint extension keyed by
`(year, area_code, item_cbs_code)` in kilograms of carbon-dioxide
equivalent (CO2e). This is the soil-N2O analogue of
[`build_livestock_ghg_extension()`](https://eduaguilera.github.io/whep/reference/build_livestock_ghg_extension.md)
and feeds
[`build_footprint()`](https://eduaguilera.github.io/whep/reference/build_footprint.md)
/
[`compute_footprint()`](https://eduaguilera.github.io/whep/reference/compute_footprint.md)
the same way.

Three nitrogen inputs to soil are included:

- **Synthetic fertiliser** (F_SN): FAOSTAT reports it only as a country
  total (tonnes N per `area_code` per year), so it is allocated to crops
  by the Coello 2025 rate-weighted, FAOSTAT-conserving crop share
  (default; the national total is preserved), or by harvested-area share
  when `synthetic_method = "area_share"`.

- **Applied manure** (F_ON): FAOSTAT "Manure applied to soils (N
  content)" country total, allocated to crops by harvested area (Coello
  is a synthetic-N rate basis only).

- **Crop residues** (F_CR): the dry matter of above-ground residues
  returned to soil (from
  [`get_primary_residues()`](https://eduaguilera.github.io/whep/reference/get_primary_residues.md),
  net of the removed fraction) times the crop's residue nitrogen content
  (IPCC 2019 Table 11.1a).

Both country totals are read under raw FAOSTAT `Area Code` values and
harmonised to the whep polity `area_code` through
[polity_area_crosswalk](https://eduaguilera.github.io/whep/reference/polity_area_crosswalk.md)
before they are split to crops, so reporting units that FABIO folds into
one bucket are summed rather than dropped (Sudan 276 + South Sudan 277
to 206, Ethiopia PDR 62 to 238, the small territories that fold into
"rest of world" to 999). FAOSTAT rows that are not territories carry no
polity and are dropped: 5000 "World", the continent, region, EU-27, OECD
and income-group rollups, and the "China" aggregate 351, which overlaps
41/96/128/214. This is a documented exception rather than a coverage
gap, since a rollup has no crop shares of its own and would double count
its members.

N2O is then estimated with IPCC 2019 Refinement (Vol 4, Ch 11) Tier 1
factors (climate-aggregated): direct `EF1 = 0.010`; indirect via
volatilisation `EF4 = 0.010` applied to the volatilised fraction
(`FracGASF = 0.11` for synthetic, `FracGASM = 0.21` for manure; crop
residues do not volatilise, Eq 11.9); indirect via leaching
`FracLEACH = 0.24` times `EF5 = 0.011`. N2O-N is converted to N2O by
44/28 and to CO2e with the chosen GWP100.

Manure deposited by grazing animals (F_PRP, which uses the grazing EF3
on pasture) and below-ground residue N are further Tier 1 inputs not yet
included.

## Usage

``` r
build_crop_soil_n2o_extension(
  gwp = c("ar6", "ar5", "ar4"),
  residue_removed_frac = 0.45,
  synthetic_method = NULL,
  data = list(),
  example = FALSE
)
```

## Arguments

- gwp:

  100-year global warming potential standard for N2O, `"ar6"` (default,
  273), `"ar5"` (265) or `"ar4"` (298).

- residue_removed_frac:

  Fraction of above-ground crop residue removed from the field (for
  feed, fuel or construction) and therefore not returned to soil.
  Defaults to `0.45`, a global mid-range value; country-specific removal
  (`gleam_fracremove`) is a future refinement.

- synthetic_method:

  Synthetic-N crop allocation method, `"coello"` or `"area_share"`. When
  `NULL` (default), uses `data$synthetic_method %||% "coello"` for
  backwards compatibility.

- data:

  Optional named list of pre-loaded inputs to avoid remote reads:
  `primary_prod`
  ([`get_primary_production()`](https://eduaguilera.github.io/whep/reference/get_primary_production.md),
  for harvested area), `fertilizer` (the `faostat-fertilizer-nutrients`
  pin), `manure` (the `faostat-emissions-livestock` pin) and
  `primary_residues`
  ([`get_primary_residues()`](https://eduaguilera.github.io/whep/reference/get_primary_residues.md)).
  Each falls back to its reader when absent. `synthetic_method` selects
  the synthetic-N crop split, `"coello"` (default; Coello 2025
  rate-weighted, FAOSTAT-conserving) or `"area_share"`; `coello_rates`
  overrides the rate table (shaped like
  [coello_synthetic_n](https://eduaguilera.github.io/whep/reference/coello_synthetic_n.md)),
  defaulting to
  [`whep::coello_synthetic_n`](https://eduaguilera.github.io/whep/reference/coello_synthetic_n.md).

- example:

  If `TRUE`, return a small fixture instead of reading remote data.
  Defaults to `FALSE`.

## Value

A tibble with columns `year`, `area_code`, `item_cbs_code`, `impact_u`
(soil N2O in kilograms CO2e) and `method_soil_n2o`, plus the polity
columns below.

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
build_crop_soil_n2o_extension(example = TRUE)
#> # A tibble: 2 × 10
#>    year area_code polity_area_code reporting_polity_code reporting_polity_name
#>   <int>     <int>            <int> <chr>                 <chr>                
#> 1  2010        10               10 AUS-1901-2025         Australia            
#> 2  2010        10               10 AUS-1901-2025         Australia            
#> # ℹ 5 more variables: reporting_polity_has_geometry <lgl>, item_cbs_code <int>,
#> #   impact_u <dbl>, method_soil_n2o <chr>, method_synthetic <chr>
```
