# Partition each polycell into mutually exclusive land uses.

Split every polycell-year's `land_area_ha` into `cropland`, `grassland`,
`urban`, `natural` and `unclassified`, so territorial quantities can be
attributed to a land class instead of being assumed agricultural or
dropped (issue \#423).

The **level** of each agricultural class comes from the statistical
record, which is authoritative; LUH2 supplies only the **within-country
spatial pattern**. The two provenances are recorded in separate columns
(`level_source`, `pattern_source`), and their per-polycell difference is
emitted as `statistical_pattern_disagreement_ha` rather than being
absorbed into the natural class. That column is a transition instrument:
its magnitude measures how much LUH2 is still doing, and it is the
criterion for retiring LUH2 as a source.

Inland water and ice are not land uses. They stay on the polycell as
their own categories and never enter a class.

## Usage

``` r
build_polycell_land_uses(
  years = NULL,
  grassland_level_source = c("faostat_pasture", "luh2"),
  overfull_method = c("spillover", "cap"),
  data = list(),
  example = FALSE
)
```

## Arguments

- years:

  Integer vector of calendar years to return, or `NULL` (default) for
  every year present in the support.

- grassland_level_source:

  Grassland level basis, `"faostat_pasture"` (default, FAO Land Use
  item 6655) or `"luh2"`. The default matches the cropland anchor so
  both agricultural classes rest on one convention; `"luh2"` remains
  selectable for sensitivity analysis and is recorded in `level_source`.
  It is never used as a fallback. Note this differs from
  [`build_grassland_land_extension()`](https://eduaguilera.github.io/whep/reference/build_grassland_land_extension.md)'s
  own default, which is `"luh2"`: item 6655 excludes temporary meadows
  and pastures, so the two are a difference in what `grassland` means
  and not only in provenance. The divergence is deliberate and tracked
  in whep#759; this function always passes its choice explicitly rather
  than inheriting a default.

- overfull_method:

  How to reconcile a polycell whose anchored agricultural area exceeds
  its land: `"spillover"` (default) or `"cap"`. See Details.

- data:

  Named list of pre-loaded inputs bypassing the readers:
  `polycell_support` (the
  [`read_polycell_support()`](https://eduaguilera.github.io/whep/reference/read_polycell_support.md)
  table, interval or year grain), `pattern` (the
  [`read_luh2_landuse()`](https://eduaguilera.github.io/whep/reference/read_luh2_landuse.md)
  grid table), `natural_split` (per cell, the non-forested share of
  natural land), `cropland_level` (the
  [`get_arable_permanent_land()`](https://eduaguilera.github.io/whep/reference/get_arable_permanent_land.md)
  table), `grassland_level` (the
  [`build_grassland_land_extension()`](https://eduaguilera.github.io/whep/reference/build_grassland_land_extension.md)
  table) and `temporary_meadows`.

- example:

  If `TRUE`, return a small fixture instead of reading remote data.
  Defaults to `FALSE`.

## Value

A tibble with one row per polycell-year-class: `polycell_id`, `lon`,
`lat`, `polity_code`, `area_code`, `year`, `land_use`, `area_ha`,
`area_source` (`anchored`, `pattern_only`, `residual` or
`unclassified`), `level_source`, `pattern_source`, `allocation_status`,
`statistical_pattern_disagreement_ha` (`NA` where no statistical level
applies), `unplaceable_statistical_ha`, `method_overfull`,
`spillover_max_ring`, `coverage_status`, and the polity reporting
columns carried through from the support.

## Details

Per polycell-year:

1.  Class **shares** come from the LUH2 `fraction` column - LUH2's share
    of the whole cell, which is identical on every polycell of a border
    cell and invariant to the reader's `area_basis`. They are applied to
    the polycell's own measured `land_area_ha`, so the classes tile the
    polycell's land by construction rather than to a tolerance.

2.  FAO counts temporary meadows and pastures (item 6633) inside arable
    land, while LUH2 books that ground as grassland. That component is
    therefore spread over the LUH2 **grassland** pattern but still
    emitted as `cropland`, because FAO's class definition is the one
    being anchored.

3.  Each agricultural class is rescaled so its polycells sum to the
    statistical national total for that `(area_code, year)`.

4.  Where the anchored area exceeds a polycell's land, it is reconciled
    by `overfull_method`, never renormalised in silence.

5.  `natural` takes the remainder of `land_area_ha`.

6.  A polycell with no pattern coverage is `unclassified` in full -
    never `natural`.

Where a statistical level is absent for a class-country-year, the LUH2
pattern level is used and the row is labelled (`level_source = "luh2"`,
`area_source = "pattern_only"`, `allocation_status = "level_missing"`).
It is never silently filled. `urban` has no statistical source and is
always pattern-only.

## Pre-1961 levels

FAOSTAT land use starts in 1961, so an unextended level would step at
that year. Both agricultural classes are therefore backcast the same
way: the FAO 1961 level is carried backwards by LUH2's own national
trend, matching FAO exactly at the splice.
[`get_arable_permanent_land()`](https://eduaguilera.github.io/whep/reference/get_arable_permanent_land.md)
already does this for cropland; the pasture backcast here mirrors it,
reading the same `luh2-areas` input. A country with no FAO 1961 anchor
receives no backcast.

A backcast row is labelled `luh2_backcast_cropland` or
`luh2_backcast_pasture` in `level_source` and is **excluded** from the
statistical-versus-pattern diagnostic, because comparing a backcast with
the pattern compares LUH2 with itself.

## Reconciling an overfull polycell

A national statistical total spread by the LUH2 pattern can give a
polycell more agricultural land than it has, driven by countries where
FAO and LUH2 disagree about how much land is permanent pasture. Measured
on this function at 2020: 63.50 Mha, 1.33% of the anchored agricultural
area, of which Saudi Arabia is 35.10 Mha and Sudan (former) 14.20 Mha.
`overfull_method` selects the treatment and is recorded per row in
`method_overfull`:

- `"spillover"` (default) places the excess on same-country neighbours,
  widening the search ring until it is absorbed, taking non-forested
  natural land (LUH2 `primn`, `secdn`) first and forest (`primf`,
  `secdf`) only as a fallback. A neighbour can only receive a class it
  has a row for, so land the pattern classified nowhere is never quietly
  credited. The ring each hectare reached is reported in
  `spillover_max_ring`; at 2020 it places 63.45 of the 63.50 Mha across
  3,878 receiving polycells, at a median ring of 2 and a maximum of 22,
  and names the remaining 42,765 ha (two polycell-classes, in a country
  whose free land holds neither class) in `unplaceable_statistical_ha`.

- `"cap"` caps the agricultural classes at `land_area_ha` pro rata and
  leaves the whole 63.50 Mha in `unplaceable_statistical_ha`. It is the
  sensitivity baseline that quantifies what spillover buys.

The methods are alternatives, never fallbacks. If `"spillover"` cannot
place a hectare anywhere in the country, it stays in
`unplaceable_statistical_ha` with a warning and the row still reads
`method_overfull = "spillover"`.

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
build_polycell_land_uses(example = TRUE)
#> # A tibble: 11 × 21
#>    polycell_id   lon   lat polity_code area_code  year land_use  area_ha
#>    <chr>       <dbl> <dbl> <chr>           <int> <int> <chr>       <dbl>
#>  1 A-X          0.25  0.25 X-1900-2025        10  2000 cropland       36
#>  2 A-X          0.25  0.25 X-1900-2025        10  2000 grassland       6
#>  3 A-X          0.25  0.25 X-1900-2025        10  2000 natural        15
#>  4 A-X          0.25  0.25 X-1900-2025        10  2000 urban           3
#>  5 A-Y          0.25  0.25 Y-1900-2025        20  2000 cropland       10
#>  6 A-Y          0.25  0.25 Y-1900-2025        20  2000 grassland       4
#>  7 A-Y          0.25  0.25 Y-1900-2025        20  2000 natural        24
#>  8 A-Y          0.25  0.25 Y-1900-2025        20  2000 urban           2
#>  9 B-X          0.75  0.25 X-1900-2025        10  2000 cropland       24
#> 10 B-X          0.75  0.25 X-1900-2025        10  2000 grassland      20
#> 11 B-X          0.75  0.25 X-1900-2025        10  2000 natural        56
#> # ℹ 13 more variables: area_source <chr>, level_source <chr>,
#> #   pattern_source <chr>, allocation_status <chr>,
#> #   statistical_pattern_disagreement_ha <dbl>,
#> #   unplaceable_statistical_ha <dbl>, method_overfull <chr>,
#> #   spillover_max_ring <int>, coverage_status <chr>, polity_area_code <int>,
#> #   reporting_polity_code <chr>, reporting_polity_name <chr>,
#> #   reporting_polity_has_geometry <lgl>
```
