# Build the livestock energy-use CO2 footprint extension (meat only).

Aggregate GLEAM 3.0 on-farm (direct) and feed-production (embedded)
energy use into a footprint extension keyed by
`(year, area_code, item_cbs_code)`, expressed in kilograms of
carbon-dioxide equivalent (CO2e). This is the energy slice of the
livestock greenhouse-gas basket and is designed to be summed with
[`build_livestock_ghg_extension()`](https://eduaguilera.github.io/whep/reference/build_livestock_ghg_extension.md)
(enteric and manure CH4/N2O), which keys on the same live-animal
sectors.

The GLEAM energy emission factors are expressed per kilogram of **live
weight** (see
[gleam_energy_use_ef](https://eduaguilera.github.io/whep/reference/gleam_energy_use_ef.md)),
which is well defined for meat but not for milk or eggs, so the
extension covers **meat only**: bovine (`item_cbs_code` 961 non-dairy
cattle and 946 buffalo), sheep (976) and goat (1016), pig (1049 and
1051) and broiler-chicken (1053) meat. Milk and eggs keep their CH4/N2O
but get no energy CO2.

For each meat group the live weight produced is recovered from FAOSTAT
carcass production divided by a GLEAM dressing fraction
([gleam_dressing_percentages](https://eduaguilera.github.io/whep/reference/gleam_dressing_percentages.md)),
multiplied by a per-country energy intensity (embedded + direct), and
then attributed to the contributing live-animal sectors in proportion to
their slaughtered head counts. Because GLEAM reports its factors by
production system and climate zone but the package has no country-level
system or climate shares, the intensities are collapsed to one value per
country by an unweighted mean across systems and climate zones; this
choice is recorded in `method_energy`.

`gleam_geographic_hierarchy` is the country universe of the whole
extension, so a reporting area absent from it gets no energy intensity
and its meat production leaves the extension. That affects both the
aggregate reporting buckets (`polity_area_code` 999 "Rest of World" and
the continental residuals 901-906) and the dissolved entities GLEAM's
present-day country table cannot carry (USSR, Czechoslovakia,
Yugoslavia, Belgium-Luxembourg, Serbia and Montenegro). The size of that
loss is now **reported** on every build rather than left to be inferred,
and `unclassified = "global_mean"` prices those areas at the world-mean
GLEAM intensity instead of losing them. The default keeps the historical
behaviour; see whep#492.

## Usage

``` r
build_energy_co2_extension(
  method = c("gleam"),
  data = list(),
  unclassified = c("drop", "global_mean"),
  example = FALSE
)
```

## Arguments

- method:

  Estimation method. Only `"gleam"` (default), the GLEAM 3.0
  per-live-weight factors, is currently available.

- data:

  Optional named list of pre-loaded inputs to avoid remote reads:
  `primary_prod` (the
  [`get_primary_production()`](https://eduaguilera.github.io/whep/reference/get_primary_production.md)
  output). It falls back to its reader when absent.

- unclassified:

  How to treat reporting areas `gleam_geographic_hierarchy` has no row
  for, and which therefore get no country energy intensity. `"drop"`
  (default) keeps the historical behaviour: their meat production leaves
  the extension, and a warning says how much. `"global_mean"` prices
  them at the unweighted world mean of the published GLEAM factors
  instead, marking those rows `"GLEAM_3.0_energy_meat_global_mean"` in
  `method_energy`.

- example:

  If `TRUE`, return a small fixture instead of reading remote data.
  Defaults to `FALSE`.

## Value

A tibble with columns `year`, `area_code`, `item_cbs_code`, `impact_u`
(energy-use emissions in kilograms CO2e) and `method_energy`
(`"GLEAM_3.0_energy_meat"`, or `"GLEAM_3.0_energy_meat_global_mean"` for
rows priced at the world mean), plus the polity columns below.

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
build_energy_co2_extension(example = TRUE)
#> # A tibble: 8 × 9
#>    year area_code polity_area_code reporting_polity_code reporting_polity_name  
#>   <int>     <int>            <int> <chr>                 <chr>                  
#> 1  2010        21               21 BRA-1909-2025         Brazil                 
#> 2  2010        21               21 BRA-1909-2025         Brazil                 
#> 3  2010       231              231 USA-1959-2025         United States of Ameri…
#> 4  2010       231              231 USA-1959-2025         United States of Ameri…
#> 5  2010       231              231 USA-1959-2025         United States of Ameri…
#> 6  2010       231              231 USA-1959-2025         United States of Ameri…
#> 7  2010       231              231 USA-1959-2025         United States of Ameri…
#> 8  2010       231              231 USA-1959-2025         United States of Ameri…
#> # ℹ 4 more variables: reporting_polity_has_geometry <lgl>, item_cbs_code <int>,
#> #   impact_u <dbl>, method_energy <chr>
```
