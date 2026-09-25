# Build gridded human-population nitrogen inputs to agriculture.

Estimates the nitrogen the human population returns to agricultural land
through municipal solid waste, sewage sludge and human excreta, per WHEP
0.5-degree grid cell. Each polycell's population is converted to a
nitrogen load via a per-capita rate interpolated from a national
historical benchmark series, the Spanish series taken as reference
([human_n_reference](https://eduaguilera.github.io/whep/reference/human_n_reference.md);
see Details), then spilled from cells with no local cropland room to
same-polity neighbouring cells with spare capacity via
[`allocate_manure_transport()`](https://eduaguilera.github.io/whep/reference/allocate_manure_transport.md),
the same buffering used by the manure engine.

The population and the rate are chosen together by `population_basis`,
so a per-urban-inhabitant rate is never applied to a total population:

- `"total"` (default): UN WPP total population downscaled by HYDE's
  total-population pattern
  ([`build_total_population_grid()`](https://eduaguilera.github.io/whep/reference/build_total_population_grid.md))
  times
  [human_kgn_cap_total_reference](https://eduaguilera.github.io/whep/reference/human_kgn_cap_total_reference.md),
  kg N per inhabitant.

- `"urban"`: HYDE's urban population count
  (`read_hyde_population(variable = "urban")`) times
  [human_kgn_cap_reference](https://eduaguilera.github.io/whep/reference/human_kgn_cap_reference.md),
  kg N per urban inhabitant.

Both rates are the calibration nitrogen divided by the calibration
population on the same basis, so either regenerates its calibration
total. Elsewhere they differ by how far a population's urban share
departs from the calibration one: in 2010 the global WPP total
population is 2.0 times HYDE's global urban count.

`build_urban_n()` is the deprecated former name of this function. It
forwards every argument to `build_human_n()` and warns (class
`whep_build_urban_n_deprecated`, also `lifecycle_warning_deprecated`);
it will be removed in a future release. The output columns were renamed
with it: `urban_n_t` is now `human_n_t`, and `method_urban`,
`method_urban_population` and `method_urban_kgn_cap` are now
`method_human`, `method_human_population` and `method_human_kgn_cap`.

## Usage

``` r
build_human_n(
  years = NULL,
  population_basis = c("total", "urban"),
  polity_validity = c("keep", "flag", "drop"),
  data = list(),
  example = FALSE
)

build_urban_n(...)
```

## Arguments

- years:

  Optional integer vector of calendar years to keep. `NULL` keeps every
  year the supplied population covers; it is required when the
  population is read rather than supplied.

- population_basis:

  Which population, and the per-capita rate on the same basis, generates
  the load: `"total"` (default) or `"urban"`. See Description. Recorded
  in the `method_human_population` and `method_human_kgn_cap` output
  columns.

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

  Optional named list of pre-loaded inputs: `total_population` (`lon`,
  `lat`, `area_code`, `year`, `population`, one row per polycell as
  [`build_total_population_grid()`](https://eduaguilera.github.io/whep/reference/build_total_population_grid.md)
  returns it; read under `population_basis = "total"`, falling back to
  that builder when absent; taken per polycell, not re-split by
  `polity_frac`, and every polycell must exist in `cell_polity`),
  `urban_population` (`lon`, `lat`, `year`, `urban_pop`; read under
  `population_basis = "urban"`, falling back to
  `read_hyde_population(variable = "urban")` when absent), `cell_polity`
  (`lon`, `lat`, `area_code`, plus optional `polity_frac`; a missing
  `polity_frac` is treated as 1 for backwards compatibility) and
  `cropland_ha` (`lon`, `lat`, `area_code`, `year`, `cropland_ha`,
  required: the gridded cropland area used as the simple room proxy,
  `cropland_ha * 0.170` t N/ha, the same EU-Nitrates fixed ceiling used
  by
  [`allocate_manure_to_land()`](https://eduaguilera.github.io/whep/reference/allocate_manure_to_land.md)'s
  `fixed_ceiling_kg_ha` default). Supplying only the other basis's
  population aborts with class `whep_human_n_population_basis_mismatch`
  rather than silently reading a default. Both frames' `area_code` must
  be the numeric WHEP area code, whole-numbered, as
  [`build_cell_polity()`](https://eduaguilera.github.io/whep/reference/build_cell_polity.md)
  emits it. Anything else – an ISO3 literal, an area name, a fractional
  value – aborts with class `whep_human_n_area_code_unresolved` (also
  `whep_urban_area_code_unresolved`, its former name), naming the frame
  that carries it. It is not bridged: the two frames key the same
  transport partition, so one written in a different vocabulary from the
  other would silently strand a cell's load on a cell with no room
  instead of placing it, and an ISO3 resolves to a `polity_area_code`
  aggregation bucket that is not every territory's own code (`"SSD"`
  would become 206, Sudan (former)). Map to the code first, via
  [`add_area_code()`](https://eduaguilera.github.io/whep/reference/add_area_code.md)
  or
  [regions_full](https://eduaguilera.github.io/whep/reference/regions_full.md).

- example:

  If `TRUE`, return a small fixture instead of reading data. Defaults to
  `FALSE`.

- ...:

  For `build_urban_n()`, arguments passed on to `build_human_n()`.
  `population_basis` defaults to `"urban"` here if omitted, matching
  this function's historical behaviour, unlike `build_human_n()`'s own
  `"total"` default.

## Value

A tibble with `lon`, `lat`, `area_code`, `year`, `human_n_t`,
`method_human`, `method_human_population` (`"total_population"` or
`"urban_population"`) and `method_human_kgn_cap`
(`"kg_n_per_total_inhabitant"` or `"kg_n_per_urban_inhabitant"`), plus
the polity columns below, plus `reporting_polity_out_of_span` when
`polity_validity = "flag"`.

## Details

The current per-capita rate is a documented placeholder (one national
historical series applied as a global default). For a future refinement,
human N should instead be derived from two distinct, more mechanistic
streams: (1) sewage/human-excreta N estimated from actual historical
per-capita dietary protein/N intake (already reconstructable in WHEP via
its FAOSTAT/commodity-balance food-supply data, rather than a fixed
external per-capita constant), and (2) food-waste/municipal-solid-waste
N from actual historical food-loss and waste estimates. This is out of
scope for the current task and is not implemented here.

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
build_human_n(example = TRUE)
#> # A tibble: 1 × 12
#>    year area_code polity_area_code reporting_polity_code reporting_polity_name
#>   <int>     <int>            <int> <chr>                 <chr>                
#> 1  2020       203              203 ESP-1800-2025         Spain                
#> # ℹ 7 more variables: reporting_polity_has_geometry <lgl>, lon <dbl>,
#> #   lat <dbl>, human_n_t <dbl>, method_human <chr>,
#> #   method_human_population <chr>, method_human_kgn_cap <chr>
```
