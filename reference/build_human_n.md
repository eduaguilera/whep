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
  example = FALSE,
  method_residual = c("nearest", "polity", "keep", "drop"),
  method_local_residual = c("room_cap", "uncapped")
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

- method_residual:

  What happens to human N that the transport step cannot deliver: the
  residual on a source cell with **no cropland**, plus, under the
  default `method_local_residual = "room_cap"`, the part of a cropland
  source cell's residual that exceeds its own room. On the 2010 global
  grid, under the default `"total"` basis, the first is 13,198 cells and
  47,365 t N, 0.700% of the 6.77 Mt of human N, and the second adds
  24,346 t on 1,424 cells; see `method_local_residual` for what
  `"nearest"` leaves stranded.

  - `"nearest"` (default): run the transport step's own rule again with
    a growing radius. Each such cell offers its nitrogen to the
    same-polity cropland cells in its nearest ring (Chebyshev distance
    in 0.5-degree steps, wrapped at the antimeridian) that still has
    room, in proportion to that room; a cell's room is its 170 kg N/ha
    ceiling minus the human N already on it, and an over-subscribed cell
    is filled only to its room. The radius grows one ring at a time
    until the nitrogen is placed or the polity has no room left.
    Conserves mass and keeps the nitrogen as close to the people who
    produced it as the room allows. No distance cap or transport
    coefficient is applied.

  - `"polity"`: pool it per polity-year and spread it over all of that
    polity-year's cropland in proportion to cropland room (area), the
    rule
    [`build_n_inputs()`](https://eduaguilera.github.io/whep/reference/build_n_inputs.md)
    applies under `method_unsupported = "reallocate"`. Conserves mass,
    but places the nitrogen anywhere in the polity.

  - `"keep"`: leave it on its source cell, as before this argument
    existed, flagged in `human_n_stranded_t`. On a cell with no
    cropland,
    [`build_n_inputs()`](https://eduaguilera.github.io/whep/reference/build_n_inputs.md)'s
    `method_unsupported` then decides its fate (by default, it aborts);
    an over-room excess is applied on its own cell's cropland, as
    `"uncapped"` would.

  - `"drop"`: discard it. Loses the mass, biased towards dense,
    cropland-free cells.

  `"nearest"` and `"polity"` never cross a polity, like the transport
  step itself, so a polity-year with population and no cropland anywhere
  keeps its nitrogen on the source cell under either, flagged in
  `human_n_stranded_t`; `"nearest"` does the same with whatever its
  polity has no room left for. Whenever any cell is undelivered, the
  count, the tonnes and the share of human N are reported (a warning,
  class `whep_human_n_undelivered`, when any nitrogen is dropped or left
  stranded; otherwise a message of the same class), and the per-year
  figures are attached as `attr(x, "human_n_undelivered")`. Recorded in
  the `method_human_residual` output column.

- method_local_residual:

  What happens to the residual the transport step hands back to a source
  cell that **has** cropland. The transport step offers a cell's load to
  its ring-1 neighbours only, so a dense cell with little cropland gets
  back most of its own load, on that little cropland.

  - `"room_cap"` (default): the cell keeps only what fits in its own
    room, 170 kg N/ha times its cropland minus the human N the transport
    step already landed on it – the room the transport step and
    `"nearest"` respect everywhere else. The excess is undelivered N,
    placed by `method_residual` like the residual of a cell with no
    cropland. On the 2010 global grid (`"total"` basis) that is 24,346 t
    on 1,424 of the 2,180 cropland source cells left with a residual.
    Under `"nearest"`, 17,083 t of it is moved and 7,263 t stays
    stranded on its own cell, in the three polities with no room left
    anywhere (Hong Kong 6,701 t, Kuwait 363 t, the Bahamas 200 t),
    alongside the 2,145 t on cells with no cropland (Qatar, Iceland,
    Samoa).

  - `"uncapped"`: the cell keeps its whole residual, as before this
    argument existed, whatever its cropland area. On the same grid 1,417
    cropland cells then end above 170 kg N/ha, holding 24,346 t above
    it, and the largest load is booked on 3.6e-7 ha.

  No minimum-cropland threshold is offered: the room cap already moves a
  sliver's whole residual, and a threshold would be a new, unsourced
  number that misses the excess on larger cells (15,202 t of the 24,346
  t sits on cells with at least 1 ha). Recorded in the
  `method_human_local_residual` output column; the part of
  `undelivered_t` it adds is the summary's `over_room_t`.

- ...:

  For `build_urban_n()`, arguments passed on to `build_human_n()`.
  `population_basis` defaults to `"urban"` here if omitted, matching
  this function's historical behaviour, unlike `build_human_n()`'s own
  `"total"` default.

## Value

A tibble with `lon`, `lat`, `area_code`, `year`, `human_n_t`,
`human_n_relocated_t` (the part of `human_n_t` placed on the cell by
`method_residual`), `human_n_stranded_t` (the undelivered part no rule
could place: on a cell with no cropland, which no downstream cropland
allocation can place, or above the room of the cell's own cropland,
which
[`build_n_inputs()`](https://eduaguilera.github.io/whep/reference/build_n_inputs.md)
then applies there, over the ceiling), `method_human`,
`method_human_population` (`"total_population"` or
`"urban_population"`), `method_human_kgn_cap`
(`"kg_n_per_total_inhabitant"` or `"kg_n_per_urban_inhabitant"`) and
`method_human_residual` and `method_human_local_residual`, plus the
polity columns below, plus `reporting_polity_out_of_span` when
`polity_validity = "flag"`. The attribute `"human_n_undelivered"` is a
tibble with one row per year: `year`, `human_n_t`, `n_cells`
(undelivered source cells), `undelivered_t`, `over_room_t` (the part of
`undelivered_t` that exceeded the room of its own cell's cropland),
`relocated_t`, `stranded_t`, `dropped_t`, `undelivered_share` (of
`human_n_t`), `method_human_residual` and `method_human_local_residual`.

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
#> # A tibble: 1 × 16
#>    year area_code polity_area_code reporting_polity_code reporting_polity_name
#>   <int>     <int>            <int> <chr>                 <chr>                
#> 1  2020       203              203 ESP-1800-2025         Spain                
#> # ℹ 11 more variables: reporting_polity_has_geometry <lgl>, lon <dbl>,
#> #   lat <dbl>, human_n_t <dbl>, human_n_relocated_t <dbl>,
#> #   human_n_stranded_t <dbl>, method_human <chr>,
#> #   method_human_population <chr>, method_human_kgn_cap <chr>,
#> #   method_human_residual <chr>, method_human_local_residual <chr>
```
