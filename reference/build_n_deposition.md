# Build gridded atmospheric nitrogen deposition inputs.

Combines HaNi NHx and NOy deposition into a total nitrogen deposition
rate per WHEP grid cell, using the true latitude-dependent 0.5-degree
cell area to convert the deposited mass into a per-hectare rate, and a
polity share of the cell to derive the absolute mass a polity receives.

The cell's deposited mass is split across the polities holding the cell
in proportion to `polity_area_ha`, the geodesic territory each holds in
it, as
[`build_polycell_support()`](https://eduaguilera.github.io/whep/reference/build_polycell_support.md)
measures it. The transitional alternative is `polity_frac`, the
subcell-count share
[`build_cell_polity()`](https://eduaguilera.github.io/whep/reference/build_cell_polity.md)
carries, which is quantised to 1/36 of a cell; it stays selectable so
the two partitions can be compared, and it is what a support table
carrying no `polity_area_ha` is split by. Either way the split is a
share of the cell, so the source mass is redistributed and never created
or destroyed.

Each polity's share is then decomposed over the territory it lands on:
land, inland water and ice, the three separately addressable categories
[`build_polycell_support()`](https://eduaguilera.github.io/whep/reference/build_polycell_support.md)
carries. Deposition to freshwater is a real flux on the eutrophication
pathway rather than a rounding error, so the three are reported side by
side and the consumer chooses. A support with no category columns cannot
be decomposed and carries the single `"territory"` category, which says
the row is undecomposed rather than claiming it is land.

## Usage

``` r
build_n_deposition(
  years = NULL,
  data = list(),
  split = c("auto", "polity_area_ha", "polity_frac"),
  categories = c("auto", "land_water_ice", "none"),
  polity_validity = c("keep", "flag", "drop"),
  example = FALSE
)
```

## Arguments

- years:

  Optional integer vector of calendar years to keep. `NULL` keeps every
  year the inputs cover.

- data:

  Optional named list of pre-loaded inputs: `nhx` and `noy` (each `lon`,
  `lat`, `year`, `value_g`, falling back to
  [`read_n_deposition()`](https://eduaguilera.github.io/whep/reference/read_n_deposition.md)
  when absent) and `cell_polity` (`lon`, `lat`, `area_code`,
  `cell_area_ha` and the `split` key column, required).

- split:

  Which polity share splits the cell's deposited mass: `"auto"`
  (default) takes `polity_area_ha` when the support carries it and
  `polity_frac` otherwise, `"polity_area_ha"` and `"polity_frac"` demand
  that key and abort when it is absent. The resolved key is recorded in
  the `method_polity_split` output column, so a table's split is
  readable from the table.

- categories:

  How each polity's share is decomposed over the territory it lands on:
  `"auto"` (default) decomposes when the support carries `land_area_ha`,
  `inland_water_ha` and `ice_area_ha` and emits the single `"territory"`
  category otherwise, `"land_water_ice"` demands those columns and
  aborts when they are absent, `"none"` keeps one undecomposed row per
  polycell. The resolved choice is recorded in `method_area_split`.

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

- example:

  If `TRUE`, return a small fixture instead of reading data. Defaults to
  `FALSE`.

## Value

A tibble with `lon`, `lat`, `area_code`, `year`, `area_category`,
`deposition_kgn_ha`, `deposition_n_t`, `method_deposition`,
`method_polity_split` and `method_area_split`, plus the polity columns
below, plus `reporting_polity_out_of_span` when
`polity_validity = "flag"`.

`area_category` is `"land"`, `"inland_water"` or `"ice"` under
`"land_water_ice"`, and `"territory"` under `"none"`. Summing
`deposition_n_t` over the categories of a polycell recovers that
polycell's whole share, so an unfiltered sum over the table is still the
source mass; a consumer wanting one category **must filter**.

`deposition_kgn_ha` is the whole-cell mean rate: the cell's total mass
over its whole area, so every polity of a cell carries the same rate on
every category row and the rate is **not** conserved on re-aggregation.
Only `deposition_n_t` is a mass.

Rows are keyed on `area_code`.
[`build_polycell_support()`](https://eduaguilera.github.io/whep/reference/build_polycell_support.md)
keys on `polity_code` and does not derive the reporting vocabulary
(DA-23), and `polity_area_crosswalk` folds distinct polities into one
`area_code`, so a support table must be converted to one row per cell
and `area_code` **before** it is passed here. That conversion is refused
rather than performed silently.

## Which land definition governs what

Two different land definitions meet in this function and they govern
different things, which is the single fact most likely to be misread
from the output.

**WHEP's territory governs placement.** Where a cell's deposited mass
goes – which polity receives it, and how much of it lands on land, on
inland water or on ice – is decided entirely by
[`build_polycell_support()`](https://eduaguilera.github.io/whep/reference/build_polycell_support.md)'s
`polity_area_ha` and its three categories.

**HaNi's own land mask governs the total.** The mass being placed is the
HaNi block sum, and HaNi is referenced to the whole 5 arcmin cell inside
a land-masked domain: its mask is a **third** land definition, measuring
13.5977 Gha against 14.3195 Gha of whole cells and 12.9931 Gha of LUH2
terrestrial. Nothing here re-references the mass to WHEP's land, and
that is deliberate. Forming a rate on the whole cell and multiplying it
by `land_area_ha` would shed about 9% of the source mass, because the
rate's denominator includes ocean while land plus inland water plus ice
is territory; re-referencing to HaNi's own mask instead is the cleanest
per-hectare rate but moves the global total by about 4.5%.

So a global sum out of this function is HaNi's total, redistributed onto
WHEP's territory – not WHEP's land multiplied by a WHEP rate.
Conservation is exact against the source (34.77 Tg NHx in 2014), and the
reconciliation that is *not* available is
`rate * (land + inland_water + ice) == source mass`, which does not hold
as an identity and is not asserted anywhere.

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
build_n_deposition(example = TRUE)
#> # A tibble: 3 × 14
#>    year area_code polity_area_code reporting_polity_code reporting_polity_name
#>   <int>     <int>            <int> <chr>                 <chr>                
#> 1  2020         1                1 ARM-1991-2025         Armenia              
#> 2  2020         1                1 ARM-1991-2025         Armenia              
#> 3  2020         1                1 ARM-1991-2025         Armenia              
#> # ℹ 9 more variables: reporting_polity_has_geometry <lgl>, lon <dbl>,
#> #   lat <dbl>, area_category <chr>, deposition_kgn_ha <dbl>,
#> #   deposition_n_t <dbl>, method_deposition <chr>, method_polity_split <chr>,
#> #   method_area_split <chr>
```
