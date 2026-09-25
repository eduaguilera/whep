# Build gridded total population from UN WPP and HYDE.

Downscales UN World Population Prospects 2024 country totals
([`read_wpp_population()`](https://eduaguilera.github.io/whep/reference/read_wpp_population.md),
`by = "total"`) to WHEP's 0.5-degree polycells by HYDE's
total-population (`popc`) pattern. Each polycell's share of its
country's HYDE population is linearly interpolated between the two
nearest HYDE snapshots (decadal before 2001, annual 2001-2017 in the
HYDE 3.x baseline release) and held at the last snapshot for later
years, and WPP sets the level. So every country's cells sum to exactly
its WPP total in every year.

This is the population
[`build_human_n()`](https://eduaguilera.github.io/whep/reference/build_human_n.md)
reads under its default `population_basis = "total"`, where it is paired
with the per-capita rate
[human_kgn_cap_total_reference](https://eduaguilera.github.io/whep/reference/human_kgn_cap_total_reference.md),
whose denominator is its calibration WPP total population, so the level
and the rate share one basis.

## Usage

``` r
build_total_population_grid(
  years = NULL,
  polity_validity = c("keep", "flag", "drop"),
  hyde_dir = NULL,
  data = list(),
  example = FALSE
)
```

## Arguments

- years:

  Integer vector of calendar years. Required. A year before the first
  HYDE snapshot, or one the WPP table does not carry (WPP 2024 starts in
  1950), aborts with class `whep_total_population_uncovered`.

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

- hyde_dir:

  Directory holding the HYDE `"{year}AD_pop.zip"` archives, whose file
  names set the available snapshot years. Defaults to
  `Sys.getenv("WHEP_HYDE_DIR")`. Ignored when `data$hyde` is supplied.

- data:

  Named list of inputs: `cell_polity` (required; `lon`, `lat`,
  `area_code`, optional `polity_frac`, as
  [`build_cell_polity()`](https://eduaguilera.github.io/whep/reference/build_cell_polity.md)
  emits it, with the numeric WHEP area code), `wpp` (optional; `year`,
  `area_code`, `population`, bypassing
  [`read_wpp_population()`](https://eduaguilera.github.io/whep/reference/read_wpp_population.md))
  and `hyde` (optional; `lon`, `lat`, `year`, `popc` on the 0.5-degree
  grid, bypassing the HYDE archives; its years are the snapshot years).

- example:

  If `TRUE`, return a small fixture instead of reading data. Defaults to
  `FALSE`.

## Value

A tibble with `lon`, `lat`, `area_code`, `year` and `population`
(persons), one row per populated polycell-year, plus the polity columns
below, and the `coverage` attribute described in Details.

## Details

WPP is keyed on the `polity_area_code` bucket, so shares are taken
within each bucket (the one
`build_cell_polity(area_key = "polity_area")` assigns) and the output
keeps the grid `area_code` of `data$cell_polity`. A bucket with a WPP
total but no HYDE population, and one with HYDE population but no WPP
total, are each reported in a warning and in the `coverage` attribute
rather than dropped silently.

The `coverage` attribute is a list recording what the numbers are:
`population_basis` (`"total"`), `level_source`, `pattern_variable`
(`"popc"`), the `exact`, `interpolated` and `held` years with `held_at`,
the snapshots used, the per-year `plan`, and the `unplaced`,
`unmatched_hyde` and `share_gaps` tables.

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
build_total_population_grid(example = TRUE)
#> # A tibble: 1 × 9
#>    year area_code polity_area_code reporting_polity_code reporting_polity_name
#>   <int>     <int>            <int> <chr>                 <chr>                
#> 1  2010       203              203 ESP-1800-2025         Spain                
#> # ℹ 4 more variables: reporting_polity_has_geometry <lgl>, lon <dbl>,
#> #   lat <dbl>, population <dbl>
```
