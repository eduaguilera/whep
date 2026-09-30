# Resolve one source per container from the admin-shares table

Choose, for every `(area_code, level, item_prod_code, year)` the
admin-shares table covers, the single source whose reported units set
the within-country shape, and record why it won. Operates on
**observed** rows only: gap filling is a separate step that runs after
resolution, so an `interpolated` or `carried` row here is an error, not
an input.

The result is a list of five tibbles rather than one table with
attributes, because every piece is itself evidence a later task reads,
and an attribute does not survive a write to parquet or CSV.

## Usage

``` r
resolve_admin_shares(
  shares,
  overrides = NULL,
  constraint_exclude = NULL,
  not_shipped = character()
)
```

## Arguments

- shares:

  Admin-shares table, conforming to
  [`admin_shares_schema()`](https://eduaguilera.github.io/whep/reference/admin_shares_schema.md)
  except that `source` joins the key: a multi-source union is exactly
  what resolution consumes, and the contract's key holds within one
  source. Every row must have `treatment_year == "observed"` and must
  carry `value`, `share` or both; a row with neither aborts with class
  `whep_error_admin_no_measure`, and a non-finite `value` or `share`
  with class `whep_error_admin_nonfinite`.

- overrides:

  Optional tibble of forced choices, with columns `area_code`, `source`
  and optionally `item_prod_code` (`NA`, or the column absent, means
  every item of that container). An item-specific row beats a
  container-wide one. A row that never forces a winner warns.

- constraint_exclude:

  Optional named list of years to withhold, keyed by `area_code` as a
  character name, such as `list("840" = 1961:1989)`. Those
  container-years leave the constraint set and are returned in
  `excluded`, never silently dropped. This is the leave-years-out
  validation switch.

- not_shipped:

  Character vector of `source` labels whose family is withheld from the
  pin board (plan T23/T24); the coverage report flags every resolved row
  whose source is one of them. Empty by default.

## Value

A list of five tibbles:

- `shares`: the winning rows, each exactly as supplied plus
  `resolved_source`, `resolved_tier`, `resolved_grain` and
  `resolution_rule` (one of `"override"`, `"grain"`, `"tier"`,
  `"run_length"`, `"source_name"`, `"single_candidate"`).

- `seams`: the seam list described above.

- `coverage`: the coverage report described above.

- `excluded`: the rows `constraint_exclude` withheld, in the input's
  shape.

- `dropped`: the losing rows, in the input's shape plus `drop_reason`
  (`"indicator_never_binds"`, `"indicator_precedence"`, `"override"`,
  `"grain"`, `"tier"`, `"run_length"`, `"source_name"`).

Every input row appears in exactly one of `shares`, `excluded` and
`dropped`.

## Precedence

Within one candidate group –
`(area_code, level, item_prod_code, indicator_used, year)` – each source
is one candidate. Candidates are ranked, best first, by:

1.  **Override**: a source named for this container in `overrides` wins
    wherever it is a candidate, and every group it reaches is recorded
    `resolution_rule = "override"` – including a group where the named
    source had no rival, so an override's footprint can be read off the
    coverage report. This is the per-country escape hatch the grain rule
    requires: an override may keep a coarser source over a finer one.

2.  **Grain**, finer first: `"admin1" < "admin2" < "admin3"`, compared
    through [`match()`](https://rdrr.io/r/base/match.html) on that
    vocabulary, exactly as
    [`admin_shares_schema()`](https://eduaguilera.github.io/whep/reference/admin_shares_schema.md)
    declares it. Grain beats tier, under decision 4 as amended at lock:
    public first at equal or finer grain, in-house where no public
    source of equal grain exists. So Spain keeps its 53 NUTS-3 provinces
    (tier 2, `"admin2"`) over Eurostat's NUTS-2 (tier 1, `"admin1"`). A
    candidate reporting at several grains at once is ranked on its
    **coarsest**, and warns.

3.  **Tier**, lower first.

4.  **Run length**, longer first: the contiguous run of that source
    around that year, defined below.

5.  **Source name**, ascending in the C locale, with a warning naming
    the tied sources. This is the deterministic last resort, never a
    scientific rule.

None of the five reads `value`, so a source consented to ship derived
shares only – the Latin American panel of the *Shares-only rows* section
of
[`admin_shares_schema()`](https://eduaguilera.github.io/whep/reference/admin_shares_schema.md)
– competes, wins and is carried on exactly the same terms as a source
shipping absolute areas. Its rows arrive in `shares` still carrying
`value = NA`; nothing here fills that in, and nothing downstream may.

Before any of that, the indicator in force is chosen per
`(area_code, level, item_prod_code, year)` by decision 8's order of
acceptance – `"area_harvested"`, `"area_planted_or_sown"`,
`"area_main"`, `"area_cultivated"`, then `"production"` only where no
area exists. Rows of a losing indicator go to `dropped` with
`drop_reason = "indicator_precedence"`. `"yield"` is in the table's
vocabulary but never binds an allocation (a share of a ratio is not a
share), so a `"yield"` row is dropped as `"indicator_never_binds"`.

## Contiguous run

The run length of source `s` at year `y` is the number of years in the
maximal block of **consecutive** years containing `y` in which `s`
supplies at least one row for the same
`(area_code, level, item_prod_code, indicator_used)` – the candidate
group's columns except `year`. So a source present in 1990:2000 has run
length 11 at every one of those years; if it is absent in 1997, its run
length is 7 at 1996 and 3 at 1998. The run is measured on the rows that
actually reach ranking, after `constraint_exclude` and after indicator
precedence: a long run of production years says nothing about the
continuity of an area series, and a year withheld for validation is not
evidence of coverage.

## Seams

One row per `(area_code, level, item_prod_code)` series, seam year and
`seam_kind`:

- `"start"`: the series' first resolved year. `value_to` is the source
  it starts on.

- `"source_switch"`, `"grain_switch"`, `"nuts_version_switch"`,
  `"indicator_switch"`: the resolved value differs from the previous
  **resolved** year of that series, which is the previous year present,
  not necessarily `year - 1`.

- `"coverage_change"`: the set of reporting units differs from the
  previous resolved year – a unit entering or leaving reporting.

A missing value and a present one count as a change; two missing values
do not.

## Coverage report

One row per `(area_code, level, item_prod_code, year)`, carrying the
resolved source, tier, grain, indicator, NUTS version and rule, the
number of reporting units, the units themselves as a `"|"`-joined string
of `level_polity_code` sorted in the C locale (a string, not a
list-column, so the table writes to parquet and CSV unchanged;
unresolved units appear as `"<NA>"` and count as one member), and
`coverage_change`, and `not_shipped`, which marks resolved rows whose
source family is withheld from the pin board (T23).

## Examples

``` r
# Spain 2010: Eurostat NUTS-2 (tier 1, admin1) against the in-house
# NUTS-3 provinces (tier 2, admin2). Grain beats tier, so tier 2 wins.
rows <- tibble::tibble(
  area_code = 724L,
  level_polity_code = c("ES-N2-A", "ES-N2-B", "ES-N3-1", "ES-N3-2"),
  level = 1L,
  item_prod_code = 15L,
  indicator_used = "area_harvested",
  year = 2010L,
  value = c(100, 200, 120, 180),
  share = c(1 / 3, 2 / 3, 0.4, 0.6),
  source = rep(c("Eurostat_apro_cpshr", "ES_provinces"), each = 2),
  tier = rep(c(1L, 2L), each = 2),
  grain = rep(c("admin1", "admin2"), each = 2),
  concept_break = FALSE,
  nuts_version = rep(c("2021", "2016"), each = 2),
  source_native_id = NA_character_,
  source_native_name = NA_character_,
  source_id = rep(c("Eurostat_apro_cpshr", "ES_provinces"), each = 2),
  source_version = NA_character_,
  recorded_at = "2026-01-01T00:00:00Z",
  treatment_year = "observed",
  value_flag = NA_character_
)
resolved <- resolve_admin_shares(rows)
resolved$coverage
#> # A tibble: 1 × 15
#>   area_code level item_prod_code species_group  year resolved_source
#>       <int> <int>          <int> <chr>         <int> <chr>          
#> 1       724     1             15 NA             2010 ES_provinces   
#> # ℹ 9 more variables: resolved_tier <int>, resolved_grain <chr>,
#> #   resolved_indicator <chr>, resolved_nuts_version <chr>,
#> #   resolution_rule <chr>, n_units_reporting <int>, reporting_units <chr>,
#> #   not_shipped <lgl>, coverage_change <lgl>
resolved$dropped
#> # A tibble: 2 × 23
#>   area_code level_polity_code level item_prod_code species_group indicator_used
#>       <int> <chr>             <int>          <int> <chr>         <chr>         
#> 1       724 ES-N2-A               1             15 NA            area_harvested
#> 2       724 ES-N2-B               1             15 NA            area_harvested
#> # ℹ 17 more variables: year <int>, value <dbl>, share <dbl>, source <chr>,
#> #   tier <int>, grain <chr>, concept_break <lgl>, nuts_version <chr>,
#> #   source_native_id <chr>, source_native_name <chr>, source_id <chr>,
#> #   source_version <chr>, recorded_at <chr>, treatment_year <chr>,
#> #   treatment_value <chr>, value_flag <chr>, drop_reason <chr>
```
