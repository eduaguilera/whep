# Read FAOSTAT Food Balance Sheet population on WHEP area codes.

Reads item 2501 "Population", element 511 "Total Population - Both
sexes", from the `faostat-fbs-old` and `faostat-fbs-new` pins — the same
two pins
[`build_commodity_balances()`](https://eduaguilera.github.io/whep/reference/build_commodity_balances.md)
reads for the food itself — and returns population per area and year on
the numeric `area_code` the rest of the package uses. FAOSTAT publishes
it in thousands; it is converted to persons here.

This is the one population source WHEP has that is **not** keyed on a
present-day ISO3 code. It is keyed on the FAOSTAT area code, the same
key space the commodity balances are built from, and FAOSTAT keeps a
dissolved reporting area alive for the years it reported. So it covers
the territories
[`read_population()`](https://eduaguilera.github.io/whep/reference/read_population.md)
and
[`read_wpp_population()`](https://eduaguilera.github.io/whep/reference/read_wpp_population.md)
structurally cannot: area 186 Serbia and Montenegro for exactly
1992–2005 and area 151 Netherlands Antilles for exactly 1961–2010, the
two largest holes
[`population_source_reach()`](https://eduaguilera.github.io/whep/reference/population_source_reach.md)
reports (#862, \#787).

The two pins overlap over 2010–2013 and disagree there, sometimes
sharply: FAOSTAT area 272 Serbia in 2010 is 9,647,000 in
`faostat-fbs-old` against 7,395,860 in `faostat-fbs-new`, because the
old vintage's Serbia includes Kosovo and the new one does not.
`faostat-fbs-new` wins an overlapping `(year, area_code)`, which is the
order
[`build_commodity_balances()`](https://eduaguilera.github.io/whep/reference/build_commodity_balances.md)
gives the same two pins for the numerator.

FAOSTAT's regional and grouping aggregates (`World`, `Africa`,
`European Union`, `Least Developed Countries` and 38 others) carry area
codes at or above 5000, resolve to no polity, and are dropped, so they
cannot leak into a per-country denominator.

`area_code` is `polity_area_code`, a **bucket, not an identity**,
resolved year by year exactly as the commodity balances resolve it. On
the real pins no bucket-year receives more than one FAOSTAT area, so no
row here sums two territories — but note South Sudan (277) has no
population row in either pin, so bucket 206 from 2012 is Sudan alone,
where the `gdp-population` pin sums `SDN + SSD` onto it.

## Usage

``` r
read_fbs_population(years = NULL, data = list(), example = FALSE)
```

## Arguments

- years:

  Optional integer vector of calendar years to keep. `NULL` (default)
  keeps every year the pins cover.

- data:

  Optional named list of pre-loaded raw pins to avoid the pin read,
  `fbs_old` and/or `fbs_new`, each in the pins' own long FAOSTAT layout
  (`Area Code`, `Item Code`, `Element Code`, `Year`, `Value`). Falls
  back to
  [`whep_read_file()`](https://eduaguilera.github.io/whep/reference/whep_read_file.md)
  for whichever is absent.

- example:

  If `TRUE`, return a small fixture instead of reading remote data.
  Defaults to `FALSE`.

## Value

A tibble with `year`, `area_code`, `population` (persons) and
`source_pop` (`"FAOSTAT FBS old"` or `"FAOSTAT FBS new"`, naming which
pin the row came from), one row per area code and year, sorted by year
then area code, plus the polity columns below.

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
read_fbs_population(example = TRUE)
#> # A tibble: 4 × 8
#>    year area_code polity_area_code reporting_polity_code reporting_polity_name
#>   <int>     <int>            <int> <chr>                 <chr>                
#> 1  1992       186              186 SCG-1992-2006         Serbia and Montenegro
#> 2  2000       186              186 SCG-1992-2006         Serbia and Montenegro
#> 3  2005       186              186 SCG-1992-2006         Serbia and Montenegro
#> 4  2010       203              203 ESP-1800-2025         Spain                
#> # ℹ 3 more variables: reporting_polity_has_geometry <lgl>, population <dbl>,
#> #   source_pop <chr>
```
