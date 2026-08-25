# Read national population on WHEP area codes.

Reads the `gdp-population` pin and returns population per country and
year on the numeric `area_code` the rest of the package uses. The pin is
keyed by ISO3 (in a column confusingly also called `area_code`) and
reports population in thousands; both are converted here, so consumers
such as
[`build_food_supply()`](https://eduaguilera.github.io/whep/reference/build_food_supply.md)
and
[`build_n_percapita()`](https://eduaguilera.github.io/whep/reference/build_n_percapita.md)
get the `year`/`area_code`/`population` contract they document.

Regional residual aggregates in the pin (`RAFR`, `RASI`, `REUR`, `RLAM`,
`ROCE`) are not countries and carry no numeric code. They are dropped
and reported rather than silently discarded, since their omission is
what makes the result a countries-only total rather than a world total.

`area_code` here is `polity_area_code`, which is a **bucket, not an
identity**: several ISO3 codes can share one code, and this function
sums them, so some rows are aggregates of more than one territory. WHEP
models the reporting members of bucket 999 in their own right, so with
the real pin and the default options the only such row is code 206
"Sudan (former)", carrying Sudan plus South Sudan from 2012 on; it
resolves to `SUD-1956-2011`, the pre-secession territory its two members
together cover. Under `options(whep.unfold_rest_of_world = "none")` code
999 folds Syria, North Macedonia, Palestine, Eswatini, Equatorial Guinea
and French Guiana as well. Every folded row is named in a message.

The pin does not cover every area WHEP models, and that is a bigger gap
than the fold. On the real pin 190 of the 256 area codes the crosswalk
resolves get a population row; the 66 that do not include Bhutan,
Comoros, New Caledonia and the Faroe Islands, all of which the commodity
balances do give food to.
[`build_food_supply()`](https://eduaguilera.github.io/whep/reference/build_food_supply.md)
and
[`build_n_percapita()`](https://eduaguilera.github.io/whep/reference/build_n_percapita.md)
inner-join this table, so those areas are absent from their per-capita
output rather than wrong in it. Both warn and name them instead of
dropping them silently; `options(whep.warn_missing_population = FALSE)`
silences that warning.

`population_source = "pin_wpp_fallback"` fills the country-years the pin
does not reach from
[`read_wpp_population()`](https://eduaguilera.github.io/whep/reference/read_wpp_population.md),
and **only** those: the pin wins wherever both have a value, so turning
it on cannot move a denominator that was already published. On the real
inputs it adds 44 areas the pin has no row for at all — Réunion, Bhutan,
Comoros, Western Sahara, New Caledonia, the French overseas departments
and the small island states — and 4,755 country-years inside the pin's
own year span. Filled rows are stamped `source_pop = "UN WPP 2024"`.

It is a gap-filler and not a replacement, because the two sources
disagree where they overlap: across 12,309 shared country-years they
differ by a median 0.64%, a 95th percentile of 4.4% and a maximum of
81%. That is why `"pin"` remains the default.

Neither source can reach an area whose territory no longer exists,
because both are keyed on a present-day ISO3 code.
[`population_source_reach()`](https://eduaguilera.github.io/whep/reference/population_source_reach.md)
reports which areas that leaves out and whether the polities database's
`successor` relation could stand in for them. Against UN WPP 2024's
vocabulary, exactly one reporting area outside the Rest-of-World bucket
is unreachable by either route: area 151 Netherlands Antilles,
`ANT-1961-2010`, which carries commodity-balance food in every year from
1961 to 2010 and for which upstream publishes no successor at all
(#787). Reachable is not the same as safe to sum — see that function and
the note at the top of `R/population_reach.R`.

## Usage

``` r
read_population(
  years = NULL,
  data = list(),
  population_source = c("pin", "pin_wpp_fallback"),
  example = FALSE
)
```

## Arguments

- years:

  Optional integer vector of calendar years to keep. `NULL` (default)
  keeps every year the pin covers.

- data:

  Optional named list of pre-loaded inputs to avoid the pin read:
  `gdp_population` (the raw pin, with `Year`, `area_code` as ISO3, `pop`
  in thousands) and `wpp_population` (a
  [`read_wpp_population()`](https://eduaguilera.github.io/whep/reference/read_wpp_population.md)
  output). Falls back to
  [`whep_read_file()`](https://eduaguilera.github.io/whep/reference/whep_read_file.md)
  when absent.

- population_source:

  `"pin"` (default, the `gdp-population` pin alone) or
  `"pin_wpp_fallback"`, which additionally fills country-years the pin
  does not cover from UN WPP.

- example:

  If `TRUE`, return a small fixture instead of reading remote data.
  Defaults to `FALSE`.

## Value

A tibble with `year`, `area_code`, `population` (persons) and
`source_pop`, one row per area code and year, sorted by year then area
code, plus the polity columns below. `source_pop` carries the pin's own
vocabulary (`"Original"`, `"Linear interpolation"`,
`"First value carried backwards"`), joined with `" + "` when a bucket
sums ISO3 codes of differing provenance, or `"UN WPP 2024"` for a
fallback-filled row. A row is one country in the common case, but
`area_code` is an aggregation bucket: rows from 2012 on 206 ("Sudan
(former)") are sums over several territories rather than a single
country, as are rows on 999 ("Rest of World") when the Rest-of-World
fold is restored.

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
read_population(example = TRUE)
#> # A tibble: 5 × 7
#>    year area_code polity_area_code reporting_polity_code reporting_polity_name  
#>   <int>     <int>            <int> <chr>                 <chr>                  
#> 1  2010        41               41 CHN-1950-2025         China (PRC)            
#> 2  2010       100              100 IND-1949-2025         India                  
#> 3  2010       231              231 USA-1959-2025         United States of Ameri…
#> 4  2010       101              101 IDN-2002-2025         Indonesia              
#> 5  2010        21               21 BRA-1909-2025         Brazil                 
#> # ℹ 2 more variables: reporting_polity_has_geometry <lgl>, population <dbl>
```
