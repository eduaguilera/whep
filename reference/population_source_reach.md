# Report which areas a present-day-ISO3 population source can reach.

Every population source WHEP reads is keyed on a present-day ISO3 code:
the `gdp-population` pin and UN WPP 2024 both are. A WHEP `area_code`
names a territory in the years it reported, and for a dissolved
territory no present-day ISO3 stands for it, so the source has no row
and the area drops out of every per-capita output (see
[`read_population()`](https://eduaguilera.github.io/whep/reference/read_population.md)
and
[`build_food_supply()`](https://eduaguilera.github.io/whep/reference/build_food_supply.md)).

This reports, for each reporting period of each area code, whether the
vocabulary in `iso3_codes` reaches it:

- `"direct"` — the period's own ISO3 is in the vocabulary.

- `"successor"` — it is not, but the `successor` relation the polities
  database publishes leads, transitively, to ISO3 codes that are. Those
  codes are returned in `iso3_reached`.

- `"unreachable"` — neither. No arrangement of the source's rows
  supplies this area.

Measured against UN WPP 2024's 237 `Country/Area` ISO3 codes, 14
reporting periods are not `"direct"` and five are `"unreachable"` —
`ATF`, `PCN`, `SGS`, `NFK` and `CXR`, every one of them inside the
Rest-of-World bucket 999. `ANT-1961-2010`, area 151 Netherlands
Antilles, was the last `"unreachable"` outside that bucket (#787); the
2026-08-25 upstream re-sync gave it
`CUW-2010-2025; SXM-2010-2025; BES-2010-2025`, so it now reads
`"successor"` by lookup rather than by anything hardcoded here.

`"successor"` says the ISO3 codes exist, **not** that summing them is a
safe denominator. In general it is not: a successor sum for the Yugoslav
SFR falls 17.5% short of the `gdp-population` pin's own figure for the
same aggregate, of which Kosovo is 42% and a level disagreement between
the pin and WPP on that series is the rest. Use this to see what a
source can cover, and read the note at the top of `R/population_reach.R`
before turning any of it into a population.

`extent_exceeds_iso3` marks the periods where the codes reported are
**narrower than the territory**, because the walk stopped on a polity
that reuses one of its own parts' ISO3 — `SRB-2006-2008` "Serbia
(including Kosovo)" carrying `SRB`, `SUD-1956-2011` carrying `SDN`
(#863). It is orthogonal to `reach`: a `"direct"` period can be flagged,
and that is the case worth knowing about, since nothing else about the
row hints at it.

Rows on `area_code` 999 describe the **members** of the Rest-of-World
fold bucket rather than the bucket itself, since each member is a
crosswalk period of its own.

## Usage

``` r
population_source_reach(iso3_codes, crosswalk = NULL)
```

## Arguments

- iso3_codes:

  Character vector of the present-day ISO3 codes the population source
  publishes, e.g. `unique(read_wpp_population()$iso3c)`.

- crosswalk:

  Optional crosswalk overriding
  [polity_area_crosswalk](https://eduaguilera.github.io/whep/reference/polity_area_crosswalk.md),
  for testing.

## Value

A tibble with one row per `area_code` and `polity_code` reporting
period, sorted by area code then first reported year: `area_code`,
`polity_code`, `polity_name`, `map_year_start`, `map_year_end`,
`own_iso3` (the period's own ISO3), `reach` (`"direct"` / `"successor"`
/ `"unreachable"`), `n_iso3`, `iso3_reached` (the ISO3 codes standing in
for the area, `NA` when unreachable) and `extent_exceeds_iso3` (`TRUE`
where those codes cover less ground than the period's territory).

## Examples

``` r
population_source_reach(c("BEL", "LUX", "CZE", "SVK", "CUW"))
#> # A tibble: 297 × 10
#>    area_code polity_code  polity_name own_iso3 map_year_start map_year_end reach
#>        <int> <chr>        <chr>       <chr>             <int>        <int> <chr>
#>  1         1 ARM-1991-20… Armenia     ARM                1992         2024 unre…
#>  2         2 AFG-1919-20… Afghanistan AFG                1961         2024 unre…
#>  3         3 ALB-1913-20… Albania (1… ALB                1961         2024 unre…
#>  4         4 DZA-1919-19… Algeria (1… DZA                1961         1961 unre…
#>  5         4 DZA-1962-20… Algeria (1… DZA                1962         2024 unre…
#>  6         7 ANG-1905-19… Angola (19… AGO                1961         1974 unre…
#>  7         7 AGO-1975-20… Angola (in… AGO                1975         2024 unre…
#>  8         8 ATG-1800-20… Antigua an… ATG                1961         2024 unre…
#>  9         9 ARG-1902-20… Argentina   ARG                1961         2024 unre…
#> 10        10 AUS-1901-20… Australia   AUS                1961         2024 unre…
#> # ℹ 287 more rows
#> # ℹ 3 more variables: n_iso3 <int>, iso3_reached <chr>,
#> #   extent_exceeds_iso3 <lgl>
```
