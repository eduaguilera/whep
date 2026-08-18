# Read UN World Population Prospects population.

Reads the UN WPP 2024 medium-variant population by five-year age group
and sex, resolved to WHEP area codes. `by = "total"` sums to one row per
country and year; `by = "age_sex"` keeps the age and sex detail that
[`build_protein_requirement()`](https://eduaguilera.github.io/whep/reference/build_protein_requirement.md)
needs.

The file is resolved in order: the `dir` argument, then the
`WHEP_WPP_DIR` environment variable, then a cache under
`rappdirs::user_cache_dir("whep")`, downloading it on first use.
Population is converted from the thousands WPP publishes to persons.

Only `Country/Area` locations with an ISO3 code are kept, so WPP's
regional and income-group aggregates cannot leak into a per-country
denominator.

## Usage

``` r
read_wpp_population(
  years = NULL,
  by = c("total", "age_sex"),
  data = NULL,
  dir = NULL
)
```

## Arguments

- years:

  Optional integer vector of years to keep. `NULL` (default) keeps every
  year in the file.

- by:

  Output grain: `"total"` (default, one row per `year` and `area_code`)
  or `"age_sex"` (adds `age_start`, `age_span` and `sex`).

- data:

  Optional pre-read WPP table, bypassing the file entirely. Used by the
  tests so the whole path stays offline.

- dir:

  Optional directory holding the WPP CSV,
  `WPP2024_PopulationByAge5GroupSex_Medium.csv.gz`.

## Value

A tibble with `year`, `area_code`, `population` (persons) and, for
`by = "age_sex"`, `age_start`, `age_span` and `sex` (`"m"` / `"f"`).
ISO3 codes the crosswalk does not resolve are dropped and named, rather
than returned on a missing `area_code`.

## Examples

``` r
read_wpp_population(
  by = "age_sex",
  data = tibble::tribble(
    ~ISO3_code, ~LocTypeName,   ~Time, ~AgeGrpStart, ~AgeGrpSpan,
    ~PopMale, ~PopFemale,
    "ESP",      "Country/Area", 2010L, 0L,           5L,
    1170.5,    1103.2
  )
)
#> # A tibble: 2 × 7
#>    year area_code iso3c age_start age_span sex   population
#>   <int>     <int> <chr>     <int>    <int> <chr>      <dbl>
#> 1  2010       203 ESP           0        5 m        1170500
#> 2  2010       203 ESP           0        5 f        1103200
```
