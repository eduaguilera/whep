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

## Usage

``` r
read_population(years = NULL, data = list(), example = FALSE)
```

## Arguments

- years:

  Optional integer vector of calendar years to keep. `NULL` (default)
  keeps every year the pin covers.

- data:

  Optional named list of pre-loaded inputs to avoid the pin read:
  `gdp_population` (the raw pin, with `Year`, `area_code` as ISO3, `pop`
  in thousands). Falls back to
  [`whep_read_file()`](https://eduaguilera.github.io/whep/reference/whep_read_file.md)
  when absent.

- example:

  If `TRUE`, return a small fixture instead of reading remote data.
  Defaults to `FALSE`.

## Value

A tibble with `year`, `area_code` and `population` (persons), one row
per country-year, sorted by year then area code, plus the polity columns
below.

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
