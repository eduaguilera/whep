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
per country-year, sorted by year then area code.

## Examples

``` r
read_population(example = TRUE)
#> # A tibble: 5 × 3
#>    year area_code population
#>   <int>     <int>      <dbl>
#> 1  2010        41 1348191400
#> 2  2010       100 1240613600
#> 3  2010       231  311182800
#> 4  2010       101  244016200
#> 5  2010        21  196353500
```
