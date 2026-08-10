# Aggregate gridded grass availability to polity totals.

Sums gridded grass availability to polity (country, or subnational where
available) totals, splitting each cell's grass by the cell's land-area
share in each polity so border cells are attributed proportionally. The
polity grass supply ceiling for feed allocation.

## Usage

``` r
aggregate_grass_to_polity(
  grass,
  cell_polity,
  polity_validity = c("keep", "flag", "drop")
)
```

## Arguments

- grass:

  Gridded grass availability from
  [`build_grass_availability()`](https://eduaguilera.github.io/whep/reference/build_grass_availability.md),
  with `lon`, `lat`, `year` and `grass_avail_dm_t`.

- cell_polity:

  Cell-to-polity mapping with `lon`, `lat`, `area_code` and
  `polity_frac` (the cell's land-area fraction in the polity; pass 1 for
  a majority assignment, e.g. from `country_grid`).

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

## Value

A tibble with `area_code`, `year` and `grass_avail_dm_t`, plus
`reporting_polity_out_of_span` when `polity_validity = "flag"`. This
output carries no reporting-polity columns, so the flag is attached
directly rather than derived from them.

## Examples

``` r
grass <- build_grass_availability(method = "lpjml", example = TRUE)
cp <- tibble::tibble(
  lon = grass$lon,
  lat = grass$lat,
  area_code = 1L,
  polity_frac = 1
)
aggregate_grass_to_polity(grass, cp)
#> # A tibble: 1 × 3
#>   area_code  year grass_avail_dm_t
#>       <int> <int>            <dbl>
#> 1         1  2000            45730
```
