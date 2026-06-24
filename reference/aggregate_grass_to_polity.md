# Aggregate gridded grass availability to polity totals.

Sums gridded grass availability to polity (country, or subnational where
available) totals, splitting each cell's grass by the cell's land-area
share in each polity so border cells are attributed proportionally. The
polity grass supply ceiling for feed allocation.

## Usage

``` r
aggregate_grass_to_polity(grass, cell_polity)
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

## Value

A tibble with `area_code`, `year` and `grass_avail_dm_t`.

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
