# Read gridded LPJmL-forcing windspeed onto WHEP's grid.

Reads the GSWP3-W5E5 monthly windspeed forcing used to drive LPJmL
(single consolidated NetCDF, already on WHEP's native 0.5-degree grid)
and returns it in tidy long form. The file's `wind` variable carries no
`units` attribute, because the monthly aggregation dropped it; the unit
is metres per second, taken from the ISIMIP2a GSWP3-W5E5 daily source
the file is derived from, which declares `units = "m s-1"` and
`standard_name = "wind_speed"`. That is why the output column is named
`windspeed_ms`.

The `windspeed_ms` column is named to match the MANNER driver column of
the same name, so the output feeds
[`calculate_nh3()`](https://eduaguilera.github.io/whep/reference/calculate_nh3.md)
(`method = "manner"` or `"manner_default"`, whose organic path requires
`windspeed_ms`) after a join onto the gridded N-input records; this
reader is the package's only source of that driver.

## Usage

``` r
read_lpjml_wind(years = NULL, wind_dir = NULL, example = FALSE)
```

## Arguments

- years:

  Optional integer vector of calendar years to keep. `NULL` reads every
  year present in the file (1901-2019 in the currently pinned base; the
  span is whatever the resolved file holds, not a fixed range).

- wind_dir:

  Path to the directory holding a `wind_gswp3-w5e5_<span>_monthly.nc`.
  The span in that filename is resolved against the directory rather
  than assumed, so an extended base reads without a code change.
  Defaults to `Sys.getenv("WHEP_WIND_DIR")`.

- example:

  If `TRUE`, return a small fixture instead of reading data. Defaults to
  `FALSE`.

## Value

A tibble with `lon`, `lat`, `year`, `month`, `windspeed_ms`.

## See also

[`calculate_nh3()`](https://eduaguilera.github.io/whep/reference/calculate_nh3.md),
which consumes `windspeed_ms`.

## Examples

``` r
read_lpjml_wind(example = TRUE)
#> # A tibble: 1 × 5
#>     lon   lat  year month windspeed_ms
#>   <dbl> <dbl> <int> <int>        <dbl>
#> 1 -180.  89.8  1901     1          3.2
```
