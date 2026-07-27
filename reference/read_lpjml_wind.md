# Read gridded LPJmL-forcing windspeed onto WHEP's grid.

Reads the GSWP3-W5E5 monthly windspeed forcing used to drive LPJmL
(single consolidated NetCDF, already on WHEP's native 0.5-degree grid)
and returns it in tidy long form. The file's `wind` variable carries no
`units` attribute; per the GSWP3-W5E5 forcing convention its physical
unit is assumed to be metres per second (m/s), and that assumption is
encoded only in the output column name (`windspeed_ms`), not inferred
from file metadata.

## Usage

``` r
read_lpjml_wind(years = NULL, wind_dir = NULL, example = FALSE)
```

## Arguments

- years:

  Optional integer vector of calendar years to keep. `NULL` reads every
  year present in the file (1901-2016).

- wind_dir:

  Path to the directory holding `wind_gswp3-w5e5_1901_2016_monthly.nc`.
  Defaults to `Sys.getenv("WHEP_WIND_DIR")`.

- example:

  If `TRUE`, return a small fixture instead of reading data. Defaults to
  `FALSE`.

## Value

A tibble with `lon`, `lat`, `year`, `month`, `windspeed_ms`.

## Examples

``` r
read_lpjml_wind(example = TRUE)
#> # A tibble: 1 × 5
#>     lon   lat  year month windspeed_ms
#>   <dbl> <dbl> <int> <int>        <dbl>
#> 1 -180.  89.8  1901     1          3.2
```
