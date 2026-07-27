# Read an LPJmL hydrology variable into a tidy tibble.

Reads one monthly LPJmL hydrology output (drainage, evapotranspiration
components, precipitation, irrigation, runoff, discharge or soil water
content) from a finished run's NetCDF files and returns it in tidy long
form. The logical `var` name is mapped to the on-disk file and in-file
variable name, so callers need not know the LPJmL naming quirks. The
synthetic `"aet"` variable sums the three actual-evapotranspiration
components (transpiration, evaporation, interception).

## Usage

``` r
read_lpjml_hydrology(
  var = c("drainage", "transp", "evap", "interc", "aet", "prec", "rain", "irrig",
    "runoff", "discharge", "swc", "cft_nir"),
  run_dir = NULL,
  years = NULL,
  first_year = 1901L,
  monthly = TRUE,
  agg = c("sum", "mean"),
  data = NULL,
  example = FALSE
)
```

## Arguments

- var:

  Logical variable name, one of `"drainage"`, `"transp"`, `"evap"`,
  `"interc"`, `"aet"`, `"prec"`, `"rain"`, `"irrig"`, `"runoff"`,
  `"discharge"`, `"swc"` or `"cft_nir"` (per-CFT net irrigation
  requirement).

- run_dir:

  Path to the LPJmL run output directory. Defaults to
  `Sys.getenv("WHEP_LPJML_RUN_DIR")`.

- years:

  Optional integer vector of calendar years to keep. `NULL` keeps every
  year present in the file.

- first_year:

  First calendar year of the run's monthly time axis.

- monthly:

  If `TRUE`, return one row per cell-month; if `FALSE`, aggregate the 12
  months of each year per cell (flux variables summed, soil water
  content averaged).

- agg:

  Annual aggregation for `monthly = FALSE`, `"sum"` (flux default) or
  `"mean"` (soil-water default).

- data:

  Optional pre-read tibble (`lon`, `lat`, `year`, `month`, `value`, plus
  `layer` for `"swc"` or `band` for `"cft_nir"`) used in place of
  reading NetCDF, for testing.

- example:

  If `TRUE`, return a small fixture instead of reading remote data.
  Defaults to `FALSE`.

## Value

A tibble with columns `lon`, `lat`, `year`, `value` (plus `month` when
`monthly = TRUE`, `layer` for `"swc"`, and `band` for `"cft_nir"`).

## Examples

``` r
read_lpjml_hydrology(example = TRUE)
#> # A tibble: 2 × 5
#>     lon   lat  year month value
#>   <dbl> <dbl> <int> <int> <dbl>
#> 1 -180.  0.25  1901     1   1.2
#> 2 -180.  0.25  1901     2   0.8
```
