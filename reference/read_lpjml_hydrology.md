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
    "runoff", "discharge", "swc", "cft_nir", "cft_consump_water_b",
    "cft_consump_water_g"),
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
  `"discharge"`, `"swc"`, `"cft_nir"` (per-CFT net irrigation
  requirement) or the per-CFT consumptive-water cubes
  `"cft_consump_water_b"` (blue) and `"cft_consump_water_g"` (green).
  The per-CFT variables keep their `band` dimension, and carry
  `band_name` when the file names its bands.

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
  content averaged). Immaterial for the annual per-CFT consumptive-water
  variables, which LPJmL writes one step per year: they carry no `month`
  column either way, and aggregating them groups rows that are already
  one per cell-year-band.

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

A tibble with columns `lon`, `lat`, `year`, `value` (plus `month` for
the monthly variables when `monthly = TRUE`, `layer` for `"swc"`, and
`band` plus `band_name` for the per-CFT variables). The annual per-CFT
consumptive-water variables never carry `month`.

## Examples

``` r
read_lpjml_hydrology(example = TRUE)
#> # A tibble: 2 × 5
#>     lon   lat  year month value
#>   <dbl> <dbl> <int> <int> <dbl>
#> 1 -180.  0.25  1901     1   1.2
#> 2 -180.  0.25  1901     2   0.8
```
