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
  partial_year = c("abort", "warn", "drop"),
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
  year present in the file. A requested year the run does not have
  aborts, naming the coverage it does have: LPJmL runs ending in
  different years sit side by side in one folder, so the coverage is a
  property of `run_dir`, never an assumption of this reader.

- first_year:

  First calendar year of the run's monthly time axis. The last year is
  not an argument — it is read from the file's own time dimension.

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

- partial_year:

  What to do when `monthly = FALSE` and a cell-year does not carry all
  twelve months, so that aggregating it would silently return a total
  over eleven. `"abort"` (default) refuses, naming the absent
  cell-months; `"warn"` returns the short aggregate anyway; `"drop"`
  removes the incomplete cell-years and reports how many went, so the
  year is *absent* rather than wrong. Immaterial for the annual per-CFT
  variables, which carry no `month`. See *Partial years* below.

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

## Partial years

An LPJmL monthly output grows one time step at a time, so a run that was
interrupted, or is still writing, leaves a final year with fewer than
twelve steps. Aggregating that year sums **eleven** months and says
nothing: no value is `NA`, the row count is unchanged (one row per
cell-year either way), and every downstream total and identity goes on
balancing over the eleven. Reproduced on a real 24-month run truncated
to 23 with `ncks -d time,0,22`: the second year's summed deep seepage
fell 3.05% over 500 land cells (98,478 to 95,477 mm), with an identical
3,392-row output and no `NA` in any land cell (whep#1073).

The refusal lives here, at the reader, because this is where the absence
is created; once the short annual total is downstream it is
indistinguishable from a measurement. `years = ` was already safe – the
coverage check counts only whole years, so requesting a partial one
aborts – and `partial_year` closes the `years = NULL` whole-file read
that it does not cover.

## Examples

``` r
read_lpjml_hydrology(example = TRUE)
#> # A tibble: 2 × 5
#>     lon   lat  year month value
#>   <dbl> <dbl> <int> <int> <dbl>
#> 1 -180.  0.25  1901     1   1.2
#> 2 -180.  0.25  1901     2   0.8
```
