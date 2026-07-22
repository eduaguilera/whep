# Flag implausible year-on-year jumps in a time series.

Scan each series for consecutive observations whose ratio falls outside
a plausible band, the level-2 (within-series) detector of the AFE data
and code validation framework. It catches unjustified single-year steps,
spikes and the level shifts that appear where two sources are spliced,
while a break-year allowlist keeps documented regime changes from firing
as false positives.

## Usage

``` r
check_series_jumps(
  data,
  value_col,
  time_col = year,
  .by = NULL,
  ratio_bounds = c(0.55, 1.6),
  bands = NULL,
  min_value = 0,
  consecutive_only = TRUE,
  allowlist = NULL,
  verbose = TRUE
)
```

## Arguments

- data:

  A data frame with one observation per row.

- value_col:

  The column holding the series values to scan.

- time_col:

  The column holding time values. Default: `year`.

- .by:

  A character vector of grouping columns identifying each series
  (optional). When `NULL`, the whole table is one series.

- ratio_bounds:

  Length-2 numeric `c(low, high)`: the default plausible band for the
  ratio of consecutive values. A ratio below `low` or above `high` is
  flagged.

- bands:

  Optional data frame of per-group band overrides: the grouping columns
  (a subset of `.by`) plus `lo` and `hi`. Where a group matches, its
  `lo`/`hi` replace `ratio_bounds` for that group.

- min_value:

  Minimum value both members of a pair must exceed to be flagged. Keeps
  genuine near-zero technology onsets from firing. Default: `0`.

- consecutive_only:

  Logical. If `TRUE` (default), only pairs one time step apart are
  scanned; larger gaps are skipped.

- allowlist:

  Optional data frame of documented break years, matched on the grouping
  columns plus `time_col`. Matching flags are returned with
  `allowlisted = TRUE` rather than dropped.

- verbose:

  Logical. If `TRUE` (default), report flag counts with `cli`.

## Value

A tibble with one row per flagged jump: the grouping columns, the
`time_col` of the later observation, `prev_value`, `value`, `ratio`
(`value / prev_value`) and `allowlisted`. When nothing is flagged, a
zero-row tibble of the same shape and column types.

## Details

This is the first landed detector of the reusable `check_*` library
described in the AFE data-validation framework decision
(`afse-wiki/wiki/decisions/afse-data-validation-framework.md`, level 2,
within-series). It generalises the energy-hist consecutive-year jump
scan (a world-total ratio scan against a break-year allowlist) into a
grouped, band-parameterised check usable both as an inline pipeline
guard and as a test backend.

Two pieces of the framework's metadata model are exposed as arguments.
`bands` supplies per-variable plausible-jump bands (land area is tight,
yield is wide, so a single global band is wrong for a mixed panel), and
`allowlist` supplies the historically justified break years that are
reported but not treated as defects. Undocumented jumps stay flagged.

A robust variant (flagging via the median absolute deviation of log
ratios, per the Hampel/MAD anchors in the framework) is a documented
future extension, not implemented here.

## Examples

``` r
series <- tibble::tibble(
  category = rep(c("area", "yield"), each = 5),
  year = rep(2000:2004, times = 2),
  value = c(100, 101, 102, 180, 181, 3.0, 3.1, 5.2, 3.0, 3.1)
)
# area steps 102 -> 180 (1.76x); yield steps 3.1 -> 5.2 (1.68x)
check_series_jumps(series, value, .by = "category")
#> ℹ check_series_jumps: 2 jumps flagged
#> (2 undocumented, 0 allowlisted).
#> # A tibble: 2 × 6
#>   category  year prev_value value ratio allowlisted
#>   <chr>    <int>      <dbl> <dbl> <dbl> <lgl>      
#> 1 area      2003      102   180    1.76 FALSE      
#> 2 yield     2002        3.1   5.2  1.68 FALSE      

# Widen the band for yield only, leaving area on the default:
bands <- tibble::tibble(category = "yield", lo = 0.4, hi = 2.5)
check_series_jumps(series, value, .by = "category", bands = bands)
#> ℹ check_series_jumps: 1 jump flagged
#> (1 undocumented, 0 allowlisted).
#> # A tibble: 1 × 6
#>   category  year prev_value value ratio allowlisted
#>   <chr>    <int>      <dbl> <dbl> <dbl> <lgl>      
#> 1 area      2003        102   180  1.76 FALSE      
```
