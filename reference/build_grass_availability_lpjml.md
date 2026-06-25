# Build grazable grass availability from an LPJmL run.

Reads managed-grassland net primary production/availability (the LPJmL
grassland CFT) from the pinned WHEP artifact by default. Pass
`availability` or `availability_path` to use a custom already-derived
artifact; pass `run_dir` to read a finished local LPJmL run instead and
convert NPP to grazable above-ground dry-matter availability, the forage
supply ceiling for feed allocation. Availability is the production flux,
not the realised grazing off-take (the off-take is the intake-validation
target, not the supply).

## Usage

``` r
build_grass_availability_lpjml(
  run_dir = NULL,
  years = NULL,
  first_year = 1901L,
  shares = grass_access_shares(),
  example = FALSE,
  availability = NULL,
  availability_path = NULL
)
```

## Arguments

- run_dir:

  Path to the LPJmL run output directory holding `pft_npp.nc` and
  `cftfrac.nc` (the `scenario_*` output folder). If unset, the pinned
  `lpjml-grass-availability` artifact is used.

- years:

  Integer vector of calendar years to read.

- first_year:

  First calendar year of the run's output time axis.

- shares:

  Accessibility and conversion parameters from
  [`grass_access_shares()`](https://eduaguilera.github.io/whep/reference/grass_access_shares.md).

- example:

  If `TRUE`, return a small fixture instead of reading a run.

- availability:

  Optional already-derived grass availability tibble/data frame. Takes
  precedence over pinned data and `run_dir`.

- availability_path:

  Optional path to an already-derived grass availability artifact
  (`.parquet`, `.csv`, or `.rds`). Takes precedence over pinned data and
  `run_dir`.

## Value

A tibble with `lon`, `lat`, `year`, `grass_npp_gc_m2`,
`grass_avail_dm_t_ha` and `grass_avail_dm_t`.

## Examples

``` r
build_grass_availability_lpjml(example = TRUE)
#> # A tibble: 3 × 6
#>      lon    lat  year grass_npp_gc_m2 grass_avail_dm_t_ha grass_avail_dm_t
#>    <dbl>  <dbl> <int>           <dbl>               <dbl>            <dbl>
#> 1   9.25  47.8   2000            612.                6.26            16980
#> 2 -55.2  -12.2   2000            488.                4.99            15640
#> 3  35.8   -1.25  2000            422.                4.31            13110
```
