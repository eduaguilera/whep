# Read natural-grass productivity from an LPJmL run.

Sums the natural-grass PFT net primary production bands (the ungrazed
natural stand, climate-driven) into a per-cell productivity layer used
to distribute grazing livestock by grass production rather than pasture
area. Natural-grass NPP is exogenous to the grazing density, so it
avoids the livestock to grassland_lsuha to grass-NPP circularity.

## Usage

``` r
read_lpjml_grass_productivity(
  run_dir = Sys.getenv("WHEP_LPJML_RUN_DIR"),
  years = NULL,
  first_year = 1901L,
  example = FALSE
)
```

## Arguments

- run_dir:

  Path to the LPJmL run output directory holding `pft_npp.nc`. Defaults
  to the `WHEP_LPJML_RUN_DIR` environment variable, consistent with
  [`build_feed_intake_local()`](https://eduaguilera.github.io/whep/reference/build_feed_intake_local.md).

- years:

  Integer vector of calendar years to read.

- first_year:

  First calendar year of the run's output time axis.

- example:

  If `TRUE`, return a small fixture instead of reading a run.

## Value

A tibble with `lon`, `lat`, `year` and `grass_npp` (gC/m2/yr).

## Examples

``` r
read_lpjml_grass_productivity(example = TRUE)
#> # A tibble: 3 × 4
#>      lon    lat  year grass_npp
#>    <dbl>  <dbl> <int>     <dbl>
#> 1   9.25  47.8   2000      413.
#> 2  35.8   -1.25  2000      631.
#> 3 -55.2  -12.2   2000      349.
```
