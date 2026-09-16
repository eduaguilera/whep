# Read monthly cropland soil cover from an LPJmL run's crop calendar.

Per-cell, per-month share of a cell's cropped area that is under a
growing crop, derived from LPJmL's sowing (`sdate.nc`) and harvest
(`hdate.nc`) dates and area-weighted by `cftfrac.nc`. Supply the result
as `data$cropland_cover` to
[`build_carbon_balance()`](https://eduaguilera.github.io/whep/reference/build_carbon_balance.md)
to place the cropland soil-cover season where the crops actually are.

## Usage

``` r
read_lpjml_crop_cover(
  run_dir = NULL,
  years = NULL,
  first_year = NULL,
  by = c("cropland", "regime"),
  example = FALSE
)
```

## Source

Schaphoff, S. et al. (2018). LPJmL4 - a dynamic global vegetation model
with managed land - Part 1: Model description. *Geoscientific Model
Development*, 11, 1343-1375.
[doi:10.5194/gmd-11-1343-2018](https://doi.org/10.5194/gmd-11-1343-2018)
.

## Arguments

- run_dir:

  Path to the LPJmL run output directory. `NULL` (default) uses
  `WHEP_LPJML_RUN_DIR`.

- years:

  Optional integer vector of calendar years to keep. `NULL` (default)
  keeps every year the files cover.

- first_year:

  Calendar year of the files' first time step. `NULL` (default) reads it
  from the file's own `time` axis.

- by:

  `"cropland"` (default) pools every calendar band into one cover per
  cell and month. `"regime"` keeps the rainfed and the irrigated bands
  apart – a band is irrigated when its name starts with `irrigated` –
  and adds `regime` and `cropped_frac`, the regime's share of the cell,
  so a consumer can pool the two back by area. Rainfed and irrigated
  stands of one crop sow and harvest on different dates; the per-regime
  layer is what the herbaceous rainfed and irrigated crop groups of
  [`build_carbon_balance()`](https://eduaguilera.github.io/whep/reference/build_carbon_balance.md)
  (`crop_groups = list(method = "spain_hist")`) read, each from its own
  bands, while plain cropland reads the pool.

- example:

  If `TRUE`, return a small fixture instead of reading a run. Defaults
  to `FALSE`.

## Value

A tibble with `lon`, `lat`, `year`, `month` and `cropland_cover` (a
fraction in 0-1); with `by = "regime"`, also `regime` (`"rainfed"` or
`"irrigated"`) and `cropped_frac`, and up to two rows per cell-month.

## Why the temperature proxy is not good enough

Without a calendar,
[soc_soil_cover_curve](https://eduaguilera.github.io/whep/reference/soc_soil_cover_curve.md)
is anchored to each cell-year's warmest month as a stand-in for peak
canopy. Measured against this run at 2010 over 18,548 cropland cells,
the real area-weighted crop mid-season falls in the warmest month in
only **5.2%** of them, within one month in 22.6%, and **three or more
months away in 51.0%** (median absolute offset three months). Winter
cereals, Mediterranean systems and irrigated dry-season crops all grow
away from the temperature peak, so the proxy puts modelled full canopy
over real fallow and modelled bare soil over the real crop. It is a
timing error rather than a level one: the curve's annual mean cover is
0.254 against the calendar's 0.343.

## What the calendar does not cover

`sdate`/`hdate` carry 24 bands - the twelve named crops, rainfed and
irrigated - while `cftfrac` carries 32. The `others` and
managed-grassland bands have no calendar, which is **1.2% of cropped
area**; cells are returned with the cover of the area that does have
one, and a cell whose cropped area is entirely `others` yields no row
rather than a guess.

Band mapping is by NAME, never by index: only 12 of the 24 line up,
because `sdate` band 13 is `"irrigated temperate cereals"` where
`cftfrac` band 13 is `"rainfed others"`.

## Examples

``` r
read_lpjml_crop_cover(example = TRUE)
#> # A tibble: 12 × 5
#>      lon   lat  year month cropland_cover
#>    <dbl> <dbl> <int> <int>          <dbl>
#>  1  0.25  45.2  2010     1              0
#>  2  0.25  45.2  2010     2              0
#>  3  0.25  45.2  2010     3              0
#>  4  0.25  45.2  2010     4              0
#>  5  0.25  45.2  2010     5              1
#>  6  0.25  45.2  2010     6              1
#>  7  0.25  45.2  2010     7              1
#>  8  0.25  45.2  2010     8              1
#>  9  0.25  45.2  2010     9              1
#> 10  0.25  45.2  2010    10              0
#> 11  0.25  45.2  2010    11              0
#> 12  0.25  45.2  2010    12              0
```
