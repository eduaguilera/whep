# Read litterfall by land-use class from an LPJmL run.

Per-cell, per-year carbon returned to the soil as litter, split by the
stand that shed it. Litterfall is what physically enters the soil; net
primary production is not, because it also contains the increment that
stays in living biomass. On this run natural litterfall is 0.84 times
natural NPP at the median cell and 0.68 in aggregate, the shortfall
being biomass accumulation, fire, and the conversion pulse.

## Usage

``` r
read_lpjml_litterfall(
  class = c("nv", "agr", "mgrass", "luc", "total"),
  run_dir = NULL,
  years = NULL,
  first_year = NULL,
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

- class:

  Which stand's litterfall to read: `"nv"` (standing natural
  vegetation), `"agr"` (crop stands), `"mgrass"` (managed grassland),
  `"luc"` (the land-use conversion pulse) or `"total"`.

- run_dir:

  Path to the LPJmL run output directory. `NULL` (default) uses
  `WHEP_LPJML_RUN_DIR`.

- years:

  Optional integer vector of calendar years to keep. `NULL` (default)
  keeps every year the file covers.

- first_year:

  Calendar year of the file's first time step. `NULL` (default) reads it
  from the file's own `time` axis, which LPJmL stamps as
  `"days since YYYY-M-D"`. Pass a value only to override a file that
  carries no reference date.

- example:

  If `TRUE`, return a small fixture instead of reading a run. Defaults
  to `FALSE`.

## Value

A tibble with `lon`, `lat`, `year`, `class` and
`litterfall_c_mgc_ha_yr`.

## Whole-cell densities

Every class is a density per square metre of GRIDCELL, not of its own
stand: each accumulation site in LPJmL multiplies by `stand->frac`. To
get the density a stand actually experiences, divide by that stand's
area fraction – for `"nv"`, `natural_stand_frac` from
[`read_lpjml_natural_cover()`](https://eduaguilera.github.io/whep/reference/read_lpjml_natural_cover.md).
Mixing the two conventions understates a partly-natural cell's input by
exactly its natural fraction.

## The four classes sum to the total

`nv + luc + agr + mgrass` recovers `total` to float32 rounding, the
remainder being set-aside grass. `"luc"` is the pulse released when land
is converted, and it is large – 9.9% of all litterfall at 2010 against
1.3% for crop stands. It is booked to no destination: at
`landusechange.c` the receiving stand is still natural when the pulse is
credited, so apportioning it to the land that gained area is a decision
for the caller, not a quantity LPJmL reports.

## Examples

``` r
read_lpjml_litterfall(example = TRUE)
#> # A tibble: 2 × 5
#>     lon   lat  year class litterfall_c_mgc_ha_yr
#>   <dbl> <dbl> <int> <chr>                  <dbl>
#> 1 -0.25  5.25  2010 nv                      6.42
#> 2 12.2  18.8   2010 nv                      0.31
```
