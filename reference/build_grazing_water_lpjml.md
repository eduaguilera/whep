# Build native grassland evapotranspiration from an LPJmL run.

Preserve green and blue water consumption from the named rainfed and
irrigated grassland stands. The native stand-depth outputs
`cft_consump_water_g.nc` and `cft_consump_water_b.nc` are weighted by
`cftfrac.nc` and WHEP's spherical 0.5-degree cell areas. One millimetre
over one hectare is ten cubic metres. Depth units alone do not establish
the spatial basis: a supplied run configuration must declare
`grid_scaled = false`. Its SHA-256 fingerprint is retained in the
output.

This is grassland-domain ET before attribution to consumed feed. It is
not maximum grazing yield, animal drinking water or a livestock
footprint. Use
[`calculate_grazing_water()`](https://eduaguilera.github.io/whep/reference/calculate_grazing_water.md)
to select an attribution basis explicitly. Native blue ET remains blue;
rainfed stand names do not reclassify water. No country codes or
present-day territorial assignments are inferred.

All three cubes must share their recorded model version, commit and run
history. Their native annual axes must be consecutive on the noleap
calendar, measured in days since January 1. Both grassland bands and
every requested year must be supplied. Positional reads require native
variable dimension order `lon`, `lat`, `pft`, `time`, with degree
coordinates. Zero-area stands retain their native missing depth and have
zero volume; missing area fractions or missing depth on a positive-area
stand abort. Ocean/ice cells with no supplied fraction in either grass
band and no supplied ET are outside the returned domain. Injected cubes
define their own cell population; agreement between cubes does not
independently prove coverage of any target territory. Provenance labels
and a supplied config identify the inputs; they cannot prove that a
config was never edited.

## Usage

``` r
build_grazing_water_lpjml(
  run_dir = NULL,
  years = NULL,
  config = NULL,
  data = NULL,
  example = FALSE
)
```

## Arguments

- run_dir:

  LPJmL output directory. If `NULL`, use `WHEP_LPJML_RUN_DIR`. Not read
  when complete `data` is supplied.

- years:

  Integer calendar years. Required for run reads, which load only these
  years and the two named grass bands. When `data` is supplied, `NULL`
  takes the years from its stand-fraction cube.

- config:

  Path to the run's resolved JSON configuration. Required for run reads
  because the NetCDF headers do not record `grid_scaled`. Only explicit
  `false` is supported; cell-scaled water aborts.

- data:

  Complete named list of `green`, `blue` and `stand_frac` tibbles keyed
  by `lon`, `lat`, `year`, `band_name`, with numeric `value`. `vintage`
  has one row per input with `input`, `units`, `lpjml_version`,
  `lpjml_git_hash`, `lpjml_run`, `spatial_basis` and `source_basis`.
  Water uses `mm/yr` and spatial basis `"stand"`; fractions are
  dimensionless (`""` or `"1"`) with basis `"cell_fraction"`. Nonempty
  source-basis labels identify the evidence for those assertions. No
  missing list member is silently fetched from another input source.

- example:

  Return a small actual-readback fixture without reading a run. Defaults
  to `FALSE`.

## Value

A tibble per cell, year, named grass stand and water component, with
`stand_frac`, `cell_area_ha`, `grass_area_ha`, native `water_mm`,
extensive `grass_water_m3`, input-status and source/basis/run metadata.
Area and stand-fraction columns repeat across water components; count
those areas once. `water_scenario` identifies the supplied run, not a
hypothetical maximum-yield reference experiment.

## Examples

``` r
build_grazing_water_lpjml(example = TRUE)
#> # A tibble: 6 × 19
#>   component   lon   lat  year band_name         water_mm stand_frac cell_area_ha
#>   <chr>     <dbl> <dbl> <int> <chr>                <dbl>      <dbl>        <dbl>
#> 1 blue      -176. -43.8  2010 rainfed grassland 2.38e-13    0.00912      223288.
#> 2 green     -176. -43.8  2010 rainfed grassland 5.76e+ 2    0.00912      223288.
#> 3 blue      -172. -13.8  2010 rainfed grassland 3.48e-13    0.00731      300248.
#> 4 green     -172. -13.8  2010 rainfed grassland 1.26e+ 3    0.00731      300248.
#> 5 blue      -156.  20.8  2010 rainfed grassland 1.25e- 4    0.152        289057.
#> 6 green     -156.  20.8  2010 rainfed grassland 1.13e+ 3    0.152        289057.
#> # ℹ 11 more variables: grass_area_ha <dbl>, grass_water_m3 <dbl>,
#> #   water_input_status <chr>, source_water <chr>, lpjml_version <chr>,
#> #   lpjml_git_hash <chr>, lpjml_run <chr>, water_input_basis <chr>,
#> #   source_water_basis <chr>, water_scenario <chr>, source_fraction_basis <chr>
```
