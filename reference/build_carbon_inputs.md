# Assemble the per-land-use-class soil carbon inputs.

Build the carbon-input layer
[`build_carbon_balance()`](https://eduaguilera.github.io/whep/reference/build_carbon_balance.md)
consumes, keyed by `(lon, lat, area_code, year, land_use)`. The cropland
class aggregates the per-crop cropland inputs from
[`build_soil_carbon_inputs()`](https://eduaguilera.github.io/whep/reference/build_soil_carbon_inputs.md)
to the class grain: the class carbon density is the
harvested-area-weighted mean of the per-crop densities, and the
humification fraction is the carbon-mass-weighted mean of the per-crop
fractions (mass = density times area). The grassland and natural classes
come from
[`build_grass_natural_carbon_inputs()`](https://eduaguilera.github.io/whep/reference/build_grass_natural_carbon_inputs.md)
unchanged.

## Usage

``` r
build_carbon_inputs(
  resolution = c("grid", "polity"),
  data = list(),
  years = NULL,
  example = FALSE
)
```

## Source

Cropland inputs from
[`build_soil_carbon_inputs()`](https://eduaguilera.github.io/whep/reference/build_soil_carbon_inputs.md);
grassland and natural inputs from
[`build_grass_natural_carbon_inputs()`](https://eduaguilera.github.io/whep/reference/build_grass_natural_carbon_inputs.md);
assembled per the WHEP historical carbon-balance design.

## Arguments

- resolution:

  `"grid"` (default, per cell and class) or `"polity"` (aggregated to
  `area_code`, area-weighting the cropland density by the polity crop
  area).

- data:

  Named list of pre-loaded inputs, each falling back to its builder when
  absent: `cropland` (the
  [`build_soil_carbon_inputs()`](https://eduaguilera.github.io/whep/reference/build_soil_carbon_inputs.md)
  output, per cell, crop and year, with `total_c_input_mgc_ha_yr` and
  `humified_fraction`); `crop_area` (per cell, crop and year harvested
  area with columns `lon`, `lat`, `area_code`, `item_prod_code`, `year`,
  `crop_area_ha`, used to area-weight the crop densities);
  `grass_natural` (the
  [`build_grass_natural_carbon_inputs()`](https://eduaguilera.github.io/whep/reference/build_grass_natural_carbon_inputs.md)
  output at the class grain); and optional `land_use` (per-cell class
  `area_ha`, used to area-weight grassland/natural polity output). When
  `cropland` or `grass_natural` are absent the respective builder is
  called with the remaining members of `data`.

- years:

  Optional integer vector of calendar years to keep. `NULL` (default)
  keeps every year the inputs cover. Threaded into the default
  [`build_soil_carbon_inputs()`](https://eduaguilera.github.io/whep/reference/build_soil_carbon_inputs.md)
  and
  [`build_grass_natural_carbon_inputs()`](https://eduaguilera.github.io/whep/reference/build_grass_natural_carbon_inputs.md)
  builders so their readers slice to the requested years; ignored for
  inputs supplied via `data`.

- example:

  If `TRUE`, return a small fixture instead of reading remote data.
  Defaults to `FALSE`.

## Value

A tibble keyed by `(lon, lat, area_code, year, land_use)` at `"grid"`
resolution (or `(area_code, year, land_use)` at `"polity"`), with
`c_input_mgc_ha_yr`, `humified_fraction` and `method_c_input`, for
`land_use` in `"cropland"`, `"grassland"` and `"natural"`.

## Examples

``` r
build_carbon_inputs(example = TRUE)
#> # A tibble: 3 × 8
#>     lon   lat area_code  year land_use  c_input_mgc_ha_yr humified_fraction
#>   <dbl> <dbl>     <int> <int> <chr>                 <dbl>             <dbl>
#> 1  0.25  0.25         1  2000 cropland               2.75             0.182
#> 2  0.25  0.25         1  2000 grassland              4                0.115
#> 3  0.25  0.25         1  2000 natural                6                0.325
#> # ℹ 1 more variable: method_c_input <chr>
```
