# Build grassland and natural-land soil carbon inputs from LPJmL.

Assemble the carbon returned to soil under grassland and natural
vegetation as the layer the soil-organic-carbon turnover models consume,
from a finished LPJmL run. The class carbon input is the net primary
production minus harvested carbon (both per-plant-functional-type,
[`read_lpjml_npp()`](https://eduaguilera.github.io/whep/reference/read_lpjml_npp.md)),
floored at zero and converted to megagrams of carbon per hectare per
year (1 gC/m2 = 0.01 MgC/ha). Natural land sums the eleven natural
plant-functional-types (they coexist in one stand); grassland takes the
stand-area-weighted mean of the rainfed and irrigated grassland net
inputs and adds the grazing-excreta carbon from
[`build_livestock_nutrient_flows()`](https://eduaguilera.github.io/whep/reference/build_livestock_nutrient_flows.md)'s
`applied` stream spread uniformly over the polity's grassland area. The
humification fraction is the spontaneous-grass value for grassland and
the woody-residue value for natural land (both from
[residue_humification](https://eduaguilera.github.io/whep/reference/residue_humification.md)).

## Usage

``` r
build_grass_natural_carbon_inputs(
  resolution = c("grid", "polity"),
  data = list(),
  years = NULL,
  example = FALSE
)
```

## Source

LPJmL run net primary production and harvested carbon; grassland and
natural carbon inputs per the WHEP historical carbon-balance design.

## Arguments

- resolution:

  `"grid"` (default, per cell and class) or `"polity"` (aggregated to
  `area_code`, area-weighting the per-hectare densities).

- data:

  Named list of pre-loaded inputs, each falling back to its reader when
  absent: `npp` and `harvestc` (per cell, PFT and year, the
  [`read_lpjml_npp()`](https://eduaguilera.github.io/whep/reference/read_lpjml_npp.md)
  output); `stand_frac` (per cell, year and PFT name the
  managed-grassland stand fractions with columns `lon`, `lat`, `year`,
  `name_pft`, `stand_frac`); `country_grid` (`lon`, `lat`, `area_code`,
  `cell_area_frac`); `land_use` (per-cell class `area_ha`, used to
  spread excreta and to area-weight polity output); `excreta` (the
  `applied` tibble of
  [`build_livestock_nutrient_flows()`](https://eduaguilera.github.io/whep/reference/build_livestock_nutrient_flows.md),
  grassland rows carry `applied_c` tonnes C); `residue_humification`
  (defaults to
  [residue_humification](https://eduaguilera.github.io/whep/reference/residue_humification.md)).

- years:

  Optional integer vector of calendar years to keep. `NULL` (default)
  keeps every year the inputs cover. Threaded into the default LPJmL
  NPP, stand-fraction and land-use readers so they slice to the
  requested years; ignored for inputs supplied via `data`.

- example:

  If `TRUE`, return a small fixture instead of reading remote data.
  Defaults to `FALSE`.

## Value

A tibble keyed by `(lon, lat, area_code, year, land_use)` at `"grid"`
resolution (or `(area_code, year, land_use)` at `"polity"`), with
`c_input_mgc_ha_yr`, `humified_fraction` and `method_c_input`, for
`land_use` in `"grassland"` and `"natural"`.

## Examples

``` r
build_grass_natural_carbon_inputs(example = TRUE)
#> # A tibble: 4 × 8
#>     lon   lat area_code  year land_use  c_input_mgc_ha_yr humified_fraction
#>   <dbl> <dbl>     <int> <int> <chr>                 <dbl>             <dbl>
#> 1  26.2  35.2       300  2000 grassland              4.35             0.115
#> 2  26.2  35.2       300  2000 natural                4.56             0.325
#> 3 -64.2 -35.8        32  2000 grassland              1.95             0.115
#> 4 -64.2 -35.8        32  2000 natural                9.26             0.325
#> # ℹ 1 more variable: method_c_input <chr>
```
