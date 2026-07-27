# Assemble soil carbon inputs per cell, crop and year.

Builds the carbon returned to soil from crop residues, crop roots, weeds
and applied manure, as the carbon-input layer the soil-organic-carbon
turnover models
([`calculate_soc_dynamics()`](https://eduaguilera.github.io/whep/reference/calculate_soc_dynamics.md))
consume. Soil-returned residue carbon (residue net of the fraction
removed for feed, fuel and burning), root carbon and weed
(spontaneous-grass) carbon come from
[`calculate_npp_carbon_nitrogen()`](https://eduaguilera.github.io/whep/reference/calculate_npp_carbon_nitrogen.md)
(per polity, crop and year); manure carbon comes from
[`build_livestock_nutrient_flows()`](https://eduaguilera.github.io/whep/reference/build_livestock_nutrient_flows.md)'s
`applied` stream. The four component carbon masses are converted to
megagrams of carbon (1 tonne = 1 Mg), gridded to cells in proportion to
each crop's harvested area, and divided by the cell-crop area to give Mg
C per hectare per year. A carbon-weighted humification fraction is
computed per cell-year from
[residue_humification](https://eduaguilera.github.io/whep/reference/residue_humification.md),
with the weed carbon humified at the weed (spontaneous-grass)
coefficient.

At `"polity"` resolution the component carbon masses are summed back to
`(area_code, item_prod_code, year)` and the per-hectare values and
humified fraction re-derived from the polity totals.

## Usage

``` r
build_soil_carbon_inputs(
  resolution = c("grid", "polity"),
  data = list(),
  years = NULL,
  example = FALSE
)
```

## Arguments

- resolution:

  `"grid"` (default, per cell) or `"polity"` (aggregated to
  `area_code`).

- data:

  Optional named list of pre-loaded inputs, each falling back to its
  reader when absent: `npp` (soil-returned residue, root and weed carbon
  per `area_code`, `item_prod_code`, `year`, columns `residue_soil_c_t`,
  `root_c_t` and `weed_npp_c_t`, tonnes C); `manure` (the `applied`
  tibble of
  [`build_livestock_nutrient_flows()`](https://eduaguilera.github.io/whep/reference/build_livestock_nutrient_flows.md),
  with `crop` either an existing `item_prod_code` or an `item_prod` name
  from
  [items_prod_full](https://eduaguilera.github.io/whep/reference/items_prod_full.md)
  (matched case-insensitively), and `territory` a stringified
  `area_code` or `iso3c`); `country_grid` and `crop_patterns` (the
  spatialization inputs, `crop_patterns` carrying per-cell
  `crop_area_ha`); `harvested_area` (the FAOSTAT national harvested area
  per `area_code`, `item_prod_code`, `year` in a `faostat_area_ha`
  column, used to renormalize each polity-crop-year's spatialized cell
  area to the national total so per-hectare densities are the national
  density and carbon mass is conserved; defaults to the same
  [`get_primary_production()`](https://eduaguilera.github.io/whep/reference/get_primary_production.md)
  table the NPP reader uses, and is skipped when a hand-supplied `npp`
  keeps the pipeline offline unless supplied here);
  `residue_humification` (defaults to
  [residue_humification](https://eduaguilera.github.io/whep/reference/residue_humification.md)).

- years:

  Optional integer vector of calendar years to keep. `NULL` (default)
  keeps every year the inputs cover. Threaded into the default NPP and
  manure readers so they slice to the requested years; ignored for
  inputs supplied via `data`.

- example:

  If `TRUE`, return a small fixture instead of reading remote data.
  Defaults to `FALSE`.

## Value

A tibble keyed by `(lon, lat, area_code, item_prod_code, year)` at
`"grid"` resolution (or `(area_code, item_prod_code, year)` at
`"polity"`), with `residue_c_mgc_ha_yr`, `root_c_mgc_ha_yr`,
`weed_c_mgc_ha_yr`, `manure_c_mgc_ha_yr`, `total_c_input_mgc_ha_yr`,
`humified_fraction` and `method_c_input`.

## Examples

``` r
build_soil_carbon_inputs(example = TRUE)
#> # A tibble: 4 × 12
#>     lon   lat area_code item_prod_code  year residue_c_mgc_ha_yr
#>   <dbl> <dbl>     <int> <chr>          <int>               <dbl>
#> 1  0.25  0.25         1 15              2020                 1.5
#> 2  0.75  0.25         1 15              2020                 1.5
#> 3  0.25  0.25         1 27              2020                 1.5
#> 4  0.75  0.25         1 27              2020                 1.5
#> # ℹ 6 more variables: root_c_mgc_ha_yr <dbl>, weed_c_mgc_ha_yr <dbl>,
#> #   manure_c_mgc_ha_yr <dbl>, total_c_input_mgc_ha_yr <dbl>,
#> #   humified_fraction <dbl>, method_c_input <chr>
```
