# Build the crop/soil N2O extension.

Estimate IPCC 2019 Tier 1 nitrous-oxide emissions from nitrogen applied
to managed soils, as a footprint extension keyed by
`(year, area_code, item_cbs_code)` in kilograms of carbon-dioxide
equivalent (CO2e). This is the soil-N2O analogue of
[`build_livestock_ghg_extension()`](https://eduaguilera.github.io/whep/reference/build_livestock_ghg_extension.md)
and feeds
[`build_footprint()`](https://eduaguilera.github.io/whep/reference/build_footprint.md)
/
[`compute_footprint()`](https://eduaguilera.github.io/whep/reference/compute_footprint.md)
the same way.

Three nitrogen inputs to soil are included:

- **Synthetic fertiliser** (F_SN): FAOSTAT reports it only as a country
  total (tonnes N per `area_code` per year), so it is allocated to crops
  in proportion to each crop's harvested area within the country-year
  (from
  [`get_primary_production()`](https://eduaguilera.github.io/whep/reference/get_primary_production.md)).

- **Applied manure** (F_ON): FAOSTAT "Manure applied to soils (N
  content)" country total, allocated to crops by harvested area as for
  F_SN.

- **Crop residues** (F_CR): the dry matter of above-ground residues
  returned to soil (from
  [`get_primary_residues()`](https://eduaguilera.github.io/whep/reference/get_primary_residues.md),
  net of the removed fraction) times the crop's residue nitrogen content
  (IPCC 2019 Table 11.1a).

N2O is then estimated with IPCC 2019 Refinement (Vol 4, Ch 11) Tier 1
factors (climate-aggregated): direct `EF1 = 0.010`; indirect via
volatilisation `EF4 = 0.010` applied to the volatilised fraction
(`FracGASF = 0.11` for synthetic, `FracGASM = 0.21` for manure; crop
residues do not volatilise, Eq 11.9); indirect via leaching
`FracLEACH = 0.24` times `EF5 = 0.011`. N2O-N is converted to N2O by
44/28 and to CO2e with the chosen GWP100.

Manure deposited by grazing animals (F_PRP, which uses the grazing EF3
on pasture) and below-ground residue N are further Tier 1 inputs not yet
included.

## Usage

``` r
build_crop_soil_n2o_extension(
  gwp = c("ar6", "ar5", "ar4"),
  residue_removed_frac = 0.45,
  data = list(),
  example = FALSE
)
```

## Arguments

- gwp:

  100-year global warming potential standard for N2O, `"ar6"` (default,
  273), `"ar5"` (265) or `"ar4"` (298).

- residue_removed_frac:

  Fraction of above-ground crop residue removed from the field (for
  feed, fuel or construction) and therefore not returned to soil.
  Defaults to `0.45`, a global mid-range value; country-specific removal
  (`gleam_fracremove`) is a future refinement.

- data:

  Optional named list of pre-loaded inputs to avoid remote reads:
  `primary_prod`
  ([`get_primary_production()`](https://eduaguilera.github.io/whep/reference/get_primary_production.md),
  for harvested area), `fertilizer` (the `faostat-fertilizer-nutrients`
  pin), `manure` (the `faostat-emissions-livestock` pin) and
  `primary_residues`
  ([`get_primary_residues()`](https://eduaguilera.github.io/whep/reference/get_primary_residues.md)).
  Each falls back to its reader when absent.

- example:

  If `TRUE`, return a small fixture instead of reading remote data.
  Defaults to `FALSE`.

## Value

A tibble with columns `year`, `area_code`, `item_cbs_code`, `impact_u`
(soil N2O in kilograms CO2e) and `method_soil_n2o`.

## Examples

``` r
build_crop_soil_n2o_extension(example = TRUE)
#> # A tibble: 2 × 5
#>    year area_code item_cbs_code  impact_u method_soil_n2o    
#>   <int>     <int>         <int>     <dbl> <chr>              
#> 1  2010        10          2511 412612200 IPCC_2019_Tier1_AR6
#> 2  2010        10          2513 176833800 IPCC_2019_Tier1_AR6
```
