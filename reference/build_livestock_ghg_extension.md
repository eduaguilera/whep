# Build the livestock greenhouse-gas emissions extension.

Aggregate per-animal IPCC livestock emissions into a footprint extension
keyed by `(year, area_code, item_cbs_code)`, expressed in kilograms of
carbon-dioxide equivalent (CO2e). This bridges the cohort-level
emissions pipeline
([`calculate_livestock_emissions()`](https://eduaguilera.github.io/whep/reference/calculate_livestock_emissions.md))
to the input-output grain used by
[`build_io_model()`](https://eduaguilera.github.io/whep/reference/build_io_model.md)
and
[`compute_footprint()`](https://eduaguilera.github.io/whep/reference/compute_footprint.md),
exactly like
[`build_grassland_land_extension()`](https://eduaguilera.github.io/whep/reference/build_grassland_land_extension.md)
does for land.

Live-animal head counts come from
[`get_primary_production()`](https://eduaguilera.github.io/whep/reference/get_primary_production.md),
are bridged to IPCC species with
[`prepare_livestock_emissions()`](https://eduaguilera.github.io/whep/reference/prepare_livestock_emissions.md),
and the resulting enteric and manure emissions are converted to CO2e and
summed back to the live-animal commodity sector (`item_cbs_code`, e.g.
961 for non-dairy cattle), which is itself a sector in
[`build_io_model()`](https://eduaguilera.github.io/whep/reference/build_io_model.md).

Two IPCC tiers are available, selected with `tier`:

- `1` (default): Tier 1 regional emission factors (IPCC 2019). It needs
  only species, country and head counts, so it is complete for every
  country in
  [`get_primary_production()`](https://eduaguilera.github.io/whep/reference/get_primary_production.md).
  It covers enteric and manure **methane** and manure **N2O** (direct
  and indirect, from default per-head nitrogen excretion rates).

- `2`: Tier 2 cohort energy balance (IPCC 2019). It derives enteric CH4
  and manure N2O from a per-animal energy and nitrogen balance, for
  finer resolution, but requires cohort weight and diet inputs. Animals
  whose emissions cannot be resolved (missing diet or energy data) are
  dropped with a warning rather than entering the footprint as `NA`. Its
  per-head enteric and manure emissions now sit in the same range as the
  Tier 1 regional factors. Tier 1 remains the default because it is
  complete for every country in
  [`get_primary_production()`](https://eduaguilera.github.io/whep/reference/get_primary_production.md),
  whereas Tier 2 needs cohort and diet inputs.

The CO2e conversion uses 100-year global warming potentials selected
with `gwp`:

- `"ar6"` (default): IPCC AR6 (2021) Table 7.15, biogenic CH4 = 27, N2O
  = 273.

- `"ar5"`: IPCC AR5 (2013), CH4 = 28, N2O = 265 (no climate-carbon
  feedback).

- `"ar4"`: IPCC AR4 (2007), CH4 = 25, N2O = 298.

## Usage

``` r
build_livestock_ghg_extension(
  tier = 1,
  gwp = c("ar6", "ar5", "ar4"),
  data = list(),
  example = FALSE
)
```

## Arguments

- tier:

  IPCC tier, `1` (default) or `2`.

- gwp:

  100-year global warming potential standard, `"ar6"` (default), `"ar5"`
  or `"ar4"`.

- data:

  Optional named list of pre-loaded inputs to avoid remote reads:
  `primary_prod` (the
  [`get_primary_production()`](https://eduaguilera.github.io/whep/reference/get_primary_production.md)
  output). It falls back to its reader when absent.

- example:

  If `TRUE`, return a small fixture instead of reading remote data.
  Defaults to `FALSE`.

## Value

A tibble with columns `year`, `area_code`, `item_cbs_code`, `impact_u`
(livestock emissions in kilograms CO2e) and `method_ghg` (the chosen
tier and GWP standard, e.g. `"IPCC_2019_Tier1_AR6"`).

## Examples

``` r
build_livestock_ghg_extension(example = TRUE)
#> # A tibble: 6 × 5
#>    year area_code item_cbs_code   impact_u method_ghg         
#>   <int>     <int>         <int>      <dbl> <chr>              
#> 1  1986        10           960  615600000 IPCC_2019_Tier1_AR6
#> 2  1986        10           961 3078000000 IPCC_2019_Tier1_AR6
#> 3  1986        10           976 1105650000 IPCC_2019_Tier1_AR6
#> 4  1986       100           961 2246400000 IPCC_2019_Tier1_AR6
#> 5  1987        10           961 3108780000 IPCC_2019_Tier1_AR6
#> 6  1987       100           960  842400000 IPCC_2019_Tier1_AR6
```
