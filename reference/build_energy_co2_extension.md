# Build the livestock energy-use CO2 footprint extension (meat only).

Aggregate GLEAM 3.0 on-farm (direct) and feed-production (embedded)
energy use into a footprint extension keyed by
`(year, area_code, item_cbs_code)`, expressed in kilograms of
carbon-dioxide equivalent (CO2e). This is the energy slice of the
livestock greenhouse-gas basket and is designed to be summed with
[`build_livestock_ghg_extension()`](https://eduaguilera.github.io/whep/reference/build_livestock_ghg_extension.md)
(enteric and manure CH4/N2O), which keys on the same live-animal
sectors.

The GLEAM energy emission factors are expressed per kilogram of **live
weight** (see
[gleam_energy_use_ef](https://eduaguilera.github.io/whep/reference/gleam_energy_use_ef.md)),
which is well defined for meat but not for milk or eggs, so the
extension covers **meat only**: bovine (`item_cbs_code` 961 non-dairy
cattle and 946 buffalo), sheep (976) and goat (1016), pig (1049 and
1051) and broiler-chicken (1053) meat. Milk and eggs keep their CH4/N2O
but get no energy CO2.

For each meat group the live weight produced is recovered from FAOSTAT
carcass production divided by a GLEAM dressing fraction
([gleam_dressing_percentages](https://eduaguilera.github.io/whep/reference/gleam_dressing_percentages.md)),
multiplied by a per-country energy intensity (embedded + direct), and
then attributed to the contributing live-animal sectors in proportion to
their slaughtered head counts. Because GLEAM reports its factors by
production system and climate zone but the package has no country-level
system or climate shares, the intensities are collapsed to one value per
country by an unweighted mean across systems and climate zones; this
choice is recorded in `method_energy`.

## Usage

``` r
build_energy_co2_extension(method = c("gleam"), data = list(), example = FALSE)
```

## Arguments

- method:

  Estimation method. Only `"gleam"` (default), the GLEAM 3.0
  per-live-weight factors, is currently available.

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
(energy-use emissions in kilograms CO2e) and `method_energy` (e.g.
`"GLEAM_3.0_energy_meat"`).

## Examples

``` r
build_energy_co2_extension(example = TRUE)
#> # A tibble: 8 × 5
#>    year area_code item_cbs_code    impact_u method_energy        
#>   <int>     <int>         <int>       <dbl> <chr>                
#> 1  2010        21           961  1766900000 GLEAM_3.0_energy_meat
#> 2  2010        21          1053  3407700000 GLEAM_3.0_energy_meat
#> 3  2010       231           961  8328900000 GLEAM_3.0_energy_meat
#> 4  2010       231           976    57395000 GLEAM_3.0_energy_meat
#> 5  2010       231          1016    17517000 GLEAM_3.0_energy_meat
#> 6  2010       231          1049  1928100000 GLEAM_3.0_energy_meat
#> 7  2010       231          1051   214230000 GLEAM_3.0_energy_meat
#> 8  2010       231          1053 11171000000 GLEAM_3.0_energy_meat
```
