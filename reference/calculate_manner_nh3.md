# Estimate ammonia-N volatilisation with the MANNER process-based model.

Ports the MANNER (Nicholson et al. 2013) process-based ammonia
volatilisation model. Dispatches on `fertiliser` to one of two
independent paths: a synthetic-fertiliser path (`"Urea"`, `"AN"`,
`"CAN"`, `"AS"`) driven by soil pH, application rate, rainfall and
temperature, and an organic-manure path (`"cattle_slurry"`,
`"pig_slurry"`, `"FYM"`, `"poultry_manure"`, `"urban"`) driven by
rainfall, temperature, wind speed, application technique, system
(arable/grassland) and incorporation delay.

## Usage

``` r
calculate_manner_nh3(
  n_applied_t = NULL,
  fertiliser = NULL,
  drivers = list(),
  example = FALSE
)
```

## Arguments

- n_applied_t:

  Numeric, nitrogen applied (t).

- fertiliser:

  One of `"Urea"`, `"AN"`, `"CAN"`, `"AS"` (synthetic path) or
  `"cattle_slurry"`, `"pig_slurry"`, `"FYM"`, `"poultry_manure"`,
  `"urban"` (organic path).

- drivers:

  A named list of driver values. Synthetic path: `soil_ph` (numeric soil
  pH), `rate_kg_ha` (numeric N application rate, kg N/ha), `rainfall_mm`
  (numeric period precipitation, mm), `irrigated` (logical), `temp_c`
  (numeric application-period temperature, deg C), `temp_c_annual_mean`
  (numeric annual mean temperature, deg C; only used for CAN/AS).
  Organic path: `rainfall_mm`, `irrigated`, `windspeed_ms` (numeric wind
  speed, m/s), `technique` (one of the six
  [manner_params](https://eduaguilera.github.io/whep/reference/manner_params.md)
  `technique` keys), `system` (`"Arable"` or `"Grassland"`), `temp_c`,
  `incorporation_delay_h` (numeric hours between surface application and
  soil incorporation, or `Inf`/`NA` for no incorporation), `species`
  (optional; one of the eight
  [manure_inorganic_n](https://eduaguilera.github.io/whep/reference/manure_inorganic_n.md)
  species, used only to look up `inorganic_n_fraction`; when omitted it
  falls back to the manure type's default species, and it is ignored
  entirely for `"urban"`).

- example:

  If `TRUE`, return a small fixture instead of computing from drivers.
  Defaults to `FALSE`.

## Value

A tibble with `n_applied_t`, `ef` (realised emission factor), `nh3_n_t`
and `method_manner`.

## Details

The organic path's `inorganic_n_fraction` lookup
([manure_inorganic_n](https://eduaguilera.github.io/whep/reference/manure_inorganic_n.md))
maps `manure_type` to its ammoniacal `manure_stream` (`"Liquid"` for
`"cattle_slurry"`/`"pig_slurry"`, `"Solid"` for
`"FYM"`/`"poultry_manure"`) and reads that stream's fraction for the
actual `species` driver supplied by the caller. This matches the source
implementation, which maps every species' solid stream to the FYM MANNER
class yet looks the inorganic-N fraction up per real species. When
`species` is omitted it falls back to the manure type's default species
(Cattle for `"cattle_slurry"`/`"FYM"`, Pigs for `"pig_slurry"`, Poultry
for `"poultry_manure"`). `"urban"` bypasses this lookup entirely: it
fixes `inorganic_n_fraction = 0.5` regardless of species, matching the
source implementation. For the AG availability and incorporation
factors, `"urban"` maps to the FYM manure class, as the source
coefficient table does, including the 0.4 Org_ef correction.

The synthetic path's temperature factor (Misselbrook et al. 2004,
doi:10.1079/SUM2004280) is an exponential scaled on UK monthly
temperatures. It is capped at 1, as the UK ammonia inventory that runs
this model caps it (Misselbrook et al. 2015, Inventory of Ammonia
Emissions from UK Agriculture 2014). Without the cap, warm application
months returned emission factors above 1. With it, `ef` never exceeds
the fertiliser's `max_nh3` in
[manner_params](https://eduaguilera.github.io/whep/reference/manner_params.md).

## Examples

``` r
calculate_manner_nh3(example = TRUE)
#> # A tibble: 1 × 4
#>   n_applied_t     ef nh3_n_t method_manner        
#>         <dbl>  <dbl>   <dbl> <chr>                
#> 1          10 0.0333   0.333 manner_synthetic_Urea
```
