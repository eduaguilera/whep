# IPCC 2006 Tier 1 direct soil N2O emission factors by climate and irrigation.

Direct nitrous-oxide emission factors (fraction of applied nitrogen
emitted as N2O-N) under the IPCC 2006 Guidelines Tier 1 defaults,
disaggregated by irrigation/management stratum and climate zone. Unlike
[`n2o_efs_disaggregated()`](https://eduaguilera.github.io/whep/reference/n2o_efs_disaggregated.md)
(the Cayuela et al. 2017 meta-analytic Mediterranean factors plus the
IPCC 2019 Atlantic default), every stratum here carries the flat IPCC
2006 default (0.010), except flooded rice (0.003) in both climate zones.
Used by
[`calculate_soil_n2o()`](https://eduaguilera.github.io/whep/reference/calculate_soil_n2o.md)'s
`method = "ipcc2006"`.

## Usage

``` r
n2o_efs_ipcc2006
```

## Format

A tibble with columns:

- irrig_type:

  Irrigation / management stratum: one of `"Tier_1"`, `"Rainfed"`,
  `"Traditional"`, `"Drip"`, `"Sprinkler"`, `"Flooded"`,
  `"Med_average"`.

- climate:

  Climate zone: `"MED"` (Mediterranean) or `"ATL"` (Atlantic).

- ef:

  Direct N2O emission factor (kg N2O-N per kg applied N).

## Source

WHEP project-internal coefficient workbook (not independently
DOI-verified): `N_coefficients.xlsx`, sheet `N2O_EFs_IPCC2006`, itself
transcribed from IPCC (2006), 2006 IPCC Guidelines for National
Greenhouse Gas Inventories, Vol. 4, Chapter 11, Tier 1 defaults.

## Examples

``` r
n2o_efs_ipcc2006
#> # A tibble: 12 × 3
#>    irrig_type  climate    ef
#>    <chr>       <chr>   <dbl>
#>  1 Tier_1      ATL     0.01 
#>  2 Rainfed     MED     0.01 
#>  3 Traditional MED     0.01 
#>  4 Drip        MED     0.01 
#>  5 Sprinkler   MED     0.01 
#>  6 Flooded     MED     0.003
#>  7 Med_average MED     0.01 
#>  8 Rainfed     ATL     0.01 
#>  9 Traditional ATL     0.01 
#> 10 Drip        ATL     0.01 
#> 11 Sprinkler   ATL     0.01 
#> 12 Flooded     ATL     0.003
```
