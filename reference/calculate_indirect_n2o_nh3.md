# Estimate indirect N2O from volatilised ammonia.

Converts the ammonia-N already volatilised
([`calculate_nh3()`](https://eduaguilera.github.io/whep/reference/calculate_nh3.md)'s
`nh3_n_t`) into indirect nitrous oxide. `"ipcc2019"` (the default) is
the IPCC 2019 Tier 1 wet/dry `EF4`, read from
[ipcc_2019_soil_n_ef](https://eduaguilera.github.io/whep/reference/ipcc_2019_soil_n_ef.md)
on each row's `moisture_regime`: 0.014 wet and 0.005 dry (IPCC 2019
Refinement Vol 4 Ch 11 Table 11.3 (Updated) p. 11.26), stamped
`"ipcc2019_wet_dry"`. `"ipcc2019_atl_med"` is the Atlantic/Mediterranean
option: Atlantic rows use the flat EF4 wet factor (`ef4_nh3_to_n2o_atl`,
0.014) and touch no emission-factor lookup; Mediterranean rows use the
disaggregated
[n2o_efs_disaggregated](https://eduaguilera.github.io/whep/reference/n2o_efs_disaggregated.md)
`ef` on `(irrig_type, climate)` alone (`NH3_MgN * N2O_EF`), WITHOUT the
[fertiliser_n2o_modifiers](https://eduaguilera.github.io/whep/reference/fertiliser_n2o_modifiers.md)
`mf` that
[`calculate_soil_n2o()`](https://eduaguilera.github.io/whep/reference/calculate_soil_n2o.md)'s
`method = "aguilera"` applies to direct N2O. It is stamped
`"ipcc2019_atl_med"`.

## Usage

``` r
calculate_indirect_n2o_nh3(
  x,
  method = c("ipcc2019", "ipcc2019_atl_med"),
  example = FALSE
)
```

## Source

IPCC (2019) 2019 Refinement to the 2006 IPCC Guidelines for National
Greenhouse Gas Inventories, Vol 4, Ch 11, Table 11.3 (Updated), p.
11.26.

## Arguments

- x:

  A tibble with `nh3_n_t`. `method = "ipcc2019"` requires
  `moisture_regime` (`"wet"` or `"dry"`); `method = "ipcc2019_atl_med"`
  requires `climate` and, for MED rows, the `irrig_type` column
  [n2o_efs_disaggregated](https://eduaguilera.github.io/whep/reference/n2o_efs_disaggregated.md)
  is keyed on.

- method:

  `"ipcc2019"` (default, IPCC 2019 Tier 1 wet/dry) or
  `"ipcc2019_atl_med"` (ATL flat EF4, MED disaggregated by irrigation).

- example:

  If `TRUE`, return a small fixture instead of computing from `x`.
  Defaults to `FALSE`.

## Value

`x` with `n2o_indirect_nh3_n_t` and `method_n2o_indirect_nh3` appended.

## Details

`method = "ipcc2019"` aborts on an absent, missing or invalid
`moisture_regime` exactly as
[`calculate_soil_n2o()`](https://eduaguilera.github.io/whep/reference/calculate_soil_n2o.md)
does.

## Examples

``` r
calculate_indirect_n2o_nh3(example = TRUE)
#> # A tibble: 2 × 4
#>   nh3_n_t moisture_regime n2o_indirect_nh3_n_t method_n2o_indirect_nh3
#>     <dbl> <chr>                          <dbl> <chr>                  
#> 1     1.1 wet                           0.0154 ipcc2019_wet_dry       
#> 2     1.1 dry                           0.0055 ipcc2019_wet_dry       
```
