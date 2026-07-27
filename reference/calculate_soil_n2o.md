# Estimate direct soil N2O emissions from applied nitrogen.

Three emission-factor regimes for direct nitrous oxide from nitrogen
applied to soil. `"ipcc2019"` (the default) is the IPCC 2019 Tier 1
climate-disaggregated `EF1`, needing only `climate`: it reuses the
[n2o_efs_disaggregated](https://eduaguilera.github.io/whep/reference/n2o_efs_disaggregated.md)
table's two climate-level rows (`irrig_type == "Tier_1"` for ATL,
`irrig_type == "Med_average"` for MED, 0.010 wet / 0.005 dry) with no
`mf` multiplier; the ATL value (0.010) is the same value documented as
`EF1` in
[`build_crop_soil_n2o_extension()`](https://eduaguilera.github.io/whep/reference/build_crop_soil_n2o_extension.md),
pulled from one shared source of truth rather than hardcoded a second
time. It is the default because it is the internationally standard,
globally applicable Tier 1 method. `"aguilera"` (`n_fun.r:906-912`) is a
finer Mediterranean-calibrated disaggregation (Cayuela et al. 2017),
selectable where its `irrig_type`/`fert_type` granularity is available
and its regional emission factors apply:
`n2o_direct_n_t = n_input_t * ef * mf`, `ef` from
[n2o_efs_disaggregated](https://eduaguilera.github.io/whep/reference/n2o_efs_disaggregated.md)
on `(irrig_type, climate)`, `mf` from
[fertiliser_n2o_modifiers](https://eduaguilera.github.io/whep/reference/fertiliser_n2o_modifiers.md)
on `(fert_type, climate)`. `"ipcc2006"` uses the
[n2o_efs_ipcc2006](https://eduaguilera.github.io/whep/reference/n2o_efs_ipcc2006.md)
table (IPCC 2006 Tier 1 defaults, flat 0.010 except flooded rice 0.003),
keyed like `"aguilera"` on `(irrig_type, climate)` with no `mf`
multiplier.

## Usage

``` r
calculate_soil_n2o(
  x,
  method = c("ipcc2019", "aguilera", "ipcc2006"),
  example = FALSE
)
```

## Arguments

- x:

  A tibble with `n_input_t` and `climate`. `method = "aguilera"` or
  `"ipcc2006"` additionally require `irrig_type` and (aguilera only)
  `fert_type`.

- method:

  `"ipcc2019"` (default, IPCC 2019 Tier 1, climate-only), `"aguilera"`
  (Mediterranean-calibrated, needs `irrig_type`/`fert_type`) or
  `"ipcc2006"` (IPCC 2006 Tier 1, needs `irrig_type`).

- example:

  If `TRUE`, return a small fixture instead of computing from `x`.
  Defaults to `FALSE`.

## Value

`x` with `n2o_direct_n_t` and `method_soil_n2o` appended.

## Examples

``` r
calculate_soil_n2o(example = TRUE)
#> # A tibble: 1 × 5
#>   n_input_t climate irrig_type  n2o_direct_n_t method_soil_n2o
#>       <dbl> <chr>   <chr>                <dbl> <chr>          
#> 1        10 MED     Med_average           0.05 ipcc2019       
```
