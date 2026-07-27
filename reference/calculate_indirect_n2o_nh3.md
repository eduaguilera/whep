# Estimate indirect N2O from volatilised ammonia.

Converts the ammonia-N already volatilised
([`calculate_nh3()`](https://eduaguilera.github.io/whep/reference/calculate_nh3.md)'s
`nh3_n_t`) into indirect nitrous oxide (`n_fun.r:955-957`). Atlantic
rows use the flat IPCC EF4 factor (`ef4_nh3_to_n2o_atl`, 0.016) and
touch no emission-factor lookup; Mediterranean rows use the
disaggregated
[n2o_efs_disaggregated](https://eduaguilera.github.io/whep/reference/n2o_efs_disaggregated.md)
`ef` on `(irrig_type, climate)` alone (`NH3_MgN * N2O_EF`), WITHOUT the
[fertiliser_n2o_modifiers](https://eduaguilera.github.io/whep/reference/fertiliser_n2o_modifiers.md)
`mf` that
[`calculate_soil_n2o()`](https://eduaguilera.github.io/whep/reference/calculate_soil_n2o.md)'s
`method = "aguilera"` applies to direct N2O.

## Usage

``` r
calculate_indirect_n2o_nh3(x, example = FALSE)
```

## Arguments

- x:

  A tibble with `nh3_n_t`, `climate` and (for MED rows) the `irrig_type`
  column
  [n2o_efs_disaggregated](https://eduaguilera.github.io/whep/reference/n2o_efs_disaggregated.md)
  is keyed on.

- example:

  If `TRUE`, return a small fixture instead of computing from `x`.
  Defaults to `FALSE`.

## Value

`x` with `n2o_indirect_nh3_n_t` appended.

## Examples

``` r
calculate_indirect_n2o_nh3(example = TRUE)
#> # A tibble: 1 × 3
#>   nh3_n_t climate n2o_indirect_nh3_n_t
#>     <dbl> <chr>                  <dbl>
#> 1     1.1 ATL                   0.0176
```
