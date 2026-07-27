# Subsoil nitrate reduction shares by source, climate and irrigation.

Fraction of leaching nitrate reduced (denitrified) in the subsoil below
the rooting zone, by nitrogen source, climate zone and irrigation
category. Applied after topsoil denitrification to compute the nitrate
that reaches groundwater.

## Usage

``` r
subsoil_no3_reduction
```

## Format

A tibble with columns:

- fert_type:

  Nitrogen source: one of `"Synthetic"`, `"SOM"`, `"Deposition"`,
  `"Solid"`, `"Excreta_cattle_monog"`, `"Excreta_other"`, `"Liquid"`,
  `"Urban"`, `"BNF"`.

- climate:

  Climate zone: `"MED"` or `"ATL"`.

- irrig_cat:

  Irrigation category: `"Rainfed"` or `"Irrigated"`.

- no3_red:

  Fraction of leaching nitrate reduced in the subsoil.

## Source

Spain historical nitrogen coefficient workbook (`N_coefficients.xlsx`,
sheet `Subsoil_NO3_denitrif`), parameterised from Mediterranean and
Atlantic subsoil denitrification literature consistent with the IPCC
(2019) indirect N2O framework.

## Examples

``` r
subsoil_no3_reduction
#> # A tibble: 36 × 4
#>    fert_type  climate irrig_cat no3_red
#>    <chr>      <chr>   <chr>       <dbl>
#>  1 Synthetic  MED     Rainfed       0.2
#>  2 Synthetic  MED     Irrigated     0.4
#>  3 Synthetic  ATL     Rainfed       0.5
#>  4 Synthetic  ATL     Irrigated     0.6
#>  5 SOM        MED     Rainfed       0.2
#>  6 SOM        MED     Irrigated     0.4
#>  7 SOM        ATL     Rainfed       0.5
#>  8 SOM        ATL     Irrigated     0.6
#>  9 Deposition MED     Rainfed       0.2
#> 10 Deposition MED     Irrigated     0.4
#> # ℹ 26 more rows
```
