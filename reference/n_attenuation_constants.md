# Nitrogen leaching-attenuation and indirect-N2O constants.

Scalar constants for the nitrogen-loss cascade: the input C:N leaching
attenuation parameters (`a_cn_*`) and the IPCC indirect-emission
factors. The C:N attenuation reduces nitrate leaching for carbon-rich,
low-availability inputs; the indirect factors convert volatilised
ammonia and leached nitrate into indirect N2O and set the
ammonia-volatilisation fractions.

## Usage

``` r
n_attenuation_constants
```

## Format

A tibble with columns:

- constant:

  Constant name: `"a_cn_min_cn"`, `"a_cn_span"`, `"a_cn_span_other"`,
  `"a_cn_max"`, `"ef5_no3_to_n2o"`, `"ef4_nh3_to_n2o_atl"`,
  `"nh3_frac_synthetic"`, `"nh3_frac_organic"`.

- value:

  Numeric value of the constant.

- description:

  Human-readable description of the constant.

## Source

C:N attenuation parameters from the Spain historical nitrogen pipeline
(`n_fun.r`). Indirect emission factors and ammonia volatilisation
fractions: IPCC (2019), 2019 Refinement to the 2006 IPCC Guidelines for
National Greenhouse Gas Inventories, Vol. 4, Chapter 11 (EF5 = 0.011 for
nitrate leaching to N2O; EF4 = 0.016 for Atlantic ammonia to N2O; NH3
volatilisation fractions 0.11 synthetic and 0.21 organic, Table 11.3).

## Examples

``` r
n_attenuation_constants
#> # A tibble: 8 × 3
#>   constant             value description                                        
#>   <chr>                <dbl> <chr>                                              
#> 1 a_cn_min_cn         15     Input C:N below which the C:N leaching attenuation…
#> 2 a_cn_span          120     C:N span above the minimum needed to reach the A_C…
#> 3 a_cn_span_other     60     Steeper C:N span for the Other land-use class (roc…
#> 4 a_cn_max             0.98  Maximum C:N leaching attenuation (forest woody-lit…
#> 5 ef5_no3_to_n2o       0.011 IPCC EF5: fraction of leached NO3-N emitted as ind…
#> 6 ef4_nh3_to_n2o_atl   0.016 Atlantic (wet) NH3-N to indirect N2O-N factor (IPC…
#> 7 nh3_frac_synthetic   0.11  IPCC 2019 Table 11.3 NH3-N volatilisation fraction…
#> 8 nh3_frac_organic     0.21  IPCC 2019 Table 11.3 NH3-N volatilisation fraction…
```
