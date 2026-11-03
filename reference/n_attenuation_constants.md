# Nitrogen leaching-attenuation and indirect-N2O constants.

Scalar constants for the nitrogen-loss cascade: the input C:N leaching
attenuation parameters (`a_cn_*`), the IPCC indirect-emission factors
and the constants of the nitrate transport factors of
[`calculate_n_leaching()`](https://eduaguilera.github.io/whep/reference/calculate_n_leaching.md).
The C:N attenuation reduces nitrate leaching for carbon-rich,
low-availability inputs; the indirect factors convert volatilised
ammonia and leached nitrate into indirect N2O and set the
ammonia-volatilisation fractions. `swat_anion_exclusion` is the SWAT
anion-exclusion fraction of porosity; `beusen_*` and
`molar_gas_constant` give the Arrhenius temperature effect on root-zone
denitrification of the IMAGE-GNM method, and `image_gnm_*` its root-zone
depth, the bounds its authors' model code places on the residence time
and the temperature effect, the total runoff below which it moves no
water through the root zone, the calibration factor of its surface
runoff nitrogen and the four constants of its erosion slope factor.
`water_flux_floor_mm` is the lowest drainage or total runoff accepted as
a float residue of zero, shared by the Meisinger and IMAGE-GNM checks of
[`calculate_n_leaching()`](https://eduaguilera.github.io/whep/reference/calculate_n_leaching.md).

## Usage

``` r
n_attenuation_constants
```

## Format

A tibble with columns:

- constant:

  Constant name: `"a_cn_min_cn"`, `"a_cn_span"`, `"a_cn_span_other"`,
  `"a_cn_max"`, `"ef5_no3_to_n2o"`, `"ef4_nh3_to_n2o_atl"`,
  `"nh3_frac_synthetic"`, `"nh3_frac_organic"`,
  `"swat_anion_exclusion"`, `"beusen_arrhenius_factor"`,
  `"beusen_activation_energy"`, `"molar_gas_constant"`,
  `"image_gnm_tawc_depth_m"`, `"image_gnm_crop_max_residence_yr"`,
  `"image_gnm_no_drainage_residence_yr"`,
  `"image_gnm_max_temperature_effect"`, `"image_gnm_min_runoff_mm"`,
  `"water_flux_floor_mm"`, `"image_gnm_runoff_calibration"`,
  `"image_gnm_erosion_slope_offset"`, `"image_gnm_erosion_slope_range"`,
  `"image_gnm_erosion_slope_intercept"`,
  `"image_gnm_erosion_slope_rate"`.

- value:

  Numeric value of the constant.

- description:

  Human-readable description of the constant.

## Source

C:N attenuation parameters from the nitrogen pipeline of an earlier
regional historical reconstruction. Indirect emission factors and
ammonia volatilisation fractions: IPCC (2019), 2019 Refinement to the
2006 IPCC Guidelines for National Greenhouse Gas Inventories, Vol. 4,
Chapter 11 (EF5 = 0.011 for nitrate leaching to N2O; EF4 = 0.014 for
Atlantic, i.e. wet-climate, ammonia to N2O, p. 11.26; NH3 volatilisation
fractions 0.11 synthetic and 0.21 organic, Table 11.3). Anion exclusion
0.5: the ANION_EXCL default of the SWAT Input/Output File Documentation,
Version 2012, ch. 22, p. 304, as used in Neitsch et al. (2011), SWAT
2009 Theoretical Documentation, TR-406, eqs. 4:2.1.2 to 4:2.1.8.
Arrhenius factor 7.94e12, activation energy 74,830 J/mol and gas
constant 8.3144 J/(mol K): Beusen et al. (2015), Geosci. Model Dev.
8:4045-4067, doi:10.5194/gmd-8-4045-2015, eqs. 12-13, p. 4052. The 1 m
depth of the total available water capacity: the same paper, eq. 14, p.
4052 (the top 1 m "or less if thinner"; WHEP has no soil-thickness
layer, so the full 1 m is used). The cropland residence time of at most
1 year, the 100-year residence time without excess water and the bound
of 1 on the temperature effect: the authors' IMAGE-GNM model code
published with the paper, `n_soil_denitrification.py` (in
`N_model/trunk/tools/`), lines 377-378, 179-182 and 97. The 3 mm/yr
total runoff below which no water moves through the root zone: the same
file, lines 341-344, with `minimal_soil_infiltration = 3.0` (mm/yr) in
`parameters_history_100.ini`, lines 203-204; the same threshold sets dry
grassland and natural surpluses aside as inactive N in `n_nowater.py`,
lines 31-46. The water-flux floor of -0.1 mm is assumed, unverified: the
lower edge of the None bin of
[drainage_ranges](https://eduaguilera.github.io/whep/reference/drainage_ranges.md),
with no source recorded for that edge. The surface runoff calibration
factor 0.3: the same paper, `f_cal` of eq. 9, p. 4052, and `C_sro,N` of
Table 3, p. 4056. The erosion slope factor
`a + b / (1 + exp(c - d S / 1000))` with `a = -1.5`, `b = 17`, `c = 2.3`
and `d = 6.1`: the authors' code, `n_erosion.py`, line 50, for the soil
loss of Cerdan et al. (2010) the paper cites on p. 4052.

## Examples

``` r
n_attenuation_constants
#> # A tibble: 26 × 3
#>    constant                   value description                                 
#>    <chr>                      <dbl> <chr>                                       
#>  1 a_cn_min_cn             1.5 e+ 1 Input C:N below which the C:N leaching atte…
#>  2 a_cn_span               1.2 e+ 2 C:N span above the minimum needed to reach …
#>  3 a_cn_span_other         6   e+ 1 Steeper C:N span for the Other land-use cla…
#>  4 a_cn_max                9.8 e- 1 Maximum C:N leaching attenuation (forest wo…
#>  5 ef5_no3_to_n2o          1.1 e- 2 IPCC EF5: fraction of leached NO3-N emitted…
#>  6 ef4_nh3_to_n2o_atl      1.4 e- 2 Atlantic (wet) NH3-N to indirect N2O-N fact…
#>  7 nh3_frac_synthetic      1.1 e- 1 IPCC 2019 Table 11.3 NH3-N volatilisation f…
#>  8 nh3_frac_organic        2.1 e- 1 IPCC 2019 Table 11.3 NH3-N volatilisation f…
#>  9 swat_anion_exclusion    5   e- 1 SWAT fraction of porosity from which nitrat…
#> 10 beusen_arrhenius_factor 7.94e+12 IMAGE-GNM pre-exponential factor of the roo…
#> # ℹ 16 more rows
```
