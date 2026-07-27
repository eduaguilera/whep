# Build the historical gridded soil-organic-carbon balance.

Reconstruct per-cell soil-organic-carbon stock trajectories: run the
selected turnover model to equilibrium under the earliest per-land-use
carbon inputs, initialise each cell by weighting those equilibria with
the earliest land-use fractions, march forward on yearly per-cell
per-land-use areas applying the model annual update plus a
carbon-conserving land-use-change transfer, and derive the
soil-organic-nitrogen change from the carbon rate via asymmetric soil
carbon-to-nitrogen ratios.

## Usage

``` r
build_carbon_balance(
  model = c("hsoc", "rothc", "icbm", "amg", "century"),
  resolution = c("grid", "polity"),
  data = list(),
  years = NULL,
  example = FALSE
)
```

## Source

Aguilera, E. et al. (2018). Embodied energy in agricultural inputs.
[doi:10.1016/j.scitotenv.2018.03.118](https://doi.org/10.1016/j.scitotenv.2018.03.118)
; land-use-change carbon transfer ported from the Spain historical
pipeline.

## Arguments

- model:

  Turnover model: one of `"hsoc"` (default), `"rothc"`, `"icbm"`,
  `"amg"` or `"century"`.

- resolution:

  `"grid"` (default, per cell and land-use class) or `"polity"`
  (aggregated to `area_code` conserving carbon mass).

- data:

  Named list of pre-loaded inputs, each falling back to its reader when
  absent: `c_inputs` (per cell, land-use class and year, with
  `c_input_mgc_ha_yr` and `humified_fraction`); `land_use` (yearly
  per-cell per-class `lon`, `lat`, `area_code`, `year`, `land_use`,
  `area_ha`); `climate` (either a precomputed per cell-year
  `climate_modifier`, applied to every land-use class alike, or the raw
  monthly drivers `temp_c` and `water_minus_pet_mm` keyed by `lon`,
  `lat`, `area_code`, `year`, `month`, from which the selected model's
  native modifier is computed internally per land-use class: for the
  RothC/HSOC cover term the monthly vegetated soil-cover fraction is
  taken from the generic land-use curve
  [`soc_soil_cover_curve`](https://eduaguilera.github.io/whep/reference/soc_soil_cover_curve.md)
  (a crop growth-stage canopy for cropland, sustained perennial cover
  for grassland/natural), so any `soil_cover` column supplied on the raw
  drivers is ignored); `clay` (per cell `clay_pct`); and an optional
  `equilibrium_climate` (the pre-industrial climatological normal, one
  representative monthly cycle per cell, used only for the equilibrium
  spin-up modifier while the forward march uses the year-specific
  drivers).

- years:

  Optional integer vector of calendar years to keep. `NULL` (default)
  keeps every year the inputs cover, but reading the full LUH2 range
  (850-2015) is infeasible turnkey, so a subset is strongly recommended
  when the default readers are used. Threaded into every default reader
  ([`read_luh2_landuse`](https://eduaguilera.github.io/whep/reference/read_luh2_landuse.md),
  [`get_soc_climate_drivers`](https://eduaguilera.github.io/whep/reference/get_soc_climate_drivers.md)
  and
  [`build_carbon_inputs`](https://eduaguilera.github.io/whep/reference/build_carbon_inputs.md));
  ignored for inputs supplied via `data`.

- example:

  If `TRUE`, return a small fixture instead of reading remote data.
  Defaults to `FALSE`.

## Value

A tibble keyed by `(lon, lat, area_code, land_use, year)` at `"grid"`
resolution (or `(area_code, year)` at `"polity"`), with `stock_mgc_ha`,
`mineralization_mgc_ha`, `c_input_mgc_ha`, `luc_transfer_mgc_ha`,
`rate_mgc_ha`, `son_change_kgn_ha`, `area_ha` and `method_soc`.

## Examples

``` r
build_carbon_balance(example = TRUE)
#> # A tibble: 6 × 13
#>     lon   lat area_code land_use     year area_ha stock_mgc_ha
#>   <dbl> <dbl>     <int> <chr>       <int>   <dbl>        <dbl>
#> 1  0.25  0.25         1 Cropland     2000      60         37.3
#> 2  0.25  0.25         1 NonCropland  2000      40         37.3
#> 3  0.25  0.25         1 Cropland     2001      50         37.7
#> 4  0.25  0.25         1 NonCropland  2001      50         36.9
#> 5  0.25  0.25         1 Cropland     2002      50         38.1
#> 6  0.25  0.25         1 NonCropland  2002      50         36.4
#> # ℹ 6 more variables: mineralization_mgc_ha <dbl>, c_input_mgc_ha <dbl>,
#> #   luc_transfer_mgc_ha <dbl>, rate_mgc_ha <dbl>, son_change_kgn_ha <dbl>,
#> #   method_soc <chr>
```
