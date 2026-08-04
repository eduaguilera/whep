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
`rate_mgc_ha`, `son_change_kgn_ha`, `area_ha` and `method_soc`, plus the
polity columns below.

## Polity columns

Every area-keyed output carries the polity its `area_code` resolves to
in that row's year:

- `polity_area_code`: The numeric key rows are AGGREGATED on, for the
  matrix workflows. It is a bucket, not an identity: use
  `reporting_polity_code` to say which territory a row belongs to.

- `reporting_polity_code`: The polity itself, e.g. `ESP-1846-1914`. It
  is year-aware, so the same `area_code` resolves to different polities
  in different years, which is the point of the crosswalk.

- `reporting_polity_name`: Its name. It can differ from the area's own
  name where the area folds into an aggregate.

- `reporting_polity_has_geometry`: Whether the polity has a polygon in
  the WHEP polity database, for callers that need to map or intersect
  it. `FALSE` is a documented gap upstream, not an error.

Rows whose `area_code` resolves to no polity keep the columns with `NA`
rather than being dropped, so a gap is visible instead of silent.

Rows before the back-cast anchor year resolve to the polity live in that
anchor year rather than to the polity live in the row's own year,
because WHEP's pre-anchor series are back-cast onto the anchor-year
territory. See
[`add_polity_code()`](https://eduaguilera.github.io/whep/reference/add_polity_code.md)
for the reasoning.

## Examples

``` r
build_carbon_balance(example = TRUE)
#> # A tibble: 6 × 17
#>    year area_code polity_area_code reporting_polity_code reporting_polity_name
#>   <int>     <int>            <int> <chr>                 <chr>                
#> 1  2000         1                1 ARM-1991-2025         Armenia              
#> 2  2000         1                1 ARM-1991-2025         Armenia              
#> 3  2001         1                1 ARM-1991-2025         Armenia              
#> 4  2001         1                1 ARM-1991-2025         Armenia              
#> 5  2002         1                1 ARM-1991-2025         Armenia              
#> 6  2002         1                1 ARM-1991-2025         Armenia              
#> # ℹ 12 more variables: reporting_polity_has_geometry <lgl>, lon <dbl>,
#> #   lat <dbl>, land_use <chr>, area_ha <dbl>, stock_mgc_ha <dbl>,
#> #   mineralization_mgc_ha <dbl>, c_input_mgc_ha <dbl>,
#> #   luc_transfer_mgc_ha <dbl>, rate_mgc_ha <dbl>, son_change_kgn_ha <dbl>,
#> #   method_soc <chr>
```
