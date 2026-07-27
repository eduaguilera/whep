# Generic land-use soil-cover curve for the RothC/HSOC cover factor.

Monthly vegetated soil-cover fraction (0 bare, 1 fully covered) by
land-use class, driving the plant-cover term of the RothC and HSOC
decomposition modifier
([`soc_rate_modifier_rothc`](https://eduaguilera.github.io/whep/reference/soc_rate_modifier_rothc.md),
where the cover factor is `0.6 + 0.4 * (1 - soil_cover)`: covered soil
decomposes more slowly than bare soil).
[`build_carbon_balance`](https://eduaguilera.github.io/whep/reference/build_carbon_balance.md)
attaches these values to each cell-month before reducing the monthly
climate drivers to an annual modifier, so cropland (a seasonal crop
canopy with a bare fallow period) mineralizes soil carbon differently
from grassland and natural land (perennial cover). The curve is keyed on
the signed month offset from the cell-year's warmest (peak-canopy) month
rather than the calendar month, so the same generic curve aligns to the
growing season in either hemisphere from the temperature seasonality
alone.

For cropland the fractions follow the FAO-56 crop growth-stage canopy
development of a generic 150-day annual field crop (the FAO-56
maize-grain template, 30/40/50/30 days for the initial, development,
mid-season and late-season stages): a low establishment cover during the
initial stage (about 10 percent ground cover), rising through the
development stage to an effective full canopy at mid-season (the peak,
offset 0), then declining through late-season senescence, with the
remaining fallow/off-season months at a low bare-soil cover. Grassland
and natural land carry a sustained high perennial cover in every month
(no bare fallow period), following the RothC convention that permanent
vegetation is treated as covered year-round.

## Usage

``` r
soc_soil_cover_curve
```

## Format

A tibble with columns:

- land_use:

  Land-use class: `"cropland"`, `"grassland"` or `"natural"` (matched
  case-insensitively; a class absent from the table, such as `"urban"`,
  is treated as bare soil).

- months_from_peak:

  Signed month offset from the warmest month of the cell-year (integer
  `-5` to `6`; `0` is the peak-canopy mid-season month).

- soil_cover:

  Vegetated soil-cover fraction for that class and month offset (0 bare,
  1 fully covered).

## Source

Crop canopy development stages and their durations follow the FAO-56
crop coefficient framework: Allen, R. G., Pereira, L. S., Raes, D. &
Smith, M. (1998). *Crop evapotranspiration: Guidelines for computing
crop water requirements* (FAO Irrigation and Drainage Paper 56). Food
and Agriculture Organization of the United Nations, Rome (Chapter 6 and
Table 11: initial stage to about 10 percent ground cover, development to
effective full cover, full canopy at mid-season, senescence in
late-season; generic 150-day annual-crop template). The binary
covered-versus-bare cover convention it feeds is that of the RothC
model: Coleman, K. & Jenkinson, D. S. (1996). RothC-26.3: a model for
the turnover of carbon in soil.
[doi:10.1007/978-3-642-61094-3_17](https://doi.org/10.1007/978-3-642-61094-3_17)
(the soil cover rate-modifying factor slows decomposition when growing
plants are present). The perennial grassland/natural cover and the
fallow-period bare-soil value are generic modelling assumptions, not a
per-crop observational survey.

## Examples

``` r
soc_soil_cover_curve
#> # A tibble: 36 × 3
#>    land_use months_from_peak soil_cover
#>    <chr>               <int>      <dbl>
#>  1 cropland               -5       0.05
#>  2 cropland               -4       0.05
#>  3 cropland               -3       0.05
#>  4 cropland               -2       0.15
#>  5 cropland               -1       0.55
#>  6 cropland                0       0.95
#>  7 cropland                1       0.75
#>  8 cropland                2       0.3 
#>  9 cropland                3       0.05
#> 10 cropland                4       0.05
#> # ℹ 26 more rows
```
