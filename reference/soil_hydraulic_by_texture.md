# Soil hydraulic properties by USDA texture class.

Class-average volumetric soil water contents by USDA texture class:
total porosity (saturation), field capacity (33 kPa) and permanent
wilting point (1500 kPa), each as a volumetric fraction (cubic metre
water per cubic metre soil). These are the per-cell soil hydraulic
drivers the ICBM soil-carbon moisture modifier consumes
([`soc_rate_modifier_icbm()`](https://eduaguilera.github.io/whep/reference/soc_rate_modifier_icbm.md)'s
`t_field`, `t_wilt` and `porosity`);
[`read_soil_hydraulic()`](https://eduaguilera.github.io/whep/reference/read_soil_hydraulic.md)
joins this table onto the dominant HWSD texture class of each grid cell
to emit those columns, which then feed
[`get_soc_climate_drivers()`](https://eduaguilera.github.io/whep/reference/get_soc_climate_drivers.md).
Every row satisfies `porosity > field_capacity > wilting_point` with all
three in `(0, 1)`.

## Usage

``` r
soil_hydraulic_by_texture
```

## Format

A tibble with columns:

- usda_texture_class:

  USDA texture class (snake_case): `"sand"`, `"loamy_sand"`,
  `"sandy_loam"`, `"silt_loam"`, `"silt"`, `"loam"`,
  `"sandy_clay_loam"`, `"silty_clay_loam"`, `"clay_loam"`,
  `"sandy_clay"`, `"silty_clay"` or `"clay"`.

- porosity:

  Total porosity / saturated volumetric water content (fraction).

- field_capacity:

  Volumetric water content at field capacity, 33 kPa (fraction).

- wilting_point:

  Volumetric water content at the permanent wilting point, 1500 kPa
  (fraction).

## Source

Class-average hydraulic properties compiled by texture class in "Average
hydraulic properties of ARS soil texture classes" (Schaake, J., draft
February 2000; 2128 soil samples), the values used as the default
soil-texture lookup in the VIC land-surface model
(<https://vic.readthedocs.io/en/master/Documentation/soiltext/>). The
underlying regression relating the water-retention parameters to texture
is Cosby, B. J., Hornberger, G. M., Clapp, R. B. & Ginn, T. R. (1984). A
statistical exploration of the relationships of soil moisture
characteristics to the physical properties of soils. *Water Resources
Research*, 20(6), 682-690.
[doi:10.1029/WR020i006p00682](https://doi.org/10.1029/WR020i006p00682) .

## Examples

``` r
soil_hydraulic_by_texture
#> # A tibble: 12 × 4
#>    usda_texture_class porosity field_capacity wilting_point
#>    <chr>                 <dbl>          <dbl>         <dbl>
#>  1 sand                   0.43           0.08          0.03
#>  2 loamy_sand             0.42           0.15          0.06
#>  3 sandy_loam             0.4            0.21          0.09
#>  4 silt_loam              0.46           0.32          0.12
#>  5 silt                   0.52           0.28          0.08
#>  6 loam                   0.43           0.29          0.14
#>  7 sandy_clay_loam        0.39           0.27          0.17
#>  8 silty_clay_loam        0.48           0.36          0.21
#>  9 clay_loam              0.46           0.34          0.21
#> 10 sandy_clay             0.41           0.31          0.23
#> 11 silty_clay             0.49           0.37          0.25
#> 12 clay                   0.47           0.36          0.27
```
