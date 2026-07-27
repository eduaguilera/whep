# Compute the RothC and HSOC annual climate rate modifier.

Annual a*b*c rate-modifying factor of the RothC / HSOC soil-carbon
model: the temperature factor a, the topsoil-moisture-deficit factor b,
and the plant-cover factor c, averaged over the supplied monthly series.
The same function modifies HSOC decomposition.

## Usage

``` r
soc_rate_modifier_rothc(
  temp_c,
  water_minus_pet_mm,
  clay_pct,
  soil_cover,
  soil_depth_m = 0.3
)
```

## Source

Coleman, K. & Jenkinson, D. S. (1996). RothC-26.3: a model for the
turnover of carbon in soil.
[doi:10.1007/978-3-642-61094-3_17](https://doi.org/10.1007/978-3-642-61094-3_17)
. Moisture deficit and cover terms as implemented in the Spain
historical SOC pipeline.

## Arguments

- temp_c:

  Numeric monthly air temperature series (degrees Celsius).

- water_minus_pet_mm:

  Numeric monthly water surplus series (precipitation minus potential
  evapotranspiration, mm), used to accumulate the topsoil moisture
  deficit.

- clay_pct:

  Soil clay content (percent).

- soil_cover:

  Vegetated soil-cover fraction (0 bare, 1 fully covered); a scalar or
  monthly series.

- soil_depth_m:

  Topsoil depth over which the moisture deficit is accumulated (metres).
  Defaults to 0.3.

## Value

The annual mean of the monthly a*b*c product (a single numeric).

## Examples

``` r
soc_rate_modifier_rothc(
  temp_c = c(1.2, 7.8, 15.0),
  water_minus_pet_mm = c(-10, 5, 20),
  clay_pct = 20,
  soil_cover = 0
)
#> [1] 0.969915
```
