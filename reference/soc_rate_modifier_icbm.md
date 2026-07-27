# Compute the ICBM annual climate rate modifier.

Annual ICBM re_clim rate-modifying factor: a Ratkowsky-type temperature
response and a piecewise volumetric-moisture response, multiplied and
rescaled to the Swedish Ultuna reference site, averaged over the
supplied series.

## Usage

``` r
soc_rate_modifier_icbm(temp_c, theta, t_field, t_wilt, porosity)
```

## Source

Katterer, T., Reichstein, M., Andren, O. & Lomander, A. (1998).
Temperature dependence of organic matter decomposition.
[doi:10.1007/s003740050430](https://doi.org/10.1007/s003740050430) .
Normalization and piecewise moisture form as implemented in the
canonical reclim package.

## Arguments

- temp_c:

  Numeric soil (or air-proxy) temperature series (degrees Celsius).

- theta:

  Numeric volumetric soil water content series (fraction).

- t_field:

  Field-capacity volumetric water content (fraction).

- t_wilt:

  Wilting-point volumetric water content (fraction).

- porosity:

  Soil porosity (fraction).

## Value

The annual mean of the monthly re_clim values (a single numeric).

## Examples

``` r
soc_rate_modifier_icbm(
  temp_c = c(5, 15, 25),
  theta = c(0.20, 0.25, 0.30),
  t_field = 0.30,
  t_wilt = 0.10,
  porosity = 0.45
)
#> [1] 3.162084
```
