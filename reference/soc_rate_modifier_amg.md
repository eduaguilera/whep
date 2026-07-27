# Compute the AMG (AMGv2) annual climate rate modifier.

Annual AMGv2 climate rate-modifying factor: the logistic temperature
function f(T) normalized to 1 at the 15 degree reference and the
logistic moisture function f(H) of the annual water balance, multiplied
and averaged over the supplied series.

## Usage

``` r
soc_rate_modifier_amg(temp_c, water_balance_mm)
```

## Source

Clivot, H., Mouny, J.-C., Duparque, A., Dinh, J.-L., Denoroy, P., ...
Mary, B. (2019). Modeling soil organic carbon evolution in long-term
arable experiments with AMG model.
[doi:10.1016/j.envsoft.2019.04.004](https://doi.org/10.1016/j.envsoft.2019.04.004)
; temperature form from Saffih-Hdadi, K. & Mary, B. (2008).
[doi:10.1016/j.soilbio.2007.08.022](https://doi.org/10.1016/j.soilbio.2007.08.022)
.

## Arguments

- temp_c:

  Numeric mean annual air temperature series (degrees Celsius).

- water_balance_mm:

  Numeric annual water balance series (precipitation plus irrigation
  minus potential evapotranspiration, mm).

## Value

The annual mean of the f(T) \* f(H) product (a single numeric).

## Examples

``` r
soc_rate_modifier_amg(
  temp_c = c(8, 15, 22),
  water_balance_mm = c(-100, 0, 200)
)
#> [1] 1.189645
```
