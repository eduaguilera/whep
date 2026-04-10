# Indirect N2O emission factors.

Parameters for indirect N2O emissions from manure management: EF4
(volatilization), EF5 (leaching), FracGasMS, FracLeach.

## Usage

``` r
indirect_n2o_ef
```

## Format

A tibble with `parameter`, `value`, `description`.

## Source

IPCC 2019, Vol 4, Ch 10, Table 10.22; Vol 4, Ch 11, Table 11.3.

## Examples

``` r
indirect_n2o_ef
#> # A tibble: 4 × 3
#>   parameter           value description                                   
#>   <chr>               <dbl> <chr>                                         
#> 1 ef4_volatilization 0.01   EF4: N2O-N per kg NH3-N + NOx-N volatilized   
#> 2 ef5_leaching       0.0075 EF5: N2O-N per kg N leached/runoff            
#> 3 frac_gasms         0.2    FracGasMS: fraction N lost as NH3+NOx from MMS
#> 4 frac_leach         0.3    FracLeach: fraction N lost via leaching/runoff
```
