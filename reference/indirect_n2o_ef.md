# Indirect N2O emission factors.

Parameters for indirect N2O emissions from manure management: EF4
(volatilization), EF5 (leaching), FracGasMS, FracLeach, one block per
IPCC edition. The manure engine reads the block its
`indirect_n2o_source` option names (see
[`apply_management_losses()`](https://eduaguilera.github.io/whep/reference/apply_management_losses.md));
the default is `"ipcc_2019"`.

Verified against both editions of Vol 4, Ch 11, Table 11.3 (whep#1245):

- `ef4_volatilization` 0.010 in both (aggregated value).

- `ef5_leaching` 0.011 in the 2019 Refinement, 0.0075 in the 2006
  Guidelines.

- `frac_leach` is FracLEACH-(H): 0.24 in the 2019 Refinement, 0.30 in
  the 2006 Guidelines. Both editions apply it only where leaching occurs
  (wet climates in 2019; where the soil water-holding capacity is
  exceeded in 2006) and take it as zero elsewhere; WHEP applies it
  everywhere.

Until whep#1245 the table cited 2019 while holding the 2006 EF5 and
FracLEACH-(H).

`frac_gasms` 0.20 is the same in both blocks and is **assumed,
unverified**: Table 10.22 of either edition publishes FracGasMS per
animal category and manure system, not one number, and 0.20 equals the
2006 FracGASM of Table 11.3 (the 2019 FracGASM is 0.21).

## Usage

``` r
indirect_n2o_ef
```

## Format

A tibble with `edition` (`"ipcc_2019"` or `"ipcc_2006"`), `parameter`,
`value`, `description`.

## Source

IPCC 2019 Refinement, Vol 4, Ch 11, Table 11.3 (Updated), p. 11.26; IPCC
2006 Guidelines, Vol 4, Ch 11, Table 11.3, p. 11.24; Vol 4, Ch 10, Table
10.22 of either edition for FracGasMS.

## Examples

``` r
indirect_n2o_ef
#> # A tibble: 8 × 4
#>   edition   parameter           value description                               
#>   <chr>     <chr>               <dbl> <chr>                                     
#> 1 ipcc_2019 ef4_volatilization 0.01   EF4: N2O-N per kg NH3-N + NOx-N volatiliz…
#> 2 ipcc_2019 ef5_leaching       0.011  EF5: N2O-N per kg N leached/runoff        
#> 3 ipcc_2019 frac_gasms         0.2    FracGasMS: fraction N lost as NH3+NOx fro…
#> 4 ipcc_2019 frac_leach         0.24   FracLeach: fraction N lost via leaching/r…
#> 5 ipcc_2006 ef4_volatilization 0.01   EF4: N2O-N per kg NH3-N + NOx-N volatiliz…
#> 6 ipcc_2006 ef5_leaching       0.0075 EF5: N2O-N per kg N leached/runoff        
#> 7 ipcc_2006 frac_gasms         0.2    FracGasMS: fraction N lost as NH3+NOx fro…
#> 8 ipcc_2006 frac_leach         0.3    FracLeach: fraction N lost via leaching/r…
```
