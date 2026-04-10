# Grazing energy coefficients.

Walking energy cost for grazing animals (MJ/kg body weight/km).

## Usage

``` r
grazing_energy_coefs
```

## Format

A tibble with `parameter`, `value_mj_kg_km`, `source`.

## Source

NRC 2001 (0.00045 Mcal/kg/km converted to MJ).

## Examples

``` r
grazing_energy_coefs
#> # A tibble: 1 × 3
#>   parameter           value_mj_kg_km source                       
#>   <chr>                        <dbl> <chr>                        
#> 1 walking_energy_cost         0.0019 NRC 2001 (0.00045 Mcal/kg/km)
```
