# Livestock physical constants.

Named list of physical constants used in livestock emission
calculations:

- `energy_content_ch4_mj_kg`: 55.65 MJ/kg CH4.

- `ch4_density_kg_m3`: 0.67 kg/m3.

- `vs_energy_content_mj_kg`: 18.45 MJ/kg DM.

- `n_to_n2o`: 44/28 (N to N2O molecular mass ratio).

- `days_in_year`: 365.

- `default_de_percent`: 65%.

- `default_ue_fraction`: 0.04 (urinary energy as fraction of GE).

- `ev_wool_mj_kg`: 24.0 MJ/kg clean wool.

## Usage

``` r
livestock_constants
```

## Format

A named list.

## Source

IPCC 2019 Refinement, Vol 4, Ch 10.

## Examples

``` r
str(livestock_constants)
#> List of 8
#>  $ energy_content_ch4_mj_kg: num 55.6
#>  $ ch4_density_kg_m3       : num 0.67
#>  $ vs_energy_content_mj_kg : num 18.4
#>  $ n_to_n2o                : num 1.57
#>  $ days_in_year            : num 365
#>  $ default_de_percent      : num 65
#>  $ default_ue_fraction     : num 0.04
#>  $ ev_wool_mj_kg           : num 24
```
