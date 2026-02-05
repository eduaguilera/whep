# System NUE ———————————————————–

Calculates the NUE for Spain at the provincial level. The system NUE is
defined as the percentage of total nitrogen production (`total_prod`)
relative to the sum of all nitrogen inputs (`inputs`) into the soil
system.

## Usage

``` r
calculate_system_nue(n_soil_inputs = create_n_soil_inputs())
```

## Arguments

- n_soil_inputs:

  A tibble of nitrogen soil input (deposition, fixation, synthetic,
  manure, urban)

## Value

A tibble with the following columns:

- `Year`: Year

- `Province_name`: Spanish province

- `total_prod`: Total nitrogen production (Mg)

- `inputs`: Total nitrogen inputs (Mg)

- `nue_system`: System-level Nitrogen Use Efficiency (%)
