# Tier 2 energy coefficients.

Coefficients for IPCC Tier 2 GE calculation including Cfi (maintenance),
Ca (activity), Cp (pregnancy), Cw (work), and energy content of weight
gain. Now includes `subcategory` column to differentiate dairy
(lactating) vs non-dairy cattle.

## Usage

``` r
ipcc_tier2_energy_coefs
```

## Format

A tibble with columns:

- category:

  Species (Cattle, Buffalo, Sheep, etc.).

- subcategory:

  Dairy, Non-Dairy, or All.

- cfi_mj_day_kg075:

  NEm coefficient (MJ/day/kg^0.75).

- ca_pasture:

  Activity coefficient for grazing.

- ca_feedlot:

  Activity coefficient for confined.

- cp:

  Pregnancy coefficient.

- cw:

  Work coefficient.

- energy_content_gain_mj_kg:

  Energy per kg gain.

## Source

IPCC 2019 Refinement, Vol 4, Ch 10, Eq 10.3-10.16; Tables 10.4-10.5.

## Examples

``` r
ipcc_tier2_energy_coefs
#> # A tibble: 6 × 8
#>   category subcategory cfi_mj_day_kg075 ca_pasture ca_feedlot    cp    cw
#>   <chr>    <chr>                  <dbl>      <dbl>      <dbl> <dbl> <dbl>
#> 1 Cattle   Dairy                  0.386     0.17            0 0.1       0
#> 2 Cattle   Non-Dairy              0.322     0.17            0 0.1       0
#> 3 Buffalo  Dairy                  0.386     0.17            0 0.1       0
#> 4 Buffalo  Non-Dairy              0.322     0.17            0 0.1       0
#> 5 Sheep    All                    0.217     0.0107          0 0.077     0
#> 6 Goats    All                    0.217     0.0107          0 0.077     0
#> # ℹ 1 more variable: energy_content_gain_mj_kg <dbl>
```
