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

IPCC 2019 Refinement, Vol 4, Ch 10, Eq 10.3-10.16, with
`cfi_mj_day_kg075` from Table 10.4 (Updated) and `ca_pasture` from Table
10.5 (Updated). Sheep and goats take their own rows in both tables and
must not be conflated: Cfi is 0.217 for sheep older than one year and
0.315 for goats, and Ca is 0.0107 for sheep grazing flat pasture and
0.019 for lowland goats. Neither goat value exists in the 2006
Guidelines, whose Table 10.4 has no goat row at all. `ca_pasture` takes
the lowland/flat-pasture value as the global default; the published hill
and mountain goat coefficient (0.024) is not stored and must be supplied
per row by the caller.

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
#> 6 Goats    All                    0.315     0.019           0 0.077     0
#> # ℹ 1 more variable: energy_content_gain_mj_kg <dbl>
```
