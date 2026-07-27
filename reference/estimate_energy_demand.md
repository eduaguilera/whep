# Estimate energy demand (Gross Energy) - Tier 2

Calculate gross energy (GE) intake per IPCC 2019 Tier 2 equations (Vol
4, Ch 10). Estimates net energy components for maintenance, activity,
lactation, work, pregnancy, growth, and wool, then derives total gross
energy using the REM/REG ratio approach from IPCC Eq 10.16.

All coefficients come from internal package data.

## Usage

``` r
estimate_energy_demand(data, method = "ipcc2019")
```

## Arguments

- data:

  A dataframe with columns `species`, `cohort`, `heads`, and optionally
  `iso3`. Optional production columns: `weight`, `milk_yield_kg_day`,
  `fat_percent`, `weight_gain_kg_day`, `work_hours_day`, `work_coef`,
  `cfi`, `pregnant_fraction`, `temperature_c`, `diet_quality`,
  `grazing_distance_km`, `system`. `work_coef` overrides the joined IPCC
  work coefficient (`cw`, 0 by default for every species) for rows that
  need draught/work energy (IPCC Eq 10.11) without changing the global
  default. `cfi` overrides the joined maintenance coefficient
  (`cfi_mj_day_kg075`, IPCC Eq 10.3) for rows whose herd-average
  maintenance requirement is known from a national inventory (e.g. a
  housed dairy herd calibrated to a Zootecnicas/NIR Cfi), without
  changing the global default.

- method:

  Method for calculation (default `"ipcc2019"`).

## Value

Dataframe with added `gross_energy` (MJ/day), intermediate net energy
components, and `method_energy` tracking column.

## Examples

``` r
tibble::tibble(
  species = "Dairy Cattle", cohort = "Adult Female",
  heads = 100, weight = 600, diet_quality = "High",
  milk_yield_kg_day = 20
) |>
  estimate_energy_demand() |>
  dplyr::select(species, cohort, heads, ne_maintenance,
    ne_activity, ne_lactation, ne_growth, gross_energy)
#> # A tibble: 1 × 8
#>   species      cohort    heads ne_maintenance ne_activity ne_lactation ne_growth
#>   <chr>        <chr>     <dbl>          <dbl>       <dbl>        <dbl>     <dbl>
#> 1 Dairy Cattle Adult Fe…   100           46.8        7.96         61.8         0
#> # ℹ 1 more variable: gross_energy <dbl>
```
