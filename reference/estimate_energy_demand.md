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
  `fat_percent`, `weight_gain_kg_day`, `work_hours_day`,
  `pregnant_fraction`, `temperature_c`, `diet_quality`,
  `grazing_distance_km`, `system`.

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
