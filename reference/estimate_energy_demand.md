# Estimate energy demand (Gross Energy) - Tier 2

Calculate gross energy (GE) intake per IPCC 2019 Tier 2 equations (Vol
4, Ch 10). Estimates net energy components for maintenance, activity,
lactation, work, pregnancy, growth, and wool, then derives total gross
energy using the REM/REG ratio approach from IPCC Eq 10.16.

All coefficients come from internal package data.

## Usage

``` r
estimate_energy_demand(
  data,
  method = "ipcc2019",
  lactation_method = c("milk_composition", "ipcc2019")
)
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

- lactation_method:

  How net energy for lactation (NEl) is derived from milk yield. One of:

  - `"milk_composition"` (default): the NRC (2001) milk-energy equation,
    `NEl = Milk * (0.389 * Fat + 0.229 * Protein + 0.165 * Lactose)`
    (MJ/kg: the published Mcal/kg coefficients 0.0929, 0.0547 and 0.0395
    times 4.184), for rows with a positive protein and lactose content.
    Rows without that composition use the `"ipcc2019"` equations.

  - `"ipcc2019"`: IPCC 2019 Refinement Vol 4 Ch 10. Eq 10.8,
    `NEl = Milk * (1.47 + 0.40 * Fat)`, for cattle, buffalo and other
    species; Eq 10.9, `NEl = Milk * EVmilk`, for sheep and goats with
    the default `EVmilk` of 4.6 MJ/kg for sheep (7% fat; AFRC
    1993, 1995) and 3 MJ/kg for goats (3.8% fat; AFRC 1998). The
    defaults ignore `fat_percent`.

  The equation used for each row is recorded in `method_lactation`
  (`"nrc2001_milk_composition"`, `"ipcc2019_eq10_8"`,
  `"ipcc2019_eq10_9_default_ev"`, or `"none"` when there is no milk).

## Value

Dataframe with added `gross_energy` (MJ/day), intermediate net energy
components, and `method_energy` and `method_lactation` tracking columns.

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
