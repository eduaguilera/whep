# Prepare production data for livestock emission calculations.

Bridge between
[`build_primary_production()`](https://eduaguilera.github.io/whep/reference/build_primary_production.md)
output and the livestock emission functions. Maps species codes to
names, converts area codes to ISO3, extracts milk and meat yields, and
optionally expands herds into GLEAM cohorts.

Any extra columns present in the input (e.g., `weight`, `diet_quality`,
`fat_percent`) are preserved and flow through to the emission functions
automatically.

## Usage

``` r
prepare_livestock_emissions(data, expand_cohorts = FALSE, system_shares = NULL)
```

## Arguments

- data:

  A tibble from
  [`build_primary_production()`](https://eduaguilera.github.io/whep/reference/build_primary_production.md)
  with columns `item_cbs_code`, `unit`, `value`, and optionally `year`,
  `area_code`, `live_anim_code`.

- expand_cohorts:

  Logical. If `TRUE`, distributes herds across GLEAM cohorts and
  production systems via
  [`calculate_cohorts_systems()`](https://eduaguilera.github.io/whep/reference/calculate_cohorts_systems.md).
  Default `FALSE`.

- system_shares:

  Optional dataframe with custom system shares. Passed to
  [`calculate_cohorts_systems()`](https://eduaguilera.github.io/whep/reference/calculate_cohorts_systems.md).

## Value

A tibble with columns `species`, `heads`, `iso3` (if `area_code`
present), and optionally `milk_yield_kg_day`, `meat_yield_t_head`,
cohort columns, plus all extra columns from the input.

## Examples

``` r
tibble::tibble(
  item_cbs_code = 961,
  unit = "heads",
  value = 5000,
  area_code = 79L
) |>
  prepare_livestock_emissions()
#> # A tibble: 1 × 6
#>   item_cbs_code unit  heads area_code species           iso3 
#>           <dbl> <chr> <dbl>     <dbl> <chr>             <chr>
#> 1           961 heads  5000        79 Cattle, non-dairy DEU  
```
