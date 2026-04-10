# Calculate uncertainty bounds for livestock emissions.

Applies IPCC uncertainty ranges to emission estimates. Multipliers
sourced from `uncertainty_ranges` table (no hardcoded values).

## Usage

``` r
calculate_uncertainty_bounds(data)
```

## Arguments

- data:

  Dataframe with emission columns from
  [`calculate_livestock_emissions()`](https://eduaguilera.github.io/whep/reference/calculate_livestock_emissions.md).

## Value

Dataframe with added `_lower` and `_upper` columns for each emission
estimate.

## Examples

``` r
tibble::tibble(
  species = "Dairy Cattle",
  cohort = "Adult Female",
  heads = 1000,
  weight = 600,
  diet_quality = "High",
  milk_yield_kg_day = 20
) |>
  calculate_livestock_emissions() |>
  calculate_uncertainty_bounds() |>
  dplyr::select(species, cohort, heads,
    enteric_ch4_tier2, enteric_ch4_tier2_lower,
    enteric_ch4_tier2_upper, manure_ch4_tier2,
    manure_ch4_tier2_lower, manure_ch4_tier2_upper)
#> # A tibble: 1 × 9
#>   species      cohort       heads enteric_ch4_tier2 enteric_ch4_tier2_lower
#>   <chr>        <chr>        <dbl>             <dbl>                   <dbl>
#> 1 Dairy Cattle Adult Female  1000           126919.                 107881.
#> # ℹ 4 more variables: enteric_ch4_tier2_upper <dbl>, manure_ch4_tier2 <dbl>,
#> #   manure_ch4_tier2_lower <dbl>, manure_ch4_tier2_upper <dbl>
```
