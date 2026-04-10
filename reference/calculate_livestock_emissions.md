# Calculate all livestock emissions.

Main dispatcher that runs the full IPCC 2019 livestock emissions
pipeline: energy demand (Tier 2), enteric CH4, manure CH4, and manure
N2O.

Selects tier automatically: Tier 2 when cohort-level data (weight, diet)
are available; Tier 1 otherwise.

## Usage

``` r
calculate_livestock_emissions(data, tier = NULL)
```

## Arguments

- data:

  Dataframe with at minimum `species` and `heads`. For Tier 2, also
  needs `cohort`, `weight` (or `iso3`), `diet_quality`, and production
  columns.

- tier:

  Integer 1 or 2. If `NULL` (default), auto-selects based on data
  completeness.

## Value

Dataframe with all emission columns, method tracking, and original data
columns preserved.

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
  dplyr::select(species, cohort, heads,
    enteric_ch4_tier2, manure_ch4_tier2,
    manure_n2o_total)
#> # A tibble: 1 × 6
#>   species      cohort  heads enteric_ch4_tier2 manure_ch4_tier2 manure_n2o_total
#>   <chr>        <chr>   <dbl>             <dbl>            <dbl>            <dbl>
#> 1 Dairy Cattle Adult …  1000           126919.           17626.            2701.
```
