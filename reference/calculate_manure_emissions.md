# Calculate manure emissions (CH4 + N2O).

Wrapper that selects Tier 1 or 2 for manure CH4 and computes N2O (Tier 2
only; skipped for Tier 1).

## Usage

``` r
calculate_manure_emissions(data, tier = NULL)
```

## Arguments

- data:

  Dataframe with `species`, `heads`. For Tier 2, also needs `cohort`,
  `weight`, and `diet_quality`. For Tier 1, `iso3` is used to select
  regional emission factors.

- tier:

  Integer 1 or 2. If `NULL` (default), auto-selects based on data
  completeness.

## Value

Dataframe with all input columns preserved, plus:

- `method_manure_ch4`: tracking label.

- Tier 1: `manure_ef_kgch4`, `manure_ch4_tier1`.

- Tier 2: `volatile_solids`, `methane_potential`, `weighted_mcf`,
  `manure_ch4_per_head`, `manure_ch4_tier2`.

- N2O (Tier 2 only): `method_manure_n2o`, `n_excretion`,
  `manure_n2o_direct`, `manure_n2o_indirect`, `manure_n2o_total`.

## Examples

``` r
tibble::tibble(
  species = "Cattle", heads = 1000, iso3 = "DEU"
) |>
  calculate_manure_emissions(tier = 1)
#> # A tibble: 1 × 7
#>   species heads iso3  species_gen method_manure_ch4 manure_ef_kgch4
#>   <chr>   <dbl> <chr> <chr>       <chr>                       <dbl>
#> 1 Cattle   1000 DEU   Cattle      IPCC_2019_Tier1                 2
#> # ℹ 1 more variable: manure_ch4_tier1 <dbl>
```
