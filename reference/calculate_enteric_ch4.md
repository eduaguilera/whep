# Calculate enteric methane emissions.

Wrapper that selects Tier 1 or 2 for enteric CH4 based on data
availability.

## Usage

``` r
calculate_enteric_ch4(data, tier = NULL)
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

- `method_enteric`: tracking label (`"IPCC_2019_Tier1"` or
  `"IPCC_2019_Tier2"`).

- Tier 1: `enteric_ef_kgch4` (emission factor), `enteric_ch4_tier1`
  (total kg CH4).

- Tier 2: `gross_energy`, `ym_factor`, `enteric_ch4_per_head` (kg
  CH4/head/yr), `enteric_ch4_tier2` (total kg CH4).

## Examples

``` r
tibble::tibble(
  species = "Cattle", heads = 1000, iso3 = "DEU"
) |>
  calculate_enteric_ch4(tier = 1)
#> # A tibble: 1 × 7
#>   species heads iso3  species_gen method_enteric  enteric_ef_kgch4
#>   <chr>   <dbl> <chr> <chr>       <chr>                      <dbl>
#> 1 Cattle   1000 DEU   Cattle      IPCC_2019_Tier1               47
#> # ℹ 1 more variable: enteric_ch4_tier1 <dbl>
```
