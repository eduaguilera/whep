# Calculate cohort and production system distribution.

Distributes national herd totals across GLEAM-defined cohorts and
production systems using `gleam_livestock_categories` and regional
weight data.

## Usage

``` r
calculate_cohorts_systems(data, system_shares = NULL)
```

## Arguments

- data:

  Dataframe with `species`, `heads`, and optionally `iso3` or `region`.

- system_shares:

  Optional dataframe with `species`, `system`, `share` columns. If
  `NULL`, uses GLEAM defaults.

## Value

Dataframe expanded to cohort level with `cohort`, `system`,
`cohort_heads`, and `cohort_fraction` columns.

## Examples

``` r
tibble::tibble(
  species = "Cattle", heads = 10000,
  iso3 = "DEU"
) |>
  calculate_cohorts_systems()
#> # A tibble: 11 × 8
#>    species heads iso3  species_gen system cohort    cohort_fraction cohort_heads
#>    <chr>   <dbl> <chr> <chr>       <chr>  <chr>               <dbl>        <dbl>
#>  1 Cattle  10000 DEU   Cattle      Dairy  Adult Fe…            0.05          500
#>  2 Cattle  10000 DEU   Cattle      Dairy  Adult Ma…            0.05          500
#>  3 Cattle  10000 DEU   Cattle      Dairy  Replacem…            0.05          500
#>  4 Cattle  10000 DEU   Cattle      Dairy  Replacem…            0.05          500
#>  5 Cattle  10000 DEU   Cattle      Dairy  Surplus …            0.05          500
#>  6 Cattle  10000 DEU   Cattle      Dairy  Surplus …            0.05          500
#>  7 Cattle  10000 DEU   Cattle      Beef   Adult Fe…            0.14         1400
#>  8 Cattle  10000 DEU   Cattle      Beef   Adult Ma…            0.14         1400
#>  9 Cattle  10000 DEU   Cattle      Beef   Replacem…            0.14         1400
#> 10 Cattle  10000 DEU   Cattle      Beef   Replacem…            0.14         1400
#> 11 Cattle  10000 DEU   Cattle      Beef   Fattening            0.14         1400
```
