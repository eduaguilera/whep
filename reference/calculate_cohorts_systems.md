# Calculate cohort and production system distribution.

Distributes national herd totals across GLEAM-defined cohorts and
production systems using `gleam_livestock_categories` and regional
weight data.

GLEAM supplies only the taxonomy here: which systems and cohorts each
species has. It supplies no herd shares. `gleam_livestock_categories`
has no share column, and the herd-parameter tables of the GLEAM 2.0 and
3.0 supplements give demographic rates and live weights, not a
dairy/meat or layer/broiler split of the herd (whep#1194).

Where FAOSTAT already reports the herd split into the items
[`build_primary_production()`](https://eduaguilera.github.io/whep/reference/build_primary_production.md)
carries (`"Cattle, dairy"` / `"Cattle, non-dairy"`, `"Pigs"` / `"Hogs"`
for market / breeding swine, and `"Chickens, layers"` /
`"Chickens, broilers"`), the whole herd goes to the system its item
names and no share is applied. Every other herd (buffalo, sheep, goats,
ducks, turkeys, geese, and any aggregate cattle, swine or poultry label)
is split by WHEP's default shares, which are **assumed, unverified**
placeholders with no source. The `method_system_share` column says which
of the two applied to each row.

## Usage

``` r
calculate_cohorts_systems(data, system_shares = NULL)
```

## Arguments

- data:

  Dataframe with `species`, `heads`, and optionally `iso3` or `region`.

- system_shares:

  Optional dataframe with `species_gen`, `system`, `system_share`
  columns. If `NULL`, a herd whose commodity names a production system
  goes wholly to it, and every other herd uses WHEP's assumed,
  unverified default shares. Supplying this overrides both, so the
  supplied shares are used verbatim.

## Value

Dataframe expanded to cohort level with `cohort`, `system`,
`cohort_heads`, and `cohort_fraction` columns, plus
`method_system_share`: `"reported"` when the commodity itself names the
system, `"assumed"` when WHEP's unsourced default split was applied, or
`"supplied"` when `system_shares` was given. A `milk_yield_kg_day`
column (per head of the input row) is moved onto the milked cohort, the
`"Dairy"` system's `"Adult Female"`, at the yield that keeps the herd's
milk unchanged; every other cohort gets 0. `method_milk_yield` says
which: `"milked_cohort"`, `"not_milked_cohort"`, or `"whole_herd"` for a
species with no cohorts. A herd with milk but no milked cohort aborts
(class `whep_milk_without_milked_cohort`).

## Examples

``` r
tibble::tibble(
  species = "Cattle", heads = 10000,
  iso3 = "DEU"
) |>
  calculate_cohorts_systems()
#> # A tibble: 11 × 9
#>    species heads iso3  species_gen system method_system_share cohort            
#>    <chr>   <dbl> <chr> <chr>       <chr>  <chr>               <chr>             
#>  1 Cattle  10000 DEU   Cattle      Dairy  assumed             Adult Female      
#>  2 Cattle  10000 DEU   Cattle      Dairy  assumed             Adult Male        
#>  3 Cattle  10000 DEU   Cattle      Dairy  assumed             Replacement Female
#>  4 Cattle  10000 DEU   Cattle      Dairy  assumed             Replacement Male  
#>  5 Cattle  10000 DEU   Cattle      Dairy  assumed             Surplus Female    
#>  6 Cattle  10000 DEU   Cattle      Dairy  assumed             Surplus Male      
#>  7 Cattle  10000 DEU   Cattle      Beef   assumed             Adult Female      
#>  8 Cattle  10000 DEU   Cattle      Beef   assumed             Adult Male        
#>  9 Cattle  10000 DEU   Cattle      Beef   assumed             Replacement Female
#> 10 Cattle  10000 DEU   Cattle      Beef   assumed             Replacement Male  
#> 11 Cattle  10000 DEU   Cattle      Beef   assumed             Fattening         
#> # ℹ 2 more variables: cohort_fraction <dbl>, cohort_heads <dbl>
```
