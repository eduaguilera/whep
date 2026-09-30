# Read the assembled subnational admin-shares pin

Read the single `admin-shares` pin – the tier-1, tier-2 and tier-3
administrative statistics assembled onto the one contract
[`admin_shares_schema()`](https://eduaguilera.github.io/whep/reference/admin_shares_schema.md)
declares – together with the report of which families are **not** in it
and why.

Polity resolution is deliberately not done here. The pin is keyed on
source-native identifiers and carries `level_polity_code == NA` on every
row; a caller that needs polities passes the rows through
[`resolve_admin_units()`](https://eduaguilera.github.io/whep/reference/resolve_admin_units.md),
which redoes the resolution against the current
[polities](https://eduaguilera.github.io/whep/reference/polities.md)
snapshot on every load. That is what lets this artifact age without
going stale.

That call needs one rename first. The contract names the identifier
column `source_native_id`, where
[`resolve_admin_units()`](https://eduaguilera.github.io/whep/reference/resolve_admin_units.md)
takes the readers' `source_native_unit_id`, so the composition is
`dplyr::rename(shares, source_native_unit_id = source_native_id)` and
then
[`resolve_admin_units()`](https://eduaguilera.github.io/whep/reference/resolve_admin_units.md).
Handing the rows over unrenamed aborts on the missing column.

## Usage

``` r
read_admin_shares(example = FALSE)
```

## Arguments

- example:

  If `TRUE`, return a small fixture instead of reading the pin. Defaults
  to `FALSE`.

## Value

A named list of three elements:

- `shares`: the pin's rows, in
  [`admin_shares_prototype()`](https://eduaguilera.github.io/whep/reference/admin_shares_prototype.md)'s
  shape, or the zero-row prototype when the pin is not shipped.

- `excluded`: a tibble of `source`, `reason` and `detail`, as above.

- `not_shipped`: `"admin-shares"` when the pin's alias is not registered
  in
  [`whep_inputs`](https://eduaguilera.github.io/whep/reference/whep_inputs.md)
  or holds no rows, `character(0)` otherwise.

## What the excluded report can and cannot say

`excluded` names every tier-2/3 family of
[`read_admin_family()`](https://eduaguilera.github.io/whep/reference/read_admin_family.md)
that contributed no row to this pin, with a reason from a closed
vocabulary and a `detail` saying why the family is absent – not what
permission it ships under, which is the manifest's own job:

- `"no_consent_manifest_row"`: the family is absent from
  `inst/extdata/admin_stats_pins_manifest.csv`, so its publication
  consent is not recorded and the assembly refused to ship it.

- `"no_rows_in_pin"`: its consent is recorded, but none of its rows
  survived onto the contract.

- `"pin_not_read"`: no pin was read at all – the alias is not registered
  in
  [`whep_inputs`](https://eduaguilera.github.io/whep/reference/whep_inputs.md),
  or the board returned no rows – so nothing about this family was
  measured. Every family carries this reason together, and `not_shipped`
  says the same thing.

A read cannot see which rows were dropped or why, so for any other
family `detail` points at the assembly's per-source counts. Those counts
are a **second, build-time report**, written by
`inst/scripts/prepare_admin_shares_pin.R` and not carried into the pin.
It shares this vocabulary where the two can mean the same thing
(`"no_consent_manifest_row"`, `"no_rows_in_pin"`) and adds one reason a
read cannot observe: `"not_available"`, a consented family that was
neither registered on the board nor staged locally when the pin was
assembled. That is a fact about an assembly this read never saw, so it
stays build-time only. What a read can always tell apart is whether it
read a pin at all, which is what `"pin_not_read"` says – the report used
to claim five families had been read and filtered when the board had not
been touched.

## Examples

``` r
read_admin_shares(example = TRUE)
#> $shares
#> # A tibble: 5 × 22
#>   area_code level_polity_code level item_prod_code species_group indicator_used
#>       <int> <chr>             <int>          <int> <chr>         <chr>         
#> 1       110 NA                    1             27 NA            area_harvested
#> 2       110 NA                    1             27 NA            area_harvested
#> 3        19 NA                    1            661 NA            area_harvested
#> 4        19 NA                    1            661 NA            area_harvested
#> 5        68 NA                    1             NA cattle_dairy  head_count    
#> # ℹ 16 more variables: year <int>, value <dbl>, share <dbl>, source <chr>,
#> #   tier <int>, grain <chr>, concept_break <lgl>, nuts_version <chr>,
#> #   source_native_id <chr>, source_native_name <chr>, source_id <chr>,
#> #   source_version <chr>, recorded_at <chr>, treatment_year <chr>,
#> #   treatment_value <chr>, value_flag <chr>
#> 
#> $excluded
#> # A tibble: 2 × 3
#>   source                      reason         detail                             
#>   <chr>                       <chr>          <chr>                              
#> 1 admin-stats-spain-provinces no_rows_in_pin consent is recorded and no row of …
#> 2 admin-stats-australia       no_rows_in_pin consent is recorded and no row of …
#> 
#> $not_shipped
#> character(0)
#> 
```
