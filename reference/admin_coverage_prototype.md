# Zero-row prototype of the run's admin-coverage report

The schema of `admin_coverage.csv`, which
[`run_spatialize()`](https://eduaguilera.github.io/whep/reference/run_spatialize.md)
writes beside `run_metadata.yaml` whenever a run requests a depth. One
row per container x item x year, saying which admin source constrained
it and at what grain. The populated table is produced by the T25
resolver; until a run is wired to one, the file is written with this
header and no rows, so a reader can tell "no coverage was granted" from
"the file was never written".

It IS the resolver's own coverage schema, returned rather than restated:
this function had a second, hand-written list of names (`source`,
`tier`, `grain`, and a character `not_shipped`) where
[`resolve_admin_shares()`](https://eduaguilera.github.io/whep/reference/resolve_admin_shares.md)
emits `resolved_source`, `resolved_tier`, `resolved_grain` and a
logical, so handing the resolver's table to the writer aborted on three
missing columns, and the writer's subset would have dropped the
resolution audit trail (`resolved_indicator`, `resolved_nuts_version`,
`resolution_rule`) from the file.

## Usage

``` r
admin_coverage_prototype()
```

## Value

A zero-row `tibble` with the report's columns.

## Examples

``` r
admin_coverage_prototype()
#> # A tibble: 0 × 15
#> # ℹ 15 variables: area_code <int>, level <int>, item_prod_code <int>,
#> #   species_group <chr>, year <int>, resolved_source <chr>,
#> #   resolved_tier <int>, resolved_grain <chr>, resolved_indicator <chr>,
#> #   resolved_nuts_version <chr>, resolution_rule <chr>,
#> #   n_units_reporting <int>, reporting_units <chr>, not_shipped <lgl>,
#> #   coverage_change <lgl>
```
