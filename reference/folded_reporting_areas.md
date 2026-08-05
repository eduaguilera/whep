# List the reporting areas whose data is folded into another area code

A FAOSTAT reporting area is *folded* when
[polity_area_crosswalk](https://eduaguilera.github.io/whep/reference/polity_area_crosswalk.md)
gives it a `polity_area_code` that is not its own `area_code`. Every row
the area reports is then summed into that other bucket, so the area
disappears from WHEP output without a single row being dropped or left
unresolved. This lists those areas, because the coverage reports cannot:
a fold resolves perfectly well, only to a territory that did not report
the data.

Two kinds exist, and they are not equally defensible:

- `"fabio_rest_of_world"`: FABIO collapses the area into its single
  Rest-of-World row (`polity_area_code` 999, `ROW-1850-2023`). Most such
  areas report nothing, but several report substantial data of their
  own: Syria, Eswatini, New Caledonia, North Macedonia, Reunion,
  Guadeloupe, Palestine, the Faroe Islands. Their observed values are
  attributed to Rest of World.

- `"successor_state"`: the area is summed into the bucket of the state
  that succeeded it, which is a deliberate territorial identity rather
  than a loss: FAOSTAT area 62 "Ethiopia PDR" into 238 Ethiopia, and
  areas 276 Sudan and 277 South Sudan into 206 Sudan (former).

Whether to lift the Rest-of-World fold is an open decision recorded in
issue 419; this function only makes the current state visible and
changes nothing. A build also warns, naming the areas and the row counts
it actually folded.

## Usage

``` r
folded_reporting_areas(crosswalk = NULL)
```

## Arguments

- crosswalk:

  Crosswalk to inspect. Defaults to
  [polity_area_crosswalk](https://eduaguilera.github.io/whep/reference/polity_area_crosswalk.md).

## Value

A tibble with one row per folded reporting area, ordered by `area_code`:

- `area_code`: The reporting area whose data is folded away.

- `area_name`, `area_iso3c`: Its name and ISO3-like code.

- `polity_area_code`: The bucket its rows are summed into.

- `polity_code`, `polity_name`: The polity the fold attributes them to.

- `fold_kind`: `"fabio_rest_of_world"` or `"successor_state"`.

## Measuring the alternative

`options(whep.unfold_rest_of_world = TRUE)` promotes every Rest-of-World
member to its own `polity_area_code` for the whole pipeline, which is
the experiment the decision needs. It is **off by default and not a
production mode**: published WHEP values assume the fold, so every read
of the crosswalk warns while it is set. The `"successor_state"` folds
are never lifted by it, since those are territorial identities rather
than a FABIO convention.

Measured with it on a full-range
[`get_wide_cbs()`](https://eduaguilera.github.io/whep/reference/get_wide_cbs.md)
(1850-2023, all 61 members promoted), global totals move by at most 1.2%
(`stock_addition`) and by less than 0.1% for `feed`, `production` and
`processing`. An earlier measurement recorded in issue 419 reported up
to 13.7x; that comparison predates the `dcast()` duplicate-key fix in
`.select_best_source()` (issue 425) and does not reproduce.

## Examples

``` r
folded <- folded_reporting_areas()
nrow(folded)
#> [1] 72
head(folded[folded$fold_kind == "successor_state", ], 4)
#> # A tibble: 4 × 7
#>   area_code area_name    area_iso3c polity_area_code polity_code   polity_name  
#>       <int> <chr>        <chr>                 <int> <chr>         <chr>        
#> 1        62 Ethiopia PDR ETH                     238 ETH-1800-1889 Ethiopia (to…
#> 2        62 Ethiopia PDR ETH                     238 ETH-1889-1897 Ethiopia (18…
#> 3        62 Ethiopia PDR ETH                     238 ETH-1897-1902 Ethiopia (18…
#> 4        62 Ethiopia PDR ETH                     238 ETH-1902-1907 Ethiopia (19…
#> # ℹ 1 more variable: fold_kind <chr>
```
