# List the reporting areas whose data is folded into another area code

A FAOSTAT reporting area is *folded* when
[polity_area_crosswalk](https://eduaguilera.github.io/whep/reference/polity_area_crosswalk.md)
gives it a `polity_area_code` that is not its own `area_code`. Every row
the area reports is then summed into that other bucket, so the area
disappears from WHEP output without a single row being dropped or left
unresolved. This lists those areas, because the coverage reports cannot:
a fold resolves perfectly well, only to a territory that did not report
the data.

Three kinds exist, and they are not equally defensible:

- `"fabio_rest_of_world"`: FABIO collapses the area into its single
  Rest-of-World row (`polity_area_code` 999, `ROW-1850-2025`) because
  its own region list does not enumerate the area either. 57 areas, all
  flagged `cbs` `FALSE` in
  [regions_full](https://eduaguilera.github.io/whep/reference/regions_full.md).
  Several still report substantial data of their own – Reunion,
  Guadeloupe, Palestine, the Faroe Islands – which is attributed to Rest
  of World.

- `"cbs_reporter_folded"`: the area is flagged `cbs` `TRUE`, so
  [regions_full](https://eduaguilera.github.io/whep/reference/regions_full.md)
  says it has a commodity balance sheet of its own, and it is folded
  into 999 anyway. Four areas: 153 New Caledonia, 154 North Macedonia,
  209 Eswatini and 212 Syria, the last being the largest single
  contributor to the fold. **FABIO does not fold these**: its published
  region list enumerates all four as regions in their own right (see the
  section below), so this fold is WHEP's, not a FABIO convention, and
  the `"fabio"` label the other 57 carry does not apply.

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

- `fold_kind`: `"fabio_rest_of_world"`, `"cbs_reporter_folded"` or
  `"successor_state"`.

## What FABIO's own region list says

FABIO (Bruckner et al. 2019) publishes the region list it uses, and it
contains all four `"cbs_reporter_folded"` areas as regions of their own:

- `io_codes.csv` of the FABIO v1.1 release (Zenodo record 2577067, the
  file `inst/scripts/compare_fabio.R` already downloads) enumerates 192
  areas x 125 commodities. Areas 153, 154, 209 and 212 each have their
  own 125-row block, distinct from area 999 `RoW`.

- The FABIO source repository
  (<https://github.com/fineprint-global/fabio>) folds an area into Rest
  of World exactly when it is absent from `inst/regions_full.csv` with
  `current == TRUE`. All four carry `current` `TRUE` there, and the 192
  codes that file flags `cbs` `TRUE` are precisely the 192 areas of
  `io_codes.csv`.

So `fabio_code == 999` for these four is a statement WHEP makes, not one
FABIO makes. Correcting it in `regions_full` would move published
values, because `polity_area_code` is derived from `fabio_code`, so the
contradiction is left standing and reported here instead (issue 556).

## The Rest-of-World fold is no longer applied

WHEP models every reporting member of bucket 999 in its own right.
FABIO's 192-country layout is a methodology this package compares
against, not a constraint on which territories it represents, and the
choice of country set is WHEP's to make (issue 459).

That matters because the fold was never doing what its name suggests. Of
the 61 members, only about a third report anything at all; the rest
contribute no rows and folding them is arithmetically a no-op.
Everything the bucket actually carried came from the members that DO
file returns – Syria, Eswatini, North Macedonia, New Caledonia, the
Faroe Islands, Palestine, Greenland and the like – and folding them
discarded whose data it was. So promotion is self-limiting: an area with
no rows is unaffected either way.

Bucket 999 survives as a genuine residual for the territories that
report nothing. Measured on a full-range
[`get_wide_cbs()`](https://eduaguilera.github.io/whep/reference/get_wide_cbs.md)
(1850-2023), promotion takes the published area count from 195 to 216
and moves global totals by at most 0.99% (`stock_addition`), with every
other column inside 0.4%.

`options(whep.unfold_rest_of_world = "none")` restores the fold, which
is what reproducing a number published before this change requires.
Because that no longer matches the published series, every read of the
crosswalk warns while it is set. `"cbs_reporters"` re-folds all but the
four `"cbs_reporter_folded"` areas and warns for the same reason. The
`"successor_state"` folds are never lifted by any mode, since those are
territorial identities rather than a FABIO convention.

An earlier measurement recorded in issue 419 reported this change at up
to 13.7x on `feed`; that comparison predates the `dcast()` duplicate-key
fix in `.select_best_source()` (issue 425) and does not reproduce.

## References

Bruckner, M., Wood, R., Moran, D., Kuschnig, N., Wieland, H., Maus, V.,
Borner, J. (2019). FABIO - The Construction of the Food and Agriculture
Input-Output Model. Environmental Science & Technology 53(19),
11302-11312.
[doi:10.1021/acs.est.9b03554](https://doi.org/10.1021/acs.est.9b03554)

## Examples

``` r
folded <- folded_reporting_areas()
nrow(folded)
#> [1] 11
head(folded[folded$fold_kind == "successor_state", ], 4)
#> # A tibble: 4 × 7
#>   area_code area_name    area_iso3c polity_area_code polity_code   polity_name  
#>       <int> <chr>        <chr>                 <int> <chr>         <chr>        
#> 1        62 Ethiopia PDR ETH                     238 ETH-1800-1889 Ethiopia (to…
#> 2        62 Ethiopia PDR ETH                     238 ETH-1889-1897 Ethiopia (18…
#> 3        62 Ethiopia PDR ETH                     238 ETH-1897-1902 Ethiopia (18…
#> 4        62 Ethiopia PDR ETH                     238 ETH-1902-1907 Ethiopia (19…
#> # ℹ 1 more variable: fold_kind <chr>
folded[folded$fold_kind == "cbs_reporter_folded", ]
#> # A tibble: 0 × 7
#> # ℹ 7 variables: area_code <int>, area_name <chr>, area_iso3c <chr>,
#> #   polity_area_code <int>, polity_code <chr>, polity_name <chr>,
#> #   fold_kind <chr>
```
