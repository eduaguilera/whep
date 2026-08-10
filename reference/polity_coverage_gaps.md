# Find rows whose polity is a nearest-period stand-in

[`add_polity_code()`](https://eduaguilera.github.io/whep/reference/add_polity_code.md)
reports a nearest-period stand-in as `mapping_status == "out_of_span"`,
but WHEP's published outputs do not carry that column:
`reporting_polity_code` and `reporting_polity_name` say which polity a
row was attributed to, and nothing says the polity did not exist in that
row's year. This answers that question for a table that has already been
built, so a consumer joining on `reporting_polity_code` can tell a real
period hit from a stand-in without re-deriving the crosswalk.

A stand-in is not an error and the row is not dropped. It means either
that the area needs the missing period added to the crosswalk, or that
the reporting area outlived (or predates) every polity mapped to it, so
treat it as a coverage gap: the polygon, population and period of the
returned polity describe a different year than the value does.

The two directions are not the same defect, so `gap_kind` names which
one a row is:

- `"polity_not_started"`: the polity begins after the row's year. This
  is mostly WHEP's own back-cast convention rather than a hole –
  pre-1961 series are back-cast onto the anchor-year territory, so a
  Soviet republic's 1900 land is attributed to the republic that reports
  it today.

- `"polity_ended"`: the polity had ended by the row's year, so the value
  covers a territory that entity no longer describes. This is the harder
  case, and the one whep#414 is about: FAOSTAT areas 276 Sudan and 277
  South Sudan fold into bucket 206, whose label `SUD-1956-2011` ended at
  the secession, and no live polity means "Sudan and South Sudan".

`gap_kind` is not derivable from the returned columns, which is why it
is returned rather than left to the caller. The comparison is against
the year the resolver actually matched on, which the back-cast anchor
floors at `backcast_anchor`, so a pre-anchor row is classified as the
anchor year it was resolved as rather than as the year it carries.

The resolution here is the same one the builds use, including the
back-cast anchor, so it reports what the table actually got rather than
a second reading of the crosswalk. The area column may hold either a
FAOSTAT `area_code` or the `polity_area_code` bucket that published
outputs are keyed by; both resolve through the same lookup.

## Usage

``` r
polity_coverage_gaps(
  table,
  code_column = "area_code",
  year_column = "year",
  backcast_anchor = 1961L
)
```

## Arguments

- table:

  A data frame carrying an area-code column, and a year column if the
  resolution is to be year-aware.

- code_column:

  Name of the column holding numeric area codes.

- year_column:

  Name of the column holding years. Set to `NULL`, or leave it absent
  from `table`, to use the current/default mapping, which has no
  stand-ins by construction.

- backcast_anchor:

  First year of reported (non-back-cast) FAOSTAT data; passed to the
  same resolution
  [`add_polity_code()`](https://eduaguilera.github.io/whep/reference/add_polity_code.md)
  documents.

## Value

A tibble with one row per `(area_code, year)` resolved by a stand-in,
ordered by area code and year, carrying `area_code`, `year`,
`polity_code`, `polity_name`, `polity_start_year`, `polity_end_year`,
`gap_kind` (`"polity_not_started"` or `"polity_ended"`) and `n_rows`,
the number of rows of `table` that pair carries. Zero rows means every
row of `table` landed inside its polity's period, which is the intended
state.

## See also

[`add_polity_code()`](https://eduaguilera.github.io/whep/reference/add_polity_code.md)
for the resolution itself, and
[`polity_bucket_coverage()`](https://eduaguilera.github.io/whep/reference/polity_bucket_coverage.md)
for the related question of which buckets sum more than one territory,
and whether their label covers the sum.

## Examples

``` r
# FAOSTAT area 206 "Sudan (former)" is the live case: it keeps reporting
# after `SUD-1956-2011` ends, so post-2011 rows are stand-ins.
polity_coverage_gaps(
  tibble::tibble(area_code = 206L, year = c(2005L, 2015L), value = 1)
)
#> # A tibble: 1 × 8
#>   area_code  year polity_code   polity_name    polity_start_year polity_end_year
#>       <int> <int> <chr>         <chr>                      <int>           <int>
#> 1       206  2015 SUD-1956-2011 Sudan (1956-2…              1956            2011
#> # ℹ 2 more variables: gap_kind <chr>, n_rows <int>
```
