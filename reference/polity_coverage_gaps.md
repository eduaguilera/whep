# Find rows attributed to a polity not live in the row's year

[`add_polity_code()`](https://eduaguilera.github.io/whep/reference/add_polity_code.md)
reports these rows in `mapping_status`, but WHEP's published outputs do
not carry that column: `reporting_polity_code` and
`reporting_polity_name` say which polity a row was attributed to, and
nothing says the polity did not exist in that row's year. This answers
that question for a table that has already been built, so a consumer
joining on `reporting_polity_code` can tell a real period hit from the
two kinds of substitute without re-deriving the crosswalk.

Neither kind is an error and no row is dropped. It means the polygon,
population and period of the returned polity describe a different year
than the value does, so `gap_kind` names which kind a row is:

- `"backcast_anchor"`: the row is before `backcast_anchor` and its
  polity was resolved at the anchor year, which is WHEP's own back-cast
  convention – pre-1961 series are reconstructions on the anchor year's
  territory, so a Soviet republic's 1900 land is booked to the republic
  that reports it today. The polity was matched at the anchor and simply
  had not begun by the row's own year.

- `"polity_not_started"`: no mapped period covered even the anchored
  year and the stand-in taken begins after it.

- `"polity_ended"`: the polity had ended, so the value covers a
  territory that entity no longer describes. This is the harder case:
  FAOSTAT area 51 "Czechoslovakia" resolves to `F51-1947-1993` whatever
  year it is asked for, because nothing later is mapped to that area.
  Bucket 206 used to be the headline instance – areas 276 Sudan and 277
  South Sudan fold into it and its label `SUD-1956-2011` had ended at
  the secession (whep#414) – and is no longer one: upstream minted
  `F206-2011-2025`, live over exactly the years the bucket sums both,
  and whep#860 wired it on.

`gap_kind` is not derivable from the returned columns, which is why it
is returned rather than left to the caller. `"backcast_anchor"` is not
visible in the years at all – the resolver matched a real period, at the
anchor – and the direction of the other two is read at the year the
resolver actually matched on, which the back-cast anchor floors at
`backcast_anchor`, so a pre-anchor row is classified as the anchor year
it was resolved as rather than as the year it carries.

Measured on a real full-range
[`get_primary_production()`](https://eduaguilera.github.io/whep/reference/get_primary_production.md):
2,301 `(area, year)` pairs / 7,247 rows are stand-ins, and the back-cast
class adds 9,544 pairs the floor previously hid from this function
entirely (whep#763).

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
  from `table`, to use the current/default mapping, which has no gaps by
  construction.

- backcast_anchor:

  First year of reported (non-back-cast) FAOSTAT data; passed to the
  same resolution
  [`add_polity_code()`](https://eduaguilera.github.io/whep/reference/add_polity_code.md)
  documents.

## Value

A tibble with one row per reported `(area_code, year)`, ordered by area
code and year, carrying `area_code`, `year`, `polity_code`,
`polity_name`, `polity_start_year`, `polity_end_year`, `gap_kind`
(`"backcast_anchor"`, `"polity_not_started"` or `"polity_ended"`) and
`n_rows`, the number of rows of `table` that pair carries. Zero rows
means every row of `table` landed inside its polity's period, which is
the intended state.

## See also

[`add_polity_code()`](https://eduaguilera.github.io/whep/reference/add_polity_code.md)
for the resolution itself, and
[`polity_bucket_coverage()`](https://eduaguilera.github.io/whep/reference/polity_bucket_coverage.md)
for the related question of which buckets sum more than one territory,
and whether their label covers the sum.

## Examples

``` r
# FAOSTAT area 51 "Czechoslovakia" is the live case: `F51-1947-1993` ended in
# 1993 and nothing later is mapped to the area, so a post-1993 row is a
# stand-in. Area 238's 1850 row is the back-cast case: `ETH-1952-1993` labels
# it because that is the polity live at the anchor, 102 years later.
polity_coverage_gaps(
  tibble::tibble(
    area_code = c(51L, 51L, 238L),
    year = c(1990L, 2015L, 1850L),
    value = 1
  )
)
#> # A tibble: 2 × 8
#>   area_code  year polity_code   polity_name    polity_start_year polity_end_year
#>       <int> <int> <chr>         <chr>                      <int>           <int>
#> 1        51  2015 F51-1947-1993 Czechoslovaki…              1947            1993
#> 2       238  1850 ETH-1952-1993 Ethiopia (195…              1952            1993
#> # ℹ 2 more variables: gap_kind <chr>, n_rows <int>
```
