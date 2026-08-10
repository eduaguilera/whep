# Add WHEP polity codes to a table

Adds periodized `polity_code` information from
[polity_area_crosswalk](https://eduaguilera.github.io/whep/reference/polity_area_crosswalk.md)
to a table with FAOSTAT/FABIO `area_code` values. If a `year` column is
present, the mapping is year-aware; otherwise the current/default
mapping is used.

When no mapped period covers a row's year, another period of the same
area is used as a stand-in and `mapping_status` reports `"out_of_span"`
rather than the crosswalk's `"matched"`/`"manual"`. Such a row is
attributed to a polity that did not exist in that year, so treat it as a
coverage gap: either the area needs the missing period added to the
crosswalk, or the reporting area outlived (or predates) every polity
mapped to it.

## Usage

``` r
add_polity_code(
  table,
  code_column = "area_code",
  year_column = "year",
  polity_code_column = "polity_code",
  backcast_anchor = 1961L
)
```

## Arguments

- table:

  A data frame.

- code_column:

  Name of the column containing numeric area codes.

- year_column:

  Name of the column containing years. Set to `NULL` to force
  current/default mapping.

- polity_code_column:

  Name of the output polity-code column.

- backcast_anchor:

  First year of reported (non-back-cast) FAOSTAT data, default `1961`.
  Years before it are matched to the polity active in the anchor year,
  because WHEP's pre-anchor series are back-cast onto the anchor-year
  territory rather than reported under their data-year borders. Set to
  `-Inf` to disable and match strictly by data year.

## Value

A tibble with added polity metadata columns.

## Which stand-in is picked

A period that has **not started yet** is preferred over one that has
already **ended**, and only then is the nearest in years taken. Ranking
by distance alone split one reporting area's series between two entities
at whichever year the arithmetic flipped: FAOSTAT area 178 Eritrea read
`ERI-1889-1952` (the Italian colonial administration) up to 1972 and
`ERI-1993-2025` from 1973, and area 273 Montenegro split at 1961 between
`MNE-1913-1918` and `MNE-2006-2025` on a one-year margin. Preferring the
not-yet-started period keeps each area on one entity and agrees with the
back-cast anchor, whose purpose is to avoid resolving back-cast rows
onto a larger historical-extent period. Set
`options(whep.polity_stand_in = "nearest")` to restore ranking by
distance alone; it changes 235 of the crosswalk's 46,640 `(area, year)`
pairs over 1850-2025, all of them areas 178 and 273 (whep#705).

## See also

[`polity_coverage_gaps()`](https://eduaguilera.github.io/whep/reference/polity_coverage_gaps.md),
which reports the stand-in rows of an already-built table, whose
published columns no longer carry `mapping_status`.

## Examples

``` r
# The same area code resolves to different polities in different years:
# area 16 reports as East Pakistan before 1971 and as Bangladesh after it.
tibble::tibble(area_code = c(16L, 16L), year = c(1965L, 2000L)) |>
  add_polity_code() |>
  dplyr::select(area_code, year, polity_code, polity_name, mapping_status)
#> # A tibble: 2 × 5
#>   area_code  year polity_code   polity_name               mapping_status
#>       <int> <int> <chr>         <chr>                     <chr>         
#> 1        16  1965 BGD-1947-1971 East Pakistan (1947-1971) matched       
#> 2        16  2000 BGD-1971-2025 Bangladesh                matched       

# Without a year column the current/default mapping is used.
add_polity_code(tibble::tibble(area_code = 231L), year_column = NULL) |>
  dplyr::select(area_code, polity_code, polity_name)
#> # A tibble: 1 × 3
#>   area_code polity_code   polity_name             
#>       <int> <chr>         <chr>                   
#> 1       231 USA-1959-2025 United States of America
```
