# Add WHEP polity codes to a table

Adds periodized `polity_code` information from
[polity_area_crosswalk](https://eduaguilera.github.io/whep/reference/polity_area_crosswalk.md)
to a table with FAOSTAT/FABIO `area_code` values. If a `year` column is
present, the mapping is year-aware; otherwise the current/default
mapping is used.

When no mapped period covers a row's year, the nearest period of the
same area is used as a stand-in and `mapping_status` reports
`"out_of_span"` rather than the crosswalk's `"matched"`/`"manual"`. Such
a row is attributed to a polity that did not exist in that year, so
treat it as a coverage gap: either the area needs the missing period
added to the crosswalk, or the reporting area outlived (or predates)
every polity mapped to it.

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
