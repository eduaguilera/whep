# Report which authority a row's territorial identity rests on

[polity_area_crosswalk](https://eduaguilera.github.io/whep/reference/polity_area_crosswalk.md)
is not the upstream FAOSTAT-to-polity map. It is that map plus rows this
package manufactures, and its `mapping_source` column says which.
Measured on the shipped snapshot, of 649 crosswalk rows:

- `"upstream_map"` (245 rows): a row of `faostat_area_polity_map.csv` in
  `eduaguilera/whep-polities`. Upstream's statement about the territory.

- `"fabio_row_promoted"` (52): the same file, for the 47 areas the FABIO
  Rest-of-World fold used to shadow. Equally upstream's statement, and
  kept separate only because `.unfold_rest_of_world()` chooses between
  it and the fold row (whep#717). The two together consume the map's 297
  rows exactly once.

- `"prefix_outside_map"` (263) and `"prefix_fallback"` (27): WHEP's own
  ISO3-prefix match, built in `data-raw/table_mappings.R`. No upstream
  authority. A prefix match can only ever produce an ISO3-family guess,
  so it cannot express the statements the pre-1961 era actually needs –
  Turkey before 1913 is the Ottoman Empire, Pakistan before 1947 is
  British India – whose target stem differs from the source's
  (whep#740).

- `"fabio_row_fold"` (62): WHEP's own Rest-of-World bucket. Legitimately
  WHEP's to decide, but the territory still has no upstream row of its
  own. In the default mode only the 31 members upstream names nowhere
  resolve through one; see
  [`row_promotion_status()`](https://eduaguilera.github.io/whep/reference/row_promotion_status.md).

Counting crosswalk rows overstates the exposure, because most
manufactured rows are never picked. This reports the provenance of the
**resolution**: for each `(area_code, year)`, the class of the crosswalk
row that
[`add_polity_code()`](https://eduaguilera.github.io/whep/reference/add_polity_code.md)
actually selected, which is what a published value rests on. Measured
over the crosswalk's own 1850-2025 grid, 259 of the 263
`"prefix_outside_map"` rows are the resolution of no `(area_code, year)`
at all: the back-cast anchor floors every lookup at `backcast_anchor`,
so the pre-1961 era resolves through whatever answers the anchor year
rather than through the historical periods the prefix rule invented for
it.

The same measurement is what whep#717 moved: 10,912 of those
`(area_code, year)` resolutions used to rest on `"fabio_row_fold"`, and
5,456 of them – every year of the 31 members upstream names – now rest
on the upstream map instead.

## Usage

``` r
polity_mapping_provenance(
  table = NULL,
  code_column = "area_code",
  year_column = "year",
  backcast_anchor = 1961L
)
```

## Arguments

- table:

  A data frame carrying an area-code column, and a year column if the
  resolution is to be year-aware. `NULL` (the default) reports over the
  crosswalk's own `(area_code, year)` grid instead, one row per pair,
  which is the provenance of the mapping rather than of a build.

- code_column:

  Name of the column holding numeric area codes.

- year_column:

  Name of the column holding years. Set to `NULL`, or leave it absent
  from `table`, to use the current/default mapping.

- backcast_anchor:

  First year of reported (non-back-cast) FAOSTAT data; passed to the
  same resolution
  [`add_polity_code()`](https://eduaguilera.github.io/whep/reference/add_polity_code.md)
  documents. Set to `-Inf` to resolve strictly by data year, which is
  what makes the manufactured pre-1961 periods live.

## Value

A tibble with one row per resolved `(area_code, year)`, ordered by area
code and year, carrying `area_code`, `year`, `polity_code`,
`mapping_source`, `authority`, `mapping_status` and `n_rows`, the number
of rows of `table` that pair carries (always 1 when `table` is `NULL`).

## Authority

`authority` collapses `mapping_source` to the question "who said so":

- `"upstream"`: `"upstream_map"` or `"fabio_row_promoted"`.

- `"whep_prefix"`: `"prefix_outside_map"` or `"prefix_fallback"` – a
  WHEP guess, and the population whep#740 asks to delete rather than
  replace.

- `"whep_bucket"`: `"fabio_row_fold"` – WHEP's own documented bucket.

- `"unresolved"`: the area resolves to no polity, so nothing was said.

An unrecognised `mapping_source` aborts rather than being folded into
one of these, so a new class of manufactured row cannot arrive already
classified.

## See also

[`add_polity_code()`](https://eduaguilera.github.io/whep/reference/add_polity_code.md)
for the resolution itself,
[`polity_coverage_gaps()`](https://eduaguilera.github.io/whep/reference/polity_coverage_gaps.md)
for the rows whose polity is a nearest-period stand-in, and
[`polity_bucket_coverage()`](https://eduaguilera.github.io/whep/reference/polity_bucket_coverage.md)
for the buckets that sum more than one territory.

## Examples

``` r
# Area 238 Ethiopia is the one reporting area whose published identity rests
# on a manufactured row: its pre-1993 years resolve to `ETH-1952-1993`, a
# period no upstream map row declares.
polity_mapping_provenance(
  tibble::tibble(area_code = 238L, year = c(1900L, 2000L), value = 1)
)
#> # A tibble: 2 × 7
#>   area_code  year polity_code   mapping_source   authority mapping_status n_rows
#>       <int> <int> <chr>         <chr>            <chr>     <chr>           <int>
#> 1       238  1900 ETH-1952-1993 prefix_outside_… whep_pre… backcast_anch…      1
#> 2       238  2000 ETH-1993-2025 upstream_map     upstream  matched             1

# The headline is one summarise away.
polity_mapping_provenance(
  tibble::tibble(area_code = c(11L, 238L), year = 1990L)
) |>
  dplyr::summarise(n_rows = sum(n_rows), .by = authority)
#> # A tibble: 2 × 2
#>   authority   n_rows
#>   <chr>        <int>
#> 1 upstream         1
#> 2 whep_prefix      1
```
